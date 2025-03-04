# file to assess distance from monitoring site locations to roads,
# classify them, and find nearest network link

library(dplyr)
library(readxl)
library(sf)
options(scipen = 1000)

# 1 Read in air quality data, network links and region ----
# -----------------------------------------------------------------------------#
PM25 <- readxl::read_xlsx("../data/processed/annual_pm_no2_vic.xlsx",
                          sheet = "pm25_vic_2017-2020")

NO2 <- readxl::read_xlsx("../data/processed/annual_pm_no2_vic.xlsx",
                         sheet = "no2_vic_2017-2019") 

melb_hourly <- read_xlsx("../data/original/2018_All_sites_air_quality_hourly_avg_AIR-I-F-V-VH-O-S1-DB-M2-4-0.xlsx")

# # old version
# network.links <- st_read("../data/processed/network.sqlite", layer = "links") %>%
#   # remove PT links
#   filter(highway != "pt")

# updated version
network.links <- st_read("../data/processed/edgesMelbourne.gpkg") %>%
  # remove PT links
  filter(highway != "pt") 

region <- st_read("../data/processed/region.sqlite")
region.buffered <- st_read("../data/processed/region_buffer.sqlite")  # 10km buffer


# 2 Table of monitoring site locations ----
# -----------------------------------------------------------------------------#
# PM25 locations
PM.25.locations <- PM25 %>%
  distinct(site, lat, long = long_) %>%
  mutate(PM25 = 1)

# NO2 locations
NO2.locations <- NO2 %>%
  distinct(site, lat, long = long_) %>%
  mutate(NO2 = 1) %>%
  # round to same level as PM25
  mutate(lat = round(lat, 6), long = round(long, 6))

# Churchill (a site that doesn't appear in the annual PM2.5 or NO2 locations,
# but does have PM2.5 readings in the EPA melb hourly readings)
Churchill <- melb_hourly %>%
  dplyr::select(site = sp_name, lat = latitude, long = longitude) %>%
  filter(site == "Churchill") %>%
  distinct() %>%
  mutate(PM25 = 1) %>%
  # round to same level as PM25
  mutate(lat = round(lat, 6), long = round(long, 6))

# combined locations
locations <- full_join(PM.25.locations, NO2.locations,
                       by = c("site", "lat", "long")) %>%
  
  # add Churchill
  bind_rows(Churchill) %>%
  
  # arrange names in alphabetical order
  arrange(., site) %>%
  
  # insert 0 where no monitoring for pollutant
  mutate(PM25 = coalesce(PM25, 0), NO2 = coalesce(NO2, 0)) %>%
  
  # convert to sf in same CRS as road network
  st_as_sf(coords = c("long", "lat"), crs = st_crs(4283), remove = FALSE) %>%  # GDA94 (assumed)
  st_transform(st_crs(network.links)) %>%
  
  # determine whether within Melbourne area of interest
  mutate(region.int = as.numeric(st_intersects(., region)),
         region.buff.int = as.numeric(st_intersects(., region.buffered))) %>%
  mutate(region = case_when(
    region.int == 1      ~ "Greater Melbourne",
    region.buff.int == 1 ~ "Greater Melbourne 10km buffer",
    TRUE                 ~ "Not in Greater Melbourne"
  )) %>%
  dplyr::select(-region.int, -region.buff.int)


# subsets of links for highway types
fwy <- network.links %>% filter(highway %in% c("motorway", "motorway_link"))
trunk <- network.links %>% filter(highway %in% c("trunk", "trunk_link"))
prim <- network.links %>% filter(highway %in% c("primary", "primary_link"))
sec <- network.links %>% filter(highway %in% c("secondary", "secondary_link"))
tert <- network.links %>% filter(highway %in% c("tertiary", "tertiary_link"))

locations.distances <- locations %>%
  
  # distance and type of nearest link
  mutate(nearest.link = st_nearest_feature(., network.links), # index of nearest link
         nearest_road_m = as.numeric(st_distance(., network.links[nearest.link, ], by_element = TRUE)),
         nearest_road.type = network.links$highway[nearest.link]) %>%
  dplyr::select(-nearest.link) %>%
  
  # distance to nearest freeway 
  mutate(nearest.fwy = st_nearest_feature(., fwy),
         nearest_fwy_m = as.numeric(st_distance(., fwy[nearest.fwy, ], by_element = TRUE))) %>%
  dplyr::select(-nearest.fwy) %>%
  
  # distance to nearest trunk rd
  mutate(nearest.trunk = st_nearest_feature(., trunk),
         nearest_trunk_m = as.numeric(st_distance(., trunk[nearest.trunk, ], by_element = TRUE))) %>%
  dplyr::select(-nearest.trunk) %>%
  
  # distance to nearest primary rd 
  mutate(nearest.prim = st_nearest_feature(., prim),
         nearest_prim_m = as.numeric(st_distance(., prim[nearest.prim, ], by_element = TRUE))) %>%
  dplyr::select(-nearest.prim) %>%
  
  # distance to nearest secondary rd 
  mutate(nearest.sec = st_nearest_feature(., sec),
         nearest_sec_m = as.numeric(st_distance(., sec[nearest.sec, ], by_element = TRUE))) %>%
  dplyr::select(-nearest.sec) %>%
  
  # distance to nearest tertiary rd 
  mutate(nearest.tert = st_nearest_feature(., tert),
         nearest_tert_m = as.numeric(st_distance(., tert[nearest.tert, ], by_element = TRUE))) %>%
  dplyr::select(-nearest.tert) %>%
  
  # remove geometry and put lat and long at end
  st_drop_geometry() %>%
  relocate(long, .after = last_col()) %>%
  relocate(lat, .after = last_col())
  
# 3 Find nearest network links ---
# -----------------------------------------------------------------------------#
# indices of the nearest car links
car.links <- network.links %>%
  filter(is_car == TRUE)
nearest.indices <- st_nearest_feature(locations, car.links)

# add nearest edgeID and name
locations.with.nearest <- locations %>%
  mutate(
    nearest_car_edgeID = car.links$edgeID[nearest.indices],
    nearest_car_name = car.links$name[nearest.indices]
  ) %>%
  dplyr::select(site, nearest_car_edgeID, nearest_car_name) %>%
  st_drop_geometry()

locations.nearest <- locations.distances %>%
  left_join(locations.with.nearest, by = "site")


# 4 Classify locations ----
# -----------------------------------------------------------------------------#
# For sites in Greater Melbourne only -classify monitoring site locations as 
# 'Urban', 'Suburban' or 'Rural', and as 'Background', 'Traffic' or 'Industrial',
# based on descriptions at https://uk-air.defra.gov.uk/networks/site-types.
# Classification is done manually based on those descriptions.  

# Note that the following sites are between 10m and 15m from kerbside:
# Alphington, Brooklyn, Campbellfield, Melbourne CBD, Millers Rd and Yarraville; 
# and also Churchill (but a residential loop road with minimal traffic).  In the UK 
# descriptions, 'Traffic' requires the sampling point to be no more than 10m from
# kerbside.  So these sites could be 'Traffic' if a greater distance of 15m
# were used instead of 10m.  However, results suggest they are no higher than
# comparable background readings - so the strict 10m has been applied (and
# as a result, ther are no 'Traffic' sites).

# # Old version, Melbourne only, where up to 15m was accepted as 'Traffic'
# locations.classified <- locations.nearest %>%
#   mutate(class = case_when(
#     site %in% c("Alphington", "Brooklyn", "Campbellfield",
#                 "Millers_Rd", "Yarraville")             ~ "Suburban Traffic",
#     site %in% c("Altona_North", "Barbara_Beyer_Reserve",
#                 "Donald_Mclean_Reserve", "Footscray", "Macleod",
#                 "Melton", "Mooroolbark", "Point_Cook",
#                 "Primula_Ave", "Railway_Reserve")       ~ "Suburban Background",
#     site %in% c("Dandenong")                            ~ "Suburban Industrial",
#     site %in% c("Melbourne_CBD")                        ~ "Urban Traffic"
#   ))

locations.classified <- locations.nearest %>%
  # add class
  mutate(class = case_when(
    site %in% c("Alphington", "Altona_North", "Barbara_Beyer_Reserve",
                "Brooklyn", "Campbellfield","Donald_Mclean_Reserve", 
                "Footscray", "Macleod", "Melton", "Millers_Rd", "Mooroolbark", 
                "Point_Cook", "Primula_Ave", "Railway_Reserve", 
                "Yarraville")         ~ "Suburban Background",
    site %in% c("Dandenong")          ~ "Suburban Industrial",
    site %in% c("Melbourne_CBD")      ~ "Urban Background",
    site %in% c("Churchill", "Moe", "Morwell_East", "Morwell_South",
                "Newborough", "Rosedale", "Traralgon",
                "Wangaratta")         ~ "Suburban Background (outside Melbourne)",
    site %in% c("Geelong_South")      ~   "Suburban Industrial (outside Melbourne)"
  )) %>%
  # add group notes
  mutate(notes = case_when(
    site %in% c("Barbara_Beyer_Reserve", "Donald_Mclean_Reserve", "Millers_Rd",
                "Primula_Ave", "Railway_Reserve", "Yarraville") ~ "West Gate Tunnel Project",
    site %in% c("Churchill", "Moe", "Morwell_East", "Morwell_South",
                "Newborough", "Traralgon")                                    ~ "Latrobe Valley"
  ))


# 3 Write output ----
# -----------------------------------------------------------------------------#
write.csv(locations.classified, "../data/processed/monitoring site locations classified.csv", row.names = F)


