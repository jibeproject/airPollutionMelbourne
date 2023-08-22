# file to assess distance from monitoring site locations to roads

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

network.links <- st_read("../data/processed/network.sqlite", layer = "links") %>%
  # remove PT links
  filter(highway != "pt")  ## CHECK THAT THIS IS CORRECT NETWORK FILE

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

# combined locations
locations <- full_join(PM.25.locations, NO2.locations,
                       by = c("site", "lat", "long")) %>%
  
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
  

# 3 Write output ----
# -----------------------------------------------------------------------------#
write.csv(locations.distances, "../data/processed/monitoring site location distances to roads.csv")


