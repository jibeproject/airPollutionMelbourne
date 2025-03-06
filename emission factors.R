# script to calculate emission factors for Melbourne from COPERT Australia data

library(tidyverse)
library(readxl)
options(scipen = 1000)


# 1 Read in required data ----
# -----------------------------------------------------------------------------#

## 1.1 COPERT Australia data ----
## ------------------------------------#

# inputs and emissions as calculated by COPERT Australia, for Victorian fleet 2010
# inputs: vehicles, mileage and driving share as between U, R and H
vehicles <- readxl::read_xls("../data/processed/COPERT_outputs_vic.xls",
                             sheet = "Population") %>%
  rename(vehicles = `2010`) %>% mutate(vehicles = as.numeric(vehicles))

mileage <- readxl::read_xls("../data/processed/COPERT_outputs_vic.xls",
                            sheet = "Mileage_km_per_year") %>%
  rename(km_per_year = `2010`) %>% mutate(km_per_year = as.numeric(km_per_year))

# percentage share of driving for each mode, converted to decimal
U_share <- readxl::read_xls("../data/processed/COPERT_outputs_vic.xls",
                            sheet = "U_Share_perc") %>%
  rename(U_share = `2010`) %>% mutate(U_share = as.numeric(U_share) / 100)

R_share <- readxl::read_xls("../data/processed/COPERT_outputs_vic.xls",
                            sheet = "R_Share_perc") %>%
  rename(R_share = `2010`) %>% mutate(R_share = as.numeric(R_share) / 100)

H_share <- readxl::read_xls("../data/processed/COPERT_outputs_vic.xls",
                            sheet = "H_Share_perc") %>%
  rename(H_share = `2010`) %>% mutate(H_share = as.numeric(H_share) / 100)

# speeds for each mode
U_speed <- readxl::read_xls("../data/processed/COPERT_outputs_vic.xls",
                            sheet = "U_Speed_km_per_h") %>%
  rename(U_speed = `2010`) %>% mutate(U_speed = as.numeric(U_speed))

R_speed <- readxl::read_xls("../data/processed/COPERT_outputs_vic.xls",
                            sheet = "R_Speed_km_per_h") %>%
  rename(R_speed = `2010`) %>% mutate(R_speed = as.numeric(R_speed))

H_speed <- readxl::read_xls("../data/processed/COPERT_outputs_vic.xls",
                            sheet = "H_Speed_km_per_h") %>%
  rename(H_speed = `2010`) %>% mutate(H_speed = as.numeric(H_speed))


# outputs: NO2, PM2.5 and PM (exhaust) emissions
NO2_U <- readxl::read_xls("../data/processed/COPERT_outputs_vic.xls",
                          sheet = "U_NO2_Emiss_t") %>%
  rename(NO2_U_total = `2010`)

NO2_R <- readxl::read_xls("../data/processed/COPERT_outputs_vic.xls",
                          sheet = "R_NO2_Emiss_t") %>%
  rename(NO2_R_hot = `2010`)

NO2_H <- readxl::read_xls("../data/processed/COPERT_outputs_vic.xls",
                          sheet = "H_NO2_Emiss_t") %>%
  rename(NO2_H_hot = `2010`)

PM2_5_U <- readxl::read_xls("../data/processed/COPERT_outputs_vic.xls",
                            sheet = "U_PM2.5_Emiss_t") %>%
  rename(PM2.5_U_hot = `2010`)

PM2_5_R <- readxl::read_xls("../data/processed/COPERT_outputs_vic.xls",
                            sheet = "R_PM2.5_Emiss_t") %>%
  rename(PM2.5_R_hot = `2010`)

PM2_5_H <- readxl::read_xls("../data/processed/COPERT_outputs_vic.xls",
                            sheet = "H_PM2.5_Emiss_t") %>%
  rename(PM2.5_H_hot = `2010`)

PM_exhaust_U <- readxl::read_xls("../data/processed/COPERT_outputs_vic.xls",
                                 sheet = "U_PM__exhaust__Emiss_t") %>%
  rename(PM.exhaust_U_total = `2010`)

PM_exhaust_R <- readxl::read_xls("../data/processed/COPERT_outputs_vic.xls",
                                 sheet = "R_PM__exhaust__Emiss_t") %>%
  rename(PM.exhaust_R_hot = `2010`)

PM_exhaust_H <- readxl::read_xls("../data/processed/COPERT_outputs_vic.xls",
                                 sheet = "H_PM__exhaust__Emiss_t") %>%
  rename(PM.exhaust_H_hot = `2010`)


# supplementary details, not exported into the main output workbook, but
# extracted from the same run of the COPERT Australia tool
# cold NO2 and PM_exhaust emissions (U only)
U_cold <- readxl::read_xlsx("../data/processed/COPERT_outputs_vic_supp.xlsx",
                            sheet = "cold") %>%
  rename(PM.exhaust_U_cold = PM_exhaust_U_cold)

# average length of trip (km)
Ltrip <- readxl::read_xlsx("../data/processed/COPERT_outputs_vic_supp.xlsx",
                           sheet = "region_param") %>%
  filter(parameter == "Ltrip (km)") %>% pull(value)


## 1.2 PIARC data ----
## ------------------------------------#

# future factor and gradient tables from PIARC 
pass_future <- readxl::read_xlsx("../data/original/PIARC_tables_aust.xlsx",
                                sheet = "pass_future", skip = 3) %>%
  setNames(c("Year", "CO_petrol", "CO_diesel", "NOx_petrol", "NOx_diesel", "Opacity_diesel"))

ldv_future <- readxl::read_xlsx("../data/original/PIARC_tables_aust.xlsx",
                                sheet = "ldv_future", skip = 1) %>%
  slice(-1) %>%
  setNames(c("Year", "CO", "NOx", "Opacity"))

hgv_future <- readxl::read_xlsx("../data/original/PIARC_tables_aust.xlsx",
                                sheet = "hgv_future", skip = 2)

gradient_names <- c("speed", "gradient_-6", "gradient_-4", "gradient_-2", "gradient_0",
                    "gradient_+2", "gradient_+4", "gradient_+6")

pass_NOx_petrol <- readxl::read_xlsx("../data/original/PIARC_tables_aust.xlsx",
                                     sheet = "pass_NOx_petrol", skip = 3) %>%
  setNames(gradient_names)

pass_NOx_diesel <- readxl::read_xlsx("../data/original/PIARC_tables_aust.xlsx",
                                     sheet = "pass_NOx_diesel", skip = 3) %>%
  setNames(gradient_names)

pass_PM_diesel <- readxl::read_xlsx("../data/original/PIARC_tables_aust.xlsx",
                                     sheet = "pass_PM_diesel", skip = 4) %>%
  setNames(gradient_names)

ldv_NOx <- readxl::read_xlsx("../data/original/PIARC_tables_aust.xlsx",
                             sheet = "ldv_NOx", skip = 4) %>%
  setNames(gradient_names)

ldv_PM <- readxl::read_xlsx("../data/original/PIARC_tables_aust.xlsx",
                            sheet = "ldv_PM", skip = 4) %>%
  setNames(gradient_names)

hgv_NOx <- readxl::read_xlsx("../data/original/PIARC_tables_aust.xlsx",
                                     sheet = "hgv_NOx", skip = 4) %>%
  setNames(gradient_names)

hgv_PM <- readxl::read_xlsx("../data/original/PIARC_tables_aust.xlsx",
                             sheet = "hgv_PM", skip = 4) %>%
  setNames(gradient_names)


## 1.3 Manchester (HBEFA) data ----
## ------------------------------------#
# emissions factors from Manchester (HBEFA) are used to spread the COPERT factors
# (which are based on an average speed model) into multiple factors for different
# traffic situations (hot emissions) or 'AmbientCondPatterns' (cold start emissions)

manch_hot <- read_xlsx("./Manchester emission examples/EFA_HOT_Vehcat_healthModelMCR.XLSX")
manch_cold <- read_xlsx("./Manchester emission examples/EFA_ColdStart_Vehcat_healthModelMCR.XLSX")


# 2 Calculate hot and cold emissions factors ----
# -----------------------------------------------------------------------------#

## 2.1 Emissions for 2010 ----
## ------------------------------------#
# calculate emissions for 2010, using the COPERT Australia data
emissions.2010.long <- vehicles %>%
  
  # assemble table of vehicles, milage and emissions
  left_join(mileage, by = c("Sector", "Subsector", "Technology")) %>%
  left_join(NO2_U, by = c("Sector", "Subsector", "Technology")) %>%
  left_join(NO2_R, by = c("Sector", "Subsector", "Technology")) %>%
  left_join(NO2_H, by = c("Sector", "Subsector", "Technology")) %>%
  left_join(PM2_5_U, by = c("Sector", "Subsector", "Technology")) %>%
  left_join(PM2_5_R, by = c("Sector", "Subsector", "Technology")) %>%
  left_join(PM2_5_H, by = c("Sector", "Subsector", "Technology")) %>%
  left_join(PM_exhaust_U, by = c("Sector", "Subsector", "Technology")) %>%
  left_join(PM_exhaust_R, by = c("Sector", "Subsector", "Technology")) %>%
  left_join(PM_exhaust_H, by = c("Sector", "Subsector", "Technology")) %>%
  left_join(U_cold, by = c("Sector", "Subsector", "Technology")) %>%
  left_join(U_share, by = c("Sector", "Subsector", "Technology")) %>%
  left_join(R_share, by = c("Sector", "Subsector", "Technology")) %>%
  left_join(H_share, by = c("Sector", "Subsector", "Technology")) %>%
  
  # calculate total distance, and distance for each driving share
  mutate(distance = vehicles * km_per_year,
         U_dist = distance * U_share,
         R_dist = distance * R_share,
         H_dist = distance * H_share) %>%
  
  # calculate hot emissions where separate cold emissions are provided (only
  # applies to NO2 and PM_exhaust for Urban)
  mutate(NO2_U_hot = NO2_U_total - NO2_U_cold,
         PM.exhaust_U_hot = PM.exhaust_U_total - PM.exhaust_U_cold) %>%
  
  # subtract PM.exhaust from PM2.5 to get non-exhaust emissions, and 
  # re-name PM.exhaust as PM2.5 (so now 'PM2.5' is the PM2.5 exhaust emissions,
  # and 'PM2.5.non-exhaust' is the PM 2.5 non-exhaust emissions)
  mutate(PM2.5.non.exhaust_U_hot = PM2.5_U_hot - PM.exhaust_U_hot,
         PM2.5.non.exhaust_R_hot = PM2.5_R_hot - PM.exhaust_R_hot,
         PM2.5.non.exhaust_H_hot = PM2.5_H_hot - PM.exhaust_H_hot,
         PM2.5_U_hot = PM.exhaust_U_hot,
         PM2.5_R_hot = PM.exhaust_R_hot,
         PM2.5_H_hot = PM.exhaust_H_hot) %>%
  rename(PM2.5_U_cold = PM.exhaust_U_cold) %>%
  
  # select required fields
  dplyr::select(Sector, Subsector, Technology, vehicles, km_per_year,
                NO2_U_hot, NO2_R_hot, NO2_H_hot,
                PM2.5_U_hot, PM2.5_R_hot, PM2.5_H_hot,
                PM2.5.non.exhaust_U_hot, PM2.5.non.exhaust_R_hot, PM2.5.non.exhaust_H_hot,
                NO2_U_cold, PM2.5_U_cold,
                U_share, R_share, H_share,
                distance, U_dist, R_dist, H_dist) %>%
  
  # add 'VehCat' field (grouping cars with SUVs), and omit mopeds/motorcycles
  mutate(VehCat = case_when(
    Sector %in% c("Passenger Cars", "SUV") ~ "pass. car",
    Sector == "Light Commercial Vehicles"  ~ "LCV",
    Sector == "Heavy Duty Trucks"          ~ "HGV",
    Sector == "Buses"                      ~ "urban bus",
    TRUE                                   ~ "other"
  )) %>%
  filter(VehCat != "other")

# calculate summary table
emissions.2010 <- emissions.2010.long %>%
  
  # summarise by vehicle type
  group_by(VehCat) %>%
  summarise(U_dist = sum(U_dist),
            R_dist = sum(R_dist),
            H_dist = sum(H_dist),
            NO2_U_hot = sum(NO2_U_hot),
            NO2_R_hot = sum(NO2_R_hot),
            NO2_H_hot = sum(NO2_H_hot),
            PM2.5_U_hot = sum(PM2.5_U_hot),
            PM2.5_R_hot = sum(PM2.5_R_hot),
            PM2.5_H_hot = sum(PM2.5_H_hot),
            PM2.5.non.exhaust_U_hot = sum(PM2.5.non.exhaust_U_hot),
            PM2.5.non.exhaust_R_hot = sum(PM2.5.non.exhaust_R_hot),
            PM2.5.non.exhaust_H_hot = sum(PM2.5.non.exhaust_H_hot),
            NO2_U_cold = sum(NO2_U_cold),
            PM2.5_U_cold = sum(PM2.5_U_cold)) %>%
  ungroup() %>%
  
  # convert hot emissions to g/km by multiplying tonnes by 1m to convert to g,
  # then dividing by distance driven
  mutate(NO2_U_hot = NO2_U_hot * 1000000 / U_dist,
         NO2_R_hot = NO2_R_hot * 1000000 / R_dist,
         NO2_H_hot = NO2_H_hot * 1000000 / H_dist,
         PM2.5_U_hot = PM2.5_U_hot * 1000000 / U_dist,
         PM2.5_R_hot = PM2.5_R_hot * 1000000 / R_dist,
         PM2.5_H_hot = PM2.5_H_hot * 1000000 / H_dist,
         PM2.5.non.exhaust_U_hot = PM2.5.non.exhaust_U_hot * 1000000 / U_dist,
         PM2.5.non.exhaust_R_hot = PM2.5.non.exhaust_R_hot * 1000000 / R_dist,
         PM2.5.non.exhaust_H_hot = PM2.5.non.exhaust_H_hot * 1000000 / H_dist) %>%
  
  # convert cold emissions to g/start by multiplying tonnes by 1m to convert to
  # g, then dividing by number of starts (that is, distance divided by Ltrip) - 
  # note that all cold emissions are attributed to Urban starts (see 
  # Help > Topics > Calculation Factors > Cold Emission Factors in the COPERT 
  # Australia tool: it's assumed that the majority of vehicles start their trips 
  # from urban areas)
  mutate(NO2_U_cold = NO2_U_cold * 1000000 / (U_dist / Ltrip),
         PM2.5_U_cold = PM2.5_U_cold * 1000000 / (U_dist / Ltrip)) %>%
  
  # omit the distance fields
  dplyr::select(-(contains("dist")))
         

## 2.2 Emissions for 2020 ----
## ------------------------------------#

# convert 2010 emissions to 2020 by using the PIARC future factors

# note the PIARC NOx factors are used for NO2, and the PIARC Opacity factors for PM

# extract the required conversion factors from the PIARC data
ft_pass_NOx_petrol <- pass_future %>% 
  filter(Year == 2020) %>% pull(NOx_petrol) %>% as.numeric()
ft_pass_NOx_diesel <- pass_future %>% 
  filter(Year == 2020) %>% pull(NOx_diesel) %>% as.numeric()
ft_pass_PM <- pass_future %>%
  filter(Year == 2020) %>% pull(Opacity_diesel) %>% as.numeric()
ft_lcv_NOx <- ldv_future %>%
  filter(Year == 2020) %>% pull(NOx) %>% as.numeric()
ft_lcv_PM <- ldv_future %>%
  filter(Year == 2020) %>% pull(Opacity) %>% as.numeric()
ft_heavy_NOx <- hgv_future %>%
  filter(Year == 2020) %>% pull(NOx) %>% as.numeric()
ft_heavy_PM <- hgv_future %>%
  filter(Year == 2020) %>% pull(Opacity) %>% as.numeric()

# calculate the combined passenger car NOx factor from the petrol and
# diesel figures by reference to the distance travelled by each of the petrol
# and diesel cars (note - both passenger cars and SUV)

# review different fuel types to consider how LPG should be treated
# pass_review <- emissions.2010.long %>%
#   filter(Sector == "Passenger Cars" | Sector == "SUV") %>%
#   mutate(fuel = case_when(
#     str_detect(Subsector, "petrol") ~ "petrol",
#     str_detect(Subsector, "diesel") ~ "diesel",
#     str_detect(Subsector, "E10") ~ "E10",
#     str_detect(Subsector, "LPG") ~ "LPG"
#   )) %>%
#   group_by(fuel) %>%
#   summarise(U_dist = sum(U_dist),
#             NO2_U_hot = sum(NO2_U_hot),
#             PM2.5_U_hot = sum(PM2.5_U_hot),
#             PM2.5.non.exhaust_U_hot = sum(PM2.5.non.exhaust_U_hot)) %>%
#   ungroup() %>%
#   mutate(NO2_U_hot = NO2_U_hot * 1000000 / U_dist,
#          PM2.5_U_hot = PM2.5_U_hot * 1000000 / U_dist,
#          PM2.5.non.exhaust_U_hot = PM2.5.non.exhaust_U_hot * 1000000 / U_dist)
# # Reviewed result - LPG and E10 are both closer to petrol than to diesel;
# # so combine them and treat as equivalent to petrol

# find the percentage of passenger car (incl. SUV) trip distance done with diesel fuel 
pass_diesel <- emissions.2010.long %>%
  # classify cars as 'diesel' if diesel used, or petrol if any of petrol, E10 or LPG
  filter(Sector == "Passenger Cars" | Sector == "SUV") %>%
  mutate(fuel = ifelse(str_detect(Subsector, "diesel"), "diesel", "petrol")) %>%
  # calculate the milage for each fuel type
  group_by(fuel) %>%
  summarise(distance = sum(distance)) %>%
  ungroup() %>%
  # calculate the proportionate milage of each, and select diesel
  mutate(propn = distance / sum(distance)) %>%
  filter(fuel == "diesel") %>%
  pull(propn)
  
# calculate the future factor for passenger NOx, based on the petrol and diesel figures
ft_pass_NOx <- (ft_pass_NOx_diesel * pass_diesel) + (ft_pass_NOx_petrol * (1 - pass_diesel))

# calculate the 2020 emissions
emissions.2020 <- emissions.2010 %>%
  
  # convert to long-form table 
    pivot_longer(cols = contains("_"),
                 names_to = "Component_RoadCat_temp", 
                 values_to = "value") %>%
    separate(col = Component_RoadCat_temp, 
             into = c("Component", "RoadCat", "temp"), 
             sep = "_") %>%
  
  # format name for PM2.5 non-exhaust emissions
  mutate(Component = ifelse(Component == "PM2.5.non.exhaust", 
                            "PM2.5 (non-exhaust)",
                            Component)) %>%
  
  # complete full names for RoadCat column
  mutate(RoadCat = case_when(
    RoadCat == "U" ~ "Urban",
    RoadCat == "R" ~ "Rural",
    RoadCat == "H" ~ "Highway"
  )) %>%
  
  # multiply by future factors
  mutate(value = case_when(
    VehCat == "pass. car" ~ case_when(
      Component == "NO2"                               ~ value * ft_pass_NOx,
      Component %in% c("PM2.5", "PM2.5 (non-exhaust)") ~ value * ft_pass_PM
    ),
    VehCat == "LCV" ~ case_when(
      Component == "NO2"                               ~ value * ft_lcv_NOx,
      Component %in% c("PM2.5", "PM2.5 (non-exhaust)") ~ value * ft_lcv_PM
    ),
    VehCat %in% c("HGV", "urban bus") ~ case_when(
      Component == "NO2"                               ~ value * ft_heavy_NOx,
      Component %in% c("PM2.5", "PM2.5 (non-exhaust)") ~ value * ft_heavy_PM
      
    )
  ))

# make separate tables for cold (now final) and hot (zero gradient, requires 
# further processing to add other gradients in section 3)

emissions.cold <- emissions.2020 %>%
  
  filter(temp == "cold") %>%
  dplyr::select(-temp) %>%
  
  # finalise other columns
  mutate(Year = 2020, AmbientCondPattern = NA) %>%
  rename(EFA_weighted = value) %>%
  
  # select required columns
  dplyr::select(VehCat, Year, Component, RoadCat, AmbientCondPattern, EFA_weighted)

emissions.hot.zero <- emissions.2020 %>%
  filter(temp == "hot") %>%
  dplyr::select(-temp)


# 3 Add gradients for hot emissions ----
# -----------------------------------------------------------------------------#

# gradients are calculated from PIARC tables, which set out emissions (in g/h or
# m2/h) for NOx and Opacity, for a range of road gradients, dependent on speed.  
# The raw values of these emissions are not used - rather, factors are calculated,
# with  the value of the relevant emission for the relevant speed and zero
# gradient set at 1, and the factors for other gradients calculated as multiples
# based on the relationships between the raw values of the emissions for the gradients.

# in the Manchester (HBEFA) outputs, there are no gradient differences for 
# PM2.5 non-exhaust emissions; so consistently, no gradient changes are applied
# here for PM2.5.non.exhaust either

# extract assumed speeds for class of vehicle
# (actually the speed is the same for all vehicles of each class, but this code
# would find a weighted average if it were not)
speeds <- emissions.2010.long %>%
  left_join(U_speed, by = c("Sector", "Subsector", "Technology")) %>%
  left_join(R_speed, by = c("Sector", "Subsector", "Technology")) %>%
  left_join(H_speed, by = c("Sector", "Subsector", "Technology")) %>%
  # mean of the speed for the vehicle class, weighted by distance
  group_by(VehCat) %>%
  summarise(Urban = weighted.mean(U_speed, w = U_dist),
            Rural = weighted.mean(R_speed, w = R_dist),
            Highway = weighted.mean(H_speed, w = H_dist)) %>%
  ungroup()


# function to calculate gradient factors for each category, speed and emission - 
# if speed is not a multiple of 10 (to match the PIARC table), then the rows above and
# below are selected (eg 35 selects rows 30 and 40); then the rows are summed,
# and the results converted to factors with gradient_0 set at 1
speed_gradients <- function(veh_cat, component, road_cat, PIARC_table) {
  
  # speed for the vehicle class and road category
  case_speed <- speeds %>% filter(VehCat == veh_cat) %>% pull(get(road_cat))
  
  # extract the relevant row(s) from the PIARC table (if speed is a multiple of
  # 10, one row; if not, the next row above and below)
  gradient_row <- PIARC_table %>%
    filter(speed %in% c(floor(case_speed / 10) * 10, 
                        ceiling(case_speed / 10) * 10)) %>%
    
    # remove 'speed' column
    dplyr::select(-speed) %>%
    
    # sum the rows
    summarise(across(everything(), sum)) %>%
    
    # convert to factors by dividing by gradient_0 (so gradient_0 will be 1)
    mutate(across(everything(), ~. / gradient_0)) %>%
    
    # add columns for joining
    mutate(VehCat = veh_cat, Component = component, RoadCat = road_cat)
  
  return(gradient_row)
}

# function to return a row with factors of 1 for PM2.5 (non-exhaust), where 
# there is no gradient effect
no_gradients <- function(veh_cat, component, road_cat, gradient_names) {
  
  # create one-row dataframe with 1s in each column
  no_gradient_row <- as_tibble(setNames(as.list(rep(1, length(gradient_names))), gradient_names)) %>%
    
    # remove 'speed' column
    dplyr::select(-speed) %>%
    
    # add columns for joining
    mutate(VehCat = veh_cat, Component = component, RoadCat = road_cat)
  
    return(no_gradient_row)
}

# build the passenger NO2 gradient factors, which are calculated from both petrol and diesel,
# weighted according to proportions of car travel in petrol or diesel cars 
# (that is, using pass_diesel, calculated in section 2.2)
pass_NO2_diesel_grad_U <- speed_gradients("pass. car", "NO2", "Urban", pass_NOx_diesel) %>%
  mutate(across(starts_with("gradient"), ~ . * pass_diesel))
pass_NO2_petrol_grad_U <- speed_gradients("pass. car", "NO2", "Urban", pass_NOx_petrol) %>%
  mutate(across(starts_with("gradient"), ~ . * (1 - pass_diesel)))
pass_NO2_grad_U <- bind_rows(pass_NO2_petrol_grad_U, pass_NO2_diesel_grad_U) %>%
  summarise(across(starts_with("gradient"), sum)) %>%
  mutate(VehCat = "pass. car", Component = "NO2", RoadCat = "Urban")

pass_NO2_diesel_grad_R <- speed_gradients("pass. car", "NO2", "Rural", pass_NOx_diesel) %>%
  mutate(across(starts_with("gradient"), ~ . * pass_diesel))
pass_NO2_petrol_grad_R <- speed_gradients("pass. car", "NO2", "Rural", pass_NOx_petrol) %>%
  mutate(across(starts_with("gradient"), ~ . * (1 - pass_diesel)))
pass_NO2_grad_R <- bind_rows(pass_NO2_petrol_grad_R, pass_NO2_diesel_grad_R) %>%
  summarise(across(starts_with("gradient"), sum)) %>%
  mutate(VehCat = "pass. car", Component = "NO2", RoadCat = "Rural")

pass_NO2_diesel_grad_H <- speed_gradients("pass. car", "NO2", "Highway", pass_NOx_diesel) %>%
  mutate(across(starts_with("gradient"), ~ . * pass_diesel))
pass_NO2_petrol_grad_H <- speed_gradients("pass. car", "NO2", "Highway", pass_NOx_petrol) %>%
  mutate(across(starts_with("gradient"), ~ . * (1 - pass_diesel)))
pass_NO2_grad_H <- bind_rows(pass_NO2_petrol_grad_H, pass_NO2_diesel_grad_H) %>%
  summarise(across(starts_with("gradient"), sum)) %>%
  mutate(VehCat = "pass. car", Component = "NO2", RoadCat = "Highway")

# combine the passenger NO2 gradient factors with the other factors (which
# are each built from a single PIARC table)
gradient.factors <- 
  bind_rows(pass_NO2_grad_U,
            pass_NO2_grad_R,
            pass_NO2_grad_H,
            speed_gradients("pass. car", "PM2.5", "Urban", pass_PM_diesel),
            speed_gradients("pass. car", "PM2.5", "Rural", pass_PM_diesel),
            speed_gradients("pass. car", "PM2.5", "Highway", pass_PM_diesel),
            no_gradients("pass. car", "PM2.5 (non-exhaust)", "Urban", gradient_names),
            no_gradients("pass. car", "PM2.5 (non-exhaust)", "Rural", gradient_names),
            no_gradients("pass. car", "PM2.5 (non-exhaust)", "Highway", gradient_names),
            speed_gradients("LCV", "NO2", "Urban", ldv_NOx),
            speed_gradients("LCV", "NO2", "Rural", ldv_NOx),
            speed_gradients("LCV", "NO2", "Highway", ldv_NOx),
            speed_gradients("LCV", "PM2.5", "Urban", ldv_PM),
            speed_gradients("LCV", "PM2.5", "Rural", ldv_PM),
            speed_gradients("LCV", "PM2.5", "Highway", ldv_PM),
            no_gradients("LCV", "PM2.5 (non-exhaust)", "Urban", gradient_names),
            no_gradients("LCV", "PM2.5 (non-exhaust)", "Rural", gradient_names),
            no_gradients("LCV", "PM2.5 (non-exhaust)", "Highway", gradient_names),
            speed_gradients("HGV", "NO2", "Urban", hgv_NOx),
            speed_gradients("HGV", "NO2", "Rural", hgv_NOx),
            speed_gradients("HGV", "NO2", "Highway", hgv_NOx),
            speed_gradients("HGV", "PM2.5", "Urban", hgv_PM),
            speed_gradients("HGV", "PM2.5", "Rural", hgv_PM),
            speed_gradients("HGV", "PM2.5", "Highway", hgv_PM),
            no_gradients("HGV", "PM2.5 (non-exhaust)", "Urban", gradient_names),
            no_gradients("HGV", "PM2.5 (non-exhaust)", "Rural", gradient_names),
            no_gradients("HGV", "PM2.5 (non-exhaust)", "Highway", gradient_names),
            speed_gradients("urban bus", "NO2", "Urban", hgv_NOx),
            speed_gradients("urban bus", "NO2", "Rural", hgv_NOx),
            speed_gradients("urban bus", "NO2", "Highway", hgv_NOx),
            speed_gradients("urban bus", "PM2.5", "Urban", hgv_PM),
            speed_gradients("urban bus", "PM2.5", "Rural", hgv_PM),
            speed_gradients("urban bus", "PM2.5", "Highway", hgv_PM),
            no_gradients("urban bus", "PM2.5 (non-exhaust)", "Urban", gradient_names),
            no_gradients("urban bus", "PM2.5 (non-exhaust)", "Rural", gradient_names),
            no_gradients("urban bus", "PM2.5 (non-exhaust)", "Highway", gradient_names),
  )

# add gradients to the hot emissions table, and finalise
emissions.hot <- emissions.hot.zero %>%
  left_join(gradient.factors, by = c("VehCat", "Component", "RoadCat")) %>%
  
  # multiply value by gradient factors
  mutate(across(starts_with("gradient"), ~ . * value)) %>%
  
  # remove the value column (which is now replicated in 'gradient_0', as 
  # 'gradient_0' always had the value of 1)
  dplyr::select(-value) %>%
  
  # pivot to long form
  pivot_longer(cols = contains("gradient"),
               names_to = "Gradient", 
               values_to = "EFA_weighted") %>%
  
  # remove word 'gradient' from Gradient column and add "%"
  mutate(Gradient = paste0(str_replace(Gradient, "gradient_", ""), "%")) %>%
  
  # add speed ('V_weighted' column) for the relevant row
  left_join(speeds %>%
              # pivot longer, with Urban/Rural/Highway columns as 'RoadCat'
              pivot_longer(cols = c("Urban", "Rural", "Highway"), 
                           names_to = "RoadCat", values_to = "V_weighted"),
            by = c("VehCat", "RoadCat")) %>%
  
  # add year
  mutate(Year = 2020) %>%
  
  # organise columns [FOR NOW, has RoadCat instead of TrafficSit]
  dplyr::select(VehCat, Year, Component, RoadCat, Gradient, V_weighted, EFA_weighted)


# 4 Write 'unspread' outputs ----
# -----------------------------------------------------------------------------#

# these outputs (before being spread into different traffic situations and
# 'AmbientCondPatterns', are used for comparision to the Manchester values)

write.table(emissions.hot, "../data/processed/EFA_hot_melbourne_unspread.txt", sep = ";", row.names = F)
write.table(emissions.cold, "../data/processed/EFA_coldstart_melbourne_unspread.txt", sep = ";", row.names = F)


# 5 Spread emissions for traffic situations / conditions ----
# -----------------------------------------------------------------------------#

## 5.1 Hot emissions ----
## ------------------------------------#

# categorise Manchester emissions as Urban, Rural or Highway
manch_hot_categorised <- manch_hot %>%
  
  mutate(RoadCat = case_when(
    str_detect(TrafficSit, "^URB/MW-Nat./") ~ "Highway",
    str_detect(TrafficSit, "^RUR/MW/")      ~ "Highway",
    str_detect(TrafficSit, "^RUR/Semi-MW/") ~ "Highway",
    str_detect(TrafficSit, "^URB/") ~ "Urban",
    str_detect(TrafficSit, "^RUR/") ~ "Rural"
  ))

# dataframe to hold spread emission outputs
emissions.hot.factors <- c()

# calculate traffic and speed factors from Manchester data: emissions and speed,
# divided by emissions and speed for a 'reference' category that has a speed
# closest to the average speed used for the Melbourne data
for (veh_cat in emissions.hot$VehCat %>% unique()) {
  for (road_cat in emissions.hot$RoadCat %>% unique()) {
    for (component in emissions.hot$Component %>% unique()) {
      
      # select the relevant Manchester readings
      manch_selection <- manch_hot_categorised %>%
        filter(VehCat == veh_cat & RoadCat == road_cat & Component == component)
      
      # select the Melbourne average speed for the vehicle and road category
      melb_speed = speeds %>%
        filter(VehCat == veh_cat) %>%
        pull(road_cat)
        
      # select the Manchester reference traffic situation, being the one 
      # with the V_weighted (ie speed) closest to the melb_speed
      ref_trafficsit <-  manch_selection %>%
        # select from zero gradient
        filter(Gradient == "0%") %>%
        # closest V_weighted to melb_speed
        mutate(speed_diff = abs(V_weighted - melb_speed)) %>%
        filter(speed_diff == min(speed_diff)) %>%
        # select one in case of equality
        slice_head() %>%
        # select the TrafficSit
        pull(TrafficSit)
      
      # select the full set of factors (ie all gradients) for the traffic situation
      ref_factors <- manch_selection %>%
        filter(TrafficSit == ref_trafficsit) %>%
        dplyr::select(Gradient, EFA_ref = EFA_weighted, V_ref = V_weighted)
      
      # calculate traffic and speed factors for Manchester, being the EFA_weighted
      # and V_weighted divided by the reference factor for the gradient
      manch_factors <- manch_selection %>%
        left_join(ref_factors, by = "Gradient") %>%
        mutate(EFA_factor = EFA_weighted / EFA_ref,
               V_factor = V_weighted / V_ref) %>%
        
        # keep just the scenario columns and the factors
        dplyr::select(VehCat, Year, Component, TrafficSit, Gradient, RoadCat, EFA_factor, V_factor)

      # add to the dataframe
      emissions.hot.factors <- bind_rows(emissions.hot.factors, manch_factors)
    }
  }
}

# stretch Melb hot emissions into a dataframe as long as the Manch dataframe,
# and calculate the Melb emissions and speeds by applying the factors
emissions.hot.spread <- emissions.hot.factors %>%
  
  # 'emissions.hot.factors' is the Manchester factors; now join the Melb values ('emissions.hot')
  left_join(emissions.hot, by = c("VehCat", "Year", "Component", "Gradient", "RoadCat")) %>%
  
  # calculate the Melb emissions and speed by applying the Manch factors
  mutate(EFA_weighted = EFA_weighted * EFA_factor,
         V_weighted = V_weighted * V_factor) %>%
  
  # select required output fields
  dplyr::select(VehCat, Year, Component, TrafficSit, Gradient, V_weighted, EFA_weighted)


## 5.2 Cold start emissions ----
## ------------------------------------#

# dataframe to hold spread emission outputs
emissions.cold.factors <- c()

# calculate condition factors from Manchester data: emissions,
# average emissions for the condition patterns for that vehicle type
for (veh_cat in emissions.cold$VehCat %>% unique()) {
    for (component in emissions.cold$Component %>% unique()) {
      
      # select the relevant Manchester readings (but there is no Manchester
      # HGV or urban bus, so use LCV instead - though it doesn't matter,
      # because cold emissions for HGV and urban bus are zero anyway)
      if (veh_cat %in% manch_cold$VehCat) {
        manch_selection <- manch_cold %>%
          filter(VehCat == veh_cat & Component == component)
      } else {
        manch_selection <- manch_cold %>%
          filter(VehCat == "LCV" & Component == component) %>%
          mutate(VehCat = veh_cat)
      }
      
      # select the Manchester reference value, being the mean value
      ref_value = mean(manch_selection$EFA_weighted)
      
      # calculate condition factors for Manchester, being the EFA_weighted
      # divided by the ref_value - note that where the value is negative, the
      # reciprocal value is used (where there are negative values, then all
      # values for that vehicle/pollutant combination are negative; there 
      # are no combinations where negative and positive values are mixed)
      manch_factors <- manch_selection %>%
        mutate(cond_factor = ifelse(EFA_weighted < 0,
                                    ref_value / EFA_weighted,
                                    EFA_weighted / ref_value)) %>%
        
        # keep just the scenario columns and the factors
        dplyr::select(VehCat, Year, Component, RoadCat, AmbientCondPattern, cond_factor)
        
      # add to the dataframe
      emissions.cold.factors <- bind_rows(emissions.cold.factors, manch_factors)
    }
}

# stretch Melb cold emissions into a dataframe as long as the Manch dataframe,
# and calculate the Melb emissions by applying the factors
emissions.cold.spread <- emissions.cold.factors %>%
  
  # 'emissions.hot.factors' is the Manchester factors; now join the Melb values ('emissions.cold')
  full_join(emissions.cold %>% dplyr::select(-AmbientCondPattern), 
            by = c("VehCat", "Year", "Component", "RoadCat")) %>%
  
  # calculate the Melb emissions and speed by applying the Manch factors
  mutate(EFA_weighted = EFA_weighted * cond_factor) %>%
  
  # select required output fields
  dplyr::select(VehCat, Year, Component, RoadCat, AmbientCondPattern, EFA_weighted)



# 6 Write final 'spread' outputs ----
# -----------------------------------------------------------------------------#

write.table(emissions.hot.spread, "../data/processed/EFA_hot_melbourne.txt", sep = ";", row.names = F)
write.table(emissions.cold.spread, "../data/processed/EFA_coldstart_melbourne.txt", sep = ";", row.names = F)
