library(tidycensus)
library(tidyverse)
library(dplyr)
library(stringr)
library(tigris)

#run this for state map outlines
state_borders <- states(cb = T)
state_borders <- state_borders %>% 
  filter(STATEFP %in% state_list)

#Get a variable
almv_acs_var <- function(varcode){
  get_acs(geography="county",
          state=state_list,
          variables =varcode,
          year=2019, geometry = T) %>%
    filter(GEOID %in% fip_list)
}

#map a variable
almv_acs_map <- function(varcode){
  get_acs(geography="county",
          state=state_list,
          variable=varcode,
          year=2019,
          geometry = TRUE,
          survey = "acs5") %>%
    filter(GEOID %in% fip_list) %>%
    transmute(GEOID, NAME, estimate = estimate, geometry) %>% 
    ggplot() +
    geom_sf(data = state_borders, color = 'black', fill = 'grey')+ 
    geom_sf(aes(fill=estimate, color = estimate)) +
    scale_color_viridis_c() + scale_fill_viridis_c()+ 
    coord_sf(datum = NA) +
    theme_minimal()
}

#get a table of percentages
almv_acs_var_summary <- function(varcode, summaryvarcode){
  tabl <- get_acs(geography="county",
                  state=state_list,
                  variables =varcode,
                  year=2019, geometry = T, summary_var = summaryvarcode) %>%
    filter(GEOID %in% fip_list)
  tabl <- tabl %>% transmute(NAME, geometry, estimate = estimate/summary_est)
  return(tabl)
}

#make and map percentages
almv_acs_var_percent_map <- function(varcode, summaryvarcode){
  tabl <- get_acs(geography="county",
                  state=state_list,
                  variables =varcode,
                  year=2019, geometry = T, summary_var = summaryvarcode) %>%
    filter(GEOID %in% fip_list)
  tabl <- tabl %>% transmute(NAME, geometry, estimate = estimate/summary_est) %>% 
    ggplot() +
    geom_sf(data = state_borders, color = 'black', fill = 'grey')+ 
    geom_sf(aes(fill=estimate, color = estimate)) +
    scale_color_viridis_c() + scale_fill_viridis_c()+ 
    coord_sf(datum = NA) +
    theme_minimal()
  return(tabl)
}

disability <- almv_acs_var("S1810_C03_001")
disability_map <- almv_acs_map("S1810_C03_001")

owner_occupied_table <- almv_acs_var_summary("S2502_C04_001")
owner_occupied_map <- almv_acs_var_percent_map("S2502_C04_001", "S2502_C01_001")

renter_occupied_table <- almv_acs_var_summary("S2502_C06_001")
renter_occupied_map <- almv_acs_var_percent_map("S2502_C06_001", "S2502_C01_001")


 

