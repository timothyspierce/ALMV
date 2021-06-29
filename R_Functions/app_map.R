
library(tidycensus)
library(tidyverse)
library(dplyr)
library(stringr)
library(tigris)

counties<-read.csv("ALMV_counties_all.csv", header=T) %>%
  rename(state_code=State)%>%
  mutate(County=str_replace_all(County,"\'|\\.",""))%>%
  mutate(County=str_trim(County, side="both"))
counties$state_code=as.character(counties$state_code)
state_list<-unique(counties$state_code)
state_list[1] <- "01"



fips<-read.csv("fips_codes.csv", header=T) %>%
  mutate(state_code= str_sub(FIPS, 1, -4))%>%
  rename(County=Name)


fips_merge<-left_join(counties, fips, by=c("County", "state_code"))

fip_list<-sprintf("%05d",fips_merge$FIPS)

almv_acs_map <- function(varcode){
  get_acs(geography="county",
          state=state_list,
          variable=varcode,
          year=2019,
          geometry = TRUE,
          survey = "acs1") %>%
    filter(GEOID %in% fip_list) %>%
    select(GEOID, NAME, estimate, geometry) %>% 
    ggplot() + geom_sf(aes(fill=estimate))
}


#Increase granularity to view heterogeneity at the county subdivision level 
almv_acs_map_subdivision <- function(varcode, sumvar){
  storevar <- map_dfr(state_list, ~ get_acs(geography = "county subdivision",
                                            variables = varcode,
                                            summary_var = sumvar,
                                            geometry = T,
                                            state = .)) %>% 
    mutate(NEWID = substr(as.character(GEOID),1,5)) %>% 
    filter(NEWID %in% fip_list) %>% 
    transmute(NAME, geometry, estimate = 100*(estimate/summary_est)) 
    almv_minimal_map(storevar)
}

almv_acs_var <- function(varcode){
  get_acs(geography="county",
          state=state_list,
          variables =varcode,
          year=2019, geometry = T) %>%
    filter(GEOID %in% fip_list)
}

almv_acs_var_summary <- function(varcode, summaryvarcode){
  tabl <- get_acs(geography="county",
          state=state_list,
          variables =varcode,
          year=2019, geometry = T, summary_var = summaryvarcode) %>%
    filter(GEOID %in% fip_list)
  tabl <- tabl %>% transmute(NAME, geometry, estimate = estimate/summary_est)
  return(tabl)
}

almv_acs_table <- function(varcode){
  get_acs(geography="county",
          state=state_list,
          table = varcode,
          year=2019,survey = "acs1") %>%
    filter(GEOID %in% fip_list) %>%
    select(NAME, estimate)
}

almv_acs_hist <- function(varcode){
  get_acs(geography="county",
          state=state_list,
          variable=varcode,
          year=2019) %>%
    filter(GEOID %in% fip_list) %>%
    select(NAME, estimate) %>% 
    ggplot() + geom_histogram(aes(x = estimate),
                              binwidth = 1000)
}
state_borders <- states(cb = T)
state_borders <- state_borders %>% 
                  filter(STATEFP %in% state_list)
#arg tibble, tibble of values with estimate to plot and geometry info
almv_minimal_map <- function(tibble) {
  ggplot() + 
    # graphing the state borders of Appalachian states. 
    geom_sf(data = state_borders, color = 'black', fill = 'grey')+ 
    # graphing the estimate in each county, associating 
    # county fill and border color with the estimate
    geom_sf(data = tibble, aes(fill = estimate, color = estimate))+ 
    # applying continuous color scheme to fill and border
    scale_color_viridis_c() + scale_fill_viridis_c()+ 
    # removing grid lines
    coord_sf(datum = NA) +
    # applying minimal theme
    theme_minimal()
}




