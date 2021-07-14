
library(tidycensus)
library(tidyverse)
library(dplyr)
library(stringr)
library(tigris)

#Create state list
counties<-read.csv("ALMV_counties_all.csv", header=T) %>%
  rename(state_code=State)%>%
  mutate(County=str_replace_all(County,"\'|\\.",""))%>%
  mutate(County=str_trim(County, side="both"))
counties$state_code=as.character(counties$state_code)
state_list<-unique(counties$state_code)
state_list[1] <- "01"

#Fips Codes
fips<-read.csv("fips_codes.csv", header=T) %>%
  mutate(state_code= str_sub(FIPS, 1, -4))%>%
  rename(County=Name)
fips_merge<-left_join(counties, fips, by=c("County", "state_code"))
fip_list<-sprintf("%05d",fips_merge$FIPS)

#Map by county using 2019 ACS 5 data. All you need is the variable code— ex. S1901_C01_001 which is table S1901, column 1, variable 1.
almv_acs_map <- function(varcode){
  get_acs(geography="county",
          state=state_list,
          variable=varcode,
          year=2019,
          geometry = TRUE,
          survey = "acs5") %>%
    filter(GEOID %in% fip_list) %>%
    transmute(GEOID, NAME, estimate = log(estimate), geometry) %>% 
    ggplot() +
    geom_sf(data = state_borders, color = 'black', fill = 'grey')+ 
   geom_sf(aes(fill=estimate, color = estimate)) +
    scale_color_viridis_c() + scale_fill_viridis_c()+ 
    coord_sf(datum = NA) +
    theme_minimal()
}



#Increase granularity to view heterogeneity at the county subdivision level 
#This is the same map as before but at the county subdivision level. its a lot of data and a bit slow, but it may be helpful.
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

#This will pull a single variable for all 420 counties. ex. S1901_C01_001
almv_acs_var <- function(varcode){
  get_acs(geography="county",
          state=state_list,
          variables =varcode,
          year=2019, geometry = T) %>%
    filter(GEOID %in% fip_list)
}

#this is a bit more complicated but much more useful. This produces a percentage. 'varcode' should be the variable of interest. 
#For example, number of people with PhD, B15003_025 is number of PhD holders. summaryvarcode should be your base number. In this case, total population which B15003_001.
#This function will give you varcode as a percent of summaryvarcode.
almv_acs_var_summary <- function(varcode, summaryvarcode){
  tabl <- get_acs(geography="county",
          state=state_list,
          variables =varcode,
          year=2019, geometry = T, summary_var = summaryvarcode) %>%
    filter(GEOID %in% fip_list)
  tabl <- tabl %>% transmute(NAME, geometry, estimate = estimate/summary_est)
  return(tabl)
}

#This returns a table. example S1901. All values for all variables in the table.
almv_acs_table <- function(varcode){
  get_acs(geography="county",
          state=state_list,
          table = varcode,
          year=2019,survey = "acs5") %>%
    filter(GEOID %in% fip_list) %>%
    select(NAME, estimate)
}

#histogram of values for a variable 
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

#austin made this and its a much more aesthetically pleasing map
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




