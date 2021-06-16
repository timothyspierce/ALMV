
library(tidycensus)
library(tidyverse)
library(dplyr)
library(stringr)

counties<-read.csv("ALMV_counties_all.csv", header=T) %>%
  rename(state_code=State)%>%
  mutate(County=str_replace_all(County,"\'|\\.",""))%>%
  mutate(County=str_trim(County, side="both"))
counties$state_code=as.character(counties$state_code)
state_list<-unique(counties$state_code)


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
          geometry = TRUE) %>%
    filter(GEOID %in% fip_list) %>%
    select(GEOID, NAME, estimate, geometry) %>% 
    ggplot() + geom_sf(aes(fill=estimate))
}

almv_acs_var <- function(varcode){
  get_acs(geography="county",
          state=state_list,
          variable=varcode,
          year=2019) %>%
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

