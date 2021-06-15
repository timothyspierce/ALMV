
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
          year=2019,
          geometry = TRUE) %>%
    filter(GEOID %in% fip_list) %>%
    select(GEOID, NAME, estimate)
}
median_income <- almv_acs_get("S2001_C01_002")
poverty <- almv_acs_get("S1701_C03_001")
black_pop <- almv_acs_get("B02001_003")
hs_dropout <- almv_acs_get("B06009_002")
hs_only <- almv_acs_get("B06009_003")
ceo <- almv_acs_get("B24114_001")
almv_acs_var("B24115_003")

