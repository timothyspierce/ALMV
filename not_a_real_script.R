library(tidyverse)
library(tidycensus)
library(dplyr)
library(ggplot2)
library(gridExtra)

almv_minimal_map <- function(tibble) {
  ggplot() + 
    # graphing the state borders of Appalachian states. 
    geom_sf(data = state_borders, color = 'black', fill = 'grey')+ 
    # graphing the estimate in each county, associating 
    # county fill and border color with the estimate
    geom_sf(data = tibble, aes(fill = Percent, color = Percent))+ 
    # applying continuous color scheme to fill and border
    scale_color_viridis_c() + scale_fill_viridis_c()+ 
    # removing grid lines
    coord_sf(datum = NA) +
    # applying minimal theme
    theme_minimal()
}

almv_acs_edu_map <- function(varname, varcode){
  varname <- get_acs(geography="county",
                  state=state_list,
                  variables =varcode,
                  year=2019, geometry = T, summary_var = "B15003_001") %>%
    filter(GEOID %in% fip_list)
  varname <- varname %>% transmute(NAME, geometry, Percent = 100*(estimate/summary_est))
  almv_minimal_map(varname)
}


PhD <- almv_acs_edu_map(PhD, "B15003_025") + labs(title ="Phd")
Masters <- almv_acs_edu_map(MS, "B15003_023") +labs(title = "Masters") 
Bachelors <- almv_acs_edu_map(BS, "B15003_022") +labs(title = "Bachelors")
Associates <- almv_acs_edu_map(AS, "B15003_021") + labs(title = "Associates")
SomeCollege <- almv_acs_edu_map(SC, "B15003_019") + labs(title = "Some College")
HighSchool <- almv_acs_edu_map(HS, "B15003_017") + labs(title = "HS Diploma")
alledu <- grid.arrange(PhD, Masters, Bachelors, Associates, SomeCollege, HighSchool, nrow = 2, ncol = 3)
alledu

income <- get_acs(geography = "county", variables = varcode,
               summary_var = "B15003_001", geometry = T)

#Map out technology with total population as a summary variable 
almv_acs_tech_map <- function(varname, varcode){
  varname <- get_acs(geography="county",
                     state=state_list,
                     variables = varcode,
                     year=2019, geometry = T, summary_var = "S2801_C01_001") %>%
    filter(GEOID %in% fip_list)
  varname <- varname %>% transmute(NAME, geometry, Percent = 100*(estimate/summary_est))
  almv_minimal_map(varname)
}

compdevice <- almv_acs_tech_map(compdevice, "S2801_C01_002") + labs(title = "With Some Device")
computer <- almv_acs_tech_map(computer, "S2801_C01_003") + labs(title = "With a Computer")
internet <- almv_acs_tech_map(internet, "S2801_C01_012") + labs(title = "With Internet")
no_internet <- almv_acs_tech_map(no_internet, "S2801_C01_019") + labs(title = "Without Internet")

#Add them together
grid.arrange(compdevice, computer, internet, no_internet, ncol = 2, top = "Technology in the Households")    



#Function with poor population as anchor variable 
almv_acs_poor_internet <- function(varname, varcode){
  varname <- get_acs(geography="county",
                     state=state_list,
                     variables = varcode,
                     year=2019, geometry = T, summary_var = "S2801_C01_020") %>%
    filter(GEOID %in% fip_list)
  varname <- varname %>% transmute(NAME, geometry, Percent = 100*(estimate/summary_est))
  almv_minimal_map(varname)
}

#Function with midincome population as anchor variable 
almv_acs_mid_internet <- function(varname, varcode){
  varname <- get_acs(geography="county",
                     state=state_list,
                     variables = varcode,
                     year=2019, geometry = T, summary_var = "S2801_C01_024") %>%
    filter(GEOID %in% fip_list)
  varname <- varname %>% transmute(NAME, geometry, Percent = 100*(estimate/summary_est))
  almv_minimal_map(varname)
}
#Function with rich population as anchor variable 
almv_acs_rich_internet <- function(varname, varcode){
  varname <- get_acs(geography="county",
                     state=state_list,
                     variables = varcode,
                     year=2019, geometry = T, summary_var = "S2801_C01_028") %>%
    filter(GEOID %in% fip_list)
  varname <- varname %>% transmute(NAME, geometry, Percent = 100*(estimate/summary_est))
  almv_minimal_map(varname)
}


all_no_internet <- almv_acs_tech_map(no_internet, "S2801_C01_019") + labs(title = "All Incomes")
poor_no_internet <- almv_acs_poor_internet(poor, "S2801_C01_023") + labs(title = "Under 20k")
mid_no_internet <- almv_acs_mid_internet(mid, "S2801_C01_027") + labs(title = "20k-75k")
rich_no_internet <- almv_acs_rich_internet(mid, "S2801_C01_031") + labs(title = "Over 75k")

#Add them together
grid.arrange(all_no_internet, poor_no_internet, mid_no_internet, rich_no_internet, ncol = 2, top = "No Internet Subscription by Income")
