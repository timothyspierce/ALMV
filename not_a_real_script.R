library(tidyverse)
library(tidycensus)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(rayshader)
library(av)
library(purrr)
library(sf)




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
  varname <- varname %>% transmute(NAME, geometry, estimate = 100*(estimate/summary_est))
  almv_minimal_map(varname)
}


PhD <- almv_acs_edu_map(PhD, "B15003_025") + labs(title ="Phd") + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank(),legend.title = element_blank(), legend.position="bottom") 
Masters <- almv_acs_edu_map(MS, "B15003_023") +labs(title = "Masters") + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank(),legend.title = element_blank(), legend.position="bottom") 
Bachelors <- almv_acs_edu_map(BS, "B15003_022") +labs(title = "Bachelors")+ theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank(),legend.title = element_blank(), legend.position="bottom") 
Associates <- almv_acs_edu_map(AS, "B15003_021") + labs(title = "Associates")+ theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank(),legend.title = element_blank(), legend.position="bottom") 
SomeCollege <- almv_acs_edu_map(SC, "B15003_019") + labs(title = "Some College")+ theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), legend.title = element_blank(),legend.position="bottom") 
HighSchool <- almv_acs_edu_map(HS, "B15003_017") + labs(title = "HS Diploma")+ theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), legend.title = element_blank(),legend.position="bottom") 
alledu <- grid.arrange(PhD, Masters, Bachelors, Associates, SomeCollege, HighSchool, ncol = 6)
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
  almv_minimal_map_V2(varname)
}

compdevice <- almv_acs_tech_map(compdevice, "S2801_C01_002") + labs(title = "With Some Device")+ theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), legend.position="bottom")
computer <- almv_acs_tech_map(computer, "S2801_C01_003") + labs(title = "With a Computer")+ theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), legend.position="bottom")
internet <- almv_acs_tech_map(internet, "S2801_C01_012") + labs(title = "With Internet")+ theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), legend.position="bottom")
no_internet <- almv_acs_tech_map(no_internet, "S2801_C01_019") + labs(title = "Without Internet")+ theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), legend.position="bottom")

#Add them together
grid.arrange(compdevice, computer, internet, no_internet, ncol = 4, top = "Technology in the Household")    



#Function with poor population as anchor variable 
almv_acs_poor_internet <- function(varname, varcode){
  varname <- get_acs(geography="county",
                     state=state_list,
                     variables = varcode,
                     year=2019, geometry = T, summary_var = "S2801_C01_020") %>%
    filter(GEOID %in% fip_list)
  varname <- varname %>% transmute(NAME, geometry, Percent = 100*(estimate/summary_est))
  almv_minimal_map_V2(varname)
}

#Function with midincome population as anchor variable 
almv_acs_mid_internet <- function(varname, varcode){
  varname <- get_acs(geography="county",
                     state=state_list,
                     variables = varcode,
                     year=2019, geometry = T, summary_var = "S2801_C01_024") %>%
    filter(GEOID %in% fip_list)
  varname <- varname %>% transmute(NAME, geometry, Percent = 100*(estimate/summary_est))
  almv_minimal_map_V2(varname)
}
#Function with rich population as anchor variable 
almv_acs_rich_internet <- function(varname, varcode){
  varname <- get_acs(geography="county",
                     state=state_list,
                     variables = varcode,
                     year=2019, geometry = T, summary_var = "S2801_C01_028") %>%
    filter(GEOID %in% fip_list)
  varname <- varname %>% transmute(NAME, geometry, Percent = 100*(estimate/summary_est))
  almv_minimal_map_V2(varname)
}


all_no_internet <- almv_acs_tech_map(no_internet, "S2801_C01_019") + labs(title = "All Incomes") + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), legend.position="bottom")
poor_no_internet <- almv_acs_poor_internet(poor, "S2801_C01_023") + labs(title = "Under 20k")+ theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), legend.position="bottom")
mid_no_internet <- almv_acs_mid_internet(mid, "S2801_C01_027") + labs(title = "20k-75k")+ theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), legend.position="bottom")
rich_no_internet <- almv_acs_rich_internet(mid, "S2801_C01_031") + labs(title = "Over 75k")+ theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), legend.position="bottom")

#Add them together
grid.arrange(all_no_internet, poor_no_internet, mid_no_internet, rich_no_internet, ncol = 4, top = "No Internet Subscription by Income")

all_no_internet_3d <- almv_acs_tech_map(no_internet, "S2801_C01_019") + labs(title = "Percent without Internet Subscription") 
plot_gg(all_no_internet_3d)

plot_gg(all_no_internet_3d)
render_movie(filename = "app_internet.mp4", type = "orbit")


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

almv_minimal_map_V2 <- function(tibble) {
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

almv_acs_map_subdivision("B15003_017", "B15003_001")

#Population
almv_acs_map("B01003_001") + labs(title = "Log of Total Population by County ")

almv_acs_map("S0101_C01_032") + labs(title = "Median Age by County")

almv_acs_map("S1901_C01_012") + labs(title = "Log of Median Income by County") 
almv_acs_map("S1901_C01_013") + labs(title = "Average Income by County") 

