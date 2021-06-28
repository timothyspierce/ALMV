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
# Filter counties such that the only remaining are those that have
# GEOID's that are also in the fip_list
var <-var %>% filter(GEOID %in% fip_list)
var <- var %>% transmute(
  NAME, geometry, estimate = 100* (estimate / summary_est))
almv_minimal_map(var)