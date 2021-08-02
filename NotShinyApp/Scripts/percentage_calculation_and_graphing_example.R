#Example of graphing percentages
#Note: requires fip_list of appalachian counties and almv_minimal_map() function
# First, get data from acs using get_acs, notice that the summary_var
# is used to add the population estimate in the returned tibble for summarizing. 
Black_percentage <- get_acs(geography = "county", variables = "B03002_004",
                            summary_var = "B03002_001", geometry = T)
# Filter counties such that the only remaining are those that have
# GEOID's that are also in the fip_list
Black_percentage <- Black_percentage %>% filter(GEOID %in% fip_list)
# Use the transmute function to keep the name and geometry columns and add a new
# estimate column for the estimate of the percentage
Black_percentage <- Black_percentage %>% transmute(
  NAME, geometry, estimate = estimate / summary_est)
# graph 
almv_minimal_map(Black_percentage)

# And an interactive map
library(mapview)
mapview(Black_percentage, zcol = "estimate", legend = T, 
        layer.name = "% Black")


pal <- colorNumeric(palette = "viridis", 
                    domain = Black_percentage$estimate)

Black_percentage %>% leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    color = ~pal(estimate), 
    label = ~str_c(NAME, ": ", scales::percent(as.numeric(formatC(estimate, format = "f", digits = 3)))), 
    stroke = T,
    smoothFactor = 0,
    fillOpacity = 0.5, 
    weight = 0.65, 
    highlightOptions = highlightOptions(fillOpacity = 1)) %>% 
      addLegend(pal = pal, values = ~estimate, title = "Percent of Population Black", 
                labFormat = labelFormat(suffix = "%", transform = function(estimate) 100 * estimate))
  