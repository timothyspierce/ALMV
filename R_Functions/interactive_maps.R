library(leaflet)
library(tigris)
library(tidyverse)
library(htmltools)

# Function to create easy colorblind palette for choropleth map 
# Requires acs data with geometry, variable to be mapped must be 
# called "estimate"
create_chloropleth_map_pal <- function(data) {
  colorNumeric(palette = "viridis", domain = data$estimate)
}
# Function to create percent chlorpleth map for any acs data
# Requires acs data with geometry, variable to be mapped must be 
# called "estimate"
# Only input required the data to be made into a choropleth map, 
# the color palette for the map, labels for the map and a title 
# for the map. 
create_chloropleth_map <- function(data, pal, labels, title) {
  data %>% leaflet %>% addTiles %>% 
    addPolygons(
    color = ~pal(estimate), 
    label = labels,
    popup = labels,
    stroke = T,
    smoothFactor = 0,
    fillOpacity = 0.5, 
    weight = 0.7, 
    highlightOptions = highlightOptions(fillOpacity = 1)) %>% 
    addLegend(pal = pal, values = ~estimate, title = title, 
              labFormat = labelFormat(suffix = "%", transform = function(estimate) 100 * estimate))
}

# Example--------------------------------------------------------------
Black_percentage <- get_acs(geography = "county", variables = "B03002_004",
                            summary_var = "B03002_001", geometry = T) %>%
  filter(GEOID %in% fip_list) %>% transmute(
  NAME, geometry, estimate = estimate / summary_est)

pal <- colorNumeric(palette = "viridis", 
                    domain = Black_percentage$estimate)
labels <- lapply(X = str_c("<strong>", Black_percentage$NAME, "</strong> ", ": ", 
                           scales::percent(as.numeric(formatC(Black_percentage$estimate, format = "f", digits = 3,)))), FUN = htmltools::HTML)

title <- "Percent of Population Black"

create_chloropleth_map(Black_percentage, pal = pal, labels = labels, title = title)
