library(leaflet)
library(tigris)
library(tidyverse)
library(htmltools)

#Function to create proper palette for chlorpleth map 
create_chloropleth_map_pal <- function(data) {
  colorNumeric(palette = "viridis", domain = data$estimate)
}
# Function to create percent chlorpleth map for any acs data 
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
pal <- colorNumeric(palette = "viridis", 
                    domain = Black_percentage$estimate)
labels <- lapply(X = str_c("<strong>", Black_percentage$NAME, "</strong> ", ": ", 
                           scales::percent(as.numeric(formatC(Black_percentage$estimate, format = "f", digits = 3,)))), FUN = htmltools::HTML)

title <- "Percent of Population Black"
