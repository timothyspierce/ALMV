library(ipumsr)
library(tm)
library(pdftools)
library(RColorBrewer)
library(gridExtra)
library(readxl)
library(tidyverse)
library(tigris)
library(sf)
library(leaflet)
library(htmltools)
library(leafpop)
library(rvest)

# Read in index data ----------------------------------------------------------------
app_weighted_skills_by_PUMA <- read_csv("App_weighted_skills_by_PUMA.csv")

# Obtain polygons to map onto leaflet for Appalachian PUMAs---------------------

# Narrow down to Appalachian States
counties<-read.csv("ALMV_counties_all.csv", header=T) %>%
  rename(state_code=State)%>%
  mutate(County=str_replace_all(County,"\'|\\.",""))%>%
  mutate(County=str_trim(County, side="both"))
counties$state_code=as.character(counties$state_code)
state_list<-unique(counties$state_code)
state_list[1] <- "01"

# Pull polygons from tigris
options(tigris_use_cache = TRUE)
puma_geoms_list <- lapply(state_list, function(x) {
  pumas(state = x, cb = T)
})
puma_geoms <- rbind_tigris(puma_geoms_list)

# Make PUMAS unique by combining STATEFIP and PUMA
puma_geoms <- puma_geoms %>% unite(STATEFP10, PUMACE10, col = "PUMA", sep = "")

#Obtain list of Appalachian PUMAS
app_pumas <- as_tibble(unique(app_weighted_skills_by_PUMA$PUMA)) %>% rename(PUMA = value)

#Limit tigris data to Appalachia
puma_app_geoms <- semi_join(as.data.frame(puma_geoms), app_pumas)

# Associate index values with geoms 
map_data <- left_join(app_weighted_skills_by_PUMA, puma_app_geoms) %>% 
  select(skillname, PUMA, `Normalized Index`, geometry, NAME10)

# Map for technology-----------------------------------------------------

# Filter to just Tech Design
TechDesign_map_data <- map_data %>% filter(skillname == "TechnologyDesign")

# Make simple feature
TechDesign_map_data <- st_as_sf(TechDesign_map_data) 

# Create palette for map
TechDesign_map_pal <- colorNumeric(palette = "viridis", domain = TechDesign_map_data$`Normalized Index`)

# Add labels 
TechDesign_map_labels <- lapply(X = str_c("<strong>", TechDesign_map_data$NAME10,"</strong>","<br/>" ,"<strong> Index Value: </strong> ", round(TechDesign_map_data$`Normalized Index`, digits = 3)), 
                                FUN = htmltools::HTML)

TechDesign_map <- TechDesign_map_data %>% leaflet() %>% addTiles() %>% 
  addPolygons(
    color = ~TechDesign_map_pal(`Normalized Index`), 
    label = TechDesign_map_labels, 
    stroke = T,
    smoothFactor = 0,
    fillOpacity = 0.65, 
    weight = 0.85, 
    highlightOptions = highlightOptions(fillOpacity = 1), group = "PUMAs") %>% 
  addLegend(pal = TechDesign_map_pal, values = ~`Normalized Index`, 
            title = "Index Value")
# Map for Critical Thinking ------------------------------------------
ReadingComp_map_data <- map_data %>% filter(skillname == "ReadingComprehension")

ReadingComp_map_data <- st_as_sf(ReadingComp_map_data) 

ReadingComp_map_pal <- colorNumeric(palette = "viridis", domain = ReadingComp_map_data$`Normalized Index`)

ReadingComp_map_labels <- lapply(X = str_c("<strong>", ReadingComp_map_data$NAME10,"</strong>","<br/>" ,"<strong> Index Value: </strong> ", round(ReadingComp_map_data$`Normalized Index`, digits = 3)), 
                                 FUN = htmltools::HTML)

ReadingComp_map <- ReadingComp_map_data %>% leaflet() %>% addTiles() %>% 
  addPolygons(
    color = ~ReadingComp_map_pal(`Normalized Index`), 
    label = ReadingComp_map_labels, 
    stroke = T,
    smoothFactor = 0,
    fillOpacity = 0.65, 
    weight = 0.85, 
    highlightOptions = highlightOptions(fillOpacity = 1), group = "PUMAs") %>% 
  addLegend(pal = ReadingComp_map_pal, values = ~`Normalized Index`, 
            title = "Index Value")

# Map for Organization ------------------------------------------
Monitoring_map_data <- map_data %>% filter(skillname == "Monitoring")

Monitoring_map_data <- st_as_sf(Monitoring_map_data) 

Monitoring_map_pal <- colorNumeric(palette = "viridis", domain = Monitoring_map_data$`Normalized Index`)

Monitoring_map_labels <- lapply(X = str_c("<strong>", Monitoring_map_data$NAME10,"</strong>","<br/>" ,"<strong> Index Value: </strong> ", round(Monitoring_map_data$`Normalized Index`, digits = 3)), 
                                FUN = htmltools::HTML)

Monitoring_map <- Monitoring_map_data %>% leaflet() %>% addTiles() %>% 
  addPolygons(
    color = ~Monitoring_map_pal(`Normalized Index`), 
    label = Monitoring_map_labels, 
    stroke = T,
    smoothFactor = 0,
    fillOpacity = 0.65, 
    weight = 0.85, 
    highlightOptions = highlightOptions(fillOpacity = 1), group = "PUMAs") %>% 
  addLegend(pal = Monitoring_map_pal, values = ~`Normalized Index`, 
            title = "Index Value")

# Map for labor----------------------------------------------------
Coordination_map_data <- map_data %>% filter(skillname == "Coordination")

Coordination_map_data <- st_as_sf(Coordination_map_data) 

Coordination_map_pal <- colorNumeric(palette = "viridis", domain = Coordination_map_data$`Normalized Index`)

Coordination_map_labels <- lapply(X = str_c("<strong>", Coordination_map_data$NAME10,"</strong>","<br/>" ,"<strong> Index Value: </strong> ", round(Coordination_map_data$`Normalized Index`, digits = 3)), 
                                  FUN = htmltools::HTML)

Coordination_map <- Coordination_map_data %>% leaflet() %>% addTiles() %>% 
  addPolygons(
    color = ~Coordination_map_pal(`Normalized Index`), 
    label = Coordination_map_labels, 
    popupOptions = popupOptions(max_width = 1000),
    stroke = T,
    smoothFactor = 0,
    fillOpacity = 0.65, 
    weight = 0.85, 
    highlightOptions = highlightOptions(fillOpacity = 1), 
    group = "PUMAs") %>% 
  addLegend(pal = Coordination_map_pal, values = ~`Normalized Index`, 
            title = "Index Value")

# Communication map----------------------------------------------------
ActiveList_map_data <- map_data %>% filter(skillname == "ActiveListening")

ActiveList_map_data <- st_as_sf(ActiveList_map_data) 

ActiveList_map_pal<- colorNumeric(palette = "viridis", domain = ActiveList_map_data$`Normalized Index`)

ActiveList_map_labels <- lapply(X = str_c("<strong>", ActiveList_map_data$NAME10,"</strong>","<br/>" ,"<strong> Index Value: </strong> ", round(ActiveList_map_data$`Normalized Index`, digits = 3)), 
                                FUN = htmltools::HTML)

ActiveList_map <- ActiveList_map_data %>% leaflet() %>% addTiles() %>% 
  addPolygons(
    color = ~ActiveList_map_pal(`Normalized Index`), 
    label = ActiveList_map_labels, 
    stroke = T,
    smoothFactor = 0,
    fillOpacity = 0.65, 
    weight = 0.85, 
    highlightOptions = highlightOptions(fillOpacity = 1), group = "PUMAs") %>% 
  addLegend(pal = ActiveList_map_pal, values = ~`Normalized Index`, 
            title = "Index Value")

# Create piecharts for map------------------------------------------------------
industry_breakdown_app_PUMAs <- read_csv("2019-App_NAICS.csv")
industry_breakdown_app_PUMAs <- industry_breakdown_app_PUMAs %>%
  mutate(relfreq = estimate / summary_est)
NAICS_piechart <- function(GEOID) {
  dataFiltered <- industry_breakdown_app_PUMAs %>% filter(PUMA == as.character(GEOID))
  piechart <- dataFiltered %>% ggplot(aes(x = "", fill = variable, y = relfreq)) + 
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    labs(title = "Industry Makeup") + 
    scale_fill_viridis_d(name = "Industry Name") + theme_void() 
  return(piechart)
}

popup_plot <- lapply(1:length(unique(industry_breakdown_app_PUMAs$PUMA)), function(i) {
  NAICS_piechart(as.character(app_pumas[i, ]))
})

# Add plots to leaflets ------------------------
Monitoring_map <- Monitoring_map %>% 
  addPopupGraphs(popup_plot, group = "PUMAs", width = 700, height = 350)
ActiveList_map <-  ActiveList_map %>% 
  addPopupGraphs(popup_plot, group = "PUMAs", width = 700, height = 350)
ReadingComp_map <-  ReadingComp_map %>% 
  addPopupGraphs(popup_plot, group = "PUMAs", width = 700, height = 350)
TechDesign_map <-  TechDesign_map %>% 
  addPopupGraphs(popup_plot, group = "PUMAs", width = 700, height = 350)
Coordination_map <- Coordination_map %>% 
  addPopupGraphs(popup_plot, group = "PUMAs", width = 700, height = 350)
View(Monitoring_map)

# Add city points-----------------------------------------------------------
# cities_link <- "https://en.wikivoyage.org/wiki/Appalachia"
# page <- read_html(cities_link)
# city <- page %>%  html_nodes("ol li") %>% html_text()
# city <- as_tibble(city) %>% separate(value, sep = " ", into  = c("City", "State"))
# city <- city %>% mutate(State = str_replace_all(State, pattern = "\\)|\\(| ", ""))
# city <- city %>% mutate(City = str_c(City, "city", sep = " ")) %>%
#   unite(City, State, col = "NAME", sep = ", ")
# city_info <- get_acs(geography = "place", variables = "B01003_001",
#         year = 2019, survey = "acs5")
# city_info <- semi_join(as.data.frame(city_info), city) %>% 
#   select(NAME, estimate) %>% rename(City = NAME, Population = estimate)
# city_info <- city_info %>%
#   mutate(City = str_replace(City, pattern = " city", ""))
# write_csv(city, "Appalachian_cities.csv")
# city_coords <- read_csv("geocoded_cities.csv")
# city_coords <- city_coords %>% select(location, lon, lat) %>% 
#   rename(City = location )
# city_info <- inner_join(city_info, city_coords, by = "City")
# write_csv(city_info, "2019-Appalachian_cities_and_population")
city_info <- read_csv("2019-Appalachian_cities_and_population")
labels = lapply(str_c("<strong>", city_info$City,"</strong>","<br/>", "Population: ", 
                      formatC(city_info$Population, format = "f", big.mark = ",", digits = 0)), 
                htmltools::HTML)

Monitoring_map <- Monitoring_map %>% 
  addCircleMarkers(data = city_info,lng = ~lon,
                   lat = ~lat,
                   label = labels, 
                   radius = ~Population/50000, 
                   color = "blue")
ActiveList_map <-  ActiveList_map %>% 
  addCircleMarkers(data = city_info,lng = ~lon,
                   lat = ~lat,
                   label = labels, 
                   radius = ~Population/50000, 
                   color = "blue")
ReadingComp_map <-  ReadingComp_map %>% 
  addCircleMarkers(data = city_info,lng = ~lon,
                   lat = ~lat,
                   label = labels, 
                   radius = ~Population/50000, 
                   color = "blue")
TechDesign_map <-  TechDesign_map %>% 
  addCircleMarkers(data = city_info,lng = ~lon,
                   lat = ~lat,
                   label = labels, 
                   radius = ~Population/50000, 
                   color = "blue")
Coordination_map <- Coordination_map %>% 
  addCircleMarkers(data = city_info,lng = ~lon,
                   lat = ~lat,
                   label = labels, 
                   radius = ~Population/50000, 
                   color = "blue")
