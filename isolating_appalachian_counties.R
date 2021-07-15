library(ipumsr)
library(dplyr)
library(ggplot2)
library(NLP)
library(tm)
library(SnowballC)
library(wordcloud)
library(pdftools)
library(RColorBrewer)
library(gridExtra)
library(stringr)
library(readxl)
library(tigris)


# Read in IPUMS data with APPAL and PUMA variables
ipums_2009_data <- read_ipums_ddi("usa_00005.xml")
ipums_2009 <- read_ipums_micro(ipums_2009_data)

# Filter data to only include individuals in Appalachia
app_data <- ipums_2009 %>% filter(APPAL != 0)

# Create table of unique Appalachian PUMAs and edit to match format of crosswalk
app_pumas <- as_tibble(unique(app_data$PUMA))
app_pumas <- app_pumas %>% mutate(PUMA00 = value) %>% 
  mutate(PUMA00 = as.character(PUMA00)) %>% 
  mutate(PUMA00 = if_else(
    nchar(PUMA00) == 3, paste0("00", PUMA00), PUMA00)) %>% 
      mutate(PUMA00 = if_else(nchar(PUMA00) == 4, paste0("0", PUMA00), PUMA00))
app_pumas <- pull(app_pumas, PUMA00)


# Read in crosswalk and condense to only include PUMAs and associated states
puma_crosswalk <- read_excel("PUMA2000_PUMA2010_crosswalk.xls")
puma_crosswalk_condensed <- puma_crosswalk %>% 
  select(State00, PUMA00, State10, PUMA10)

# Ensure only Appalachian states are included in crosswalk
puma_crosswalk_app_states <- puma_crosswalk_condensed %>% 
  filter(State00 %in% state_list)

# Create table of PUMAs that are in appalachia and have only remained the same
# Through change from 2000 to 2010 PUMAs
puma_crosswalk_app_pumas <- puma_crosswalk_app_states %>% 
  filter(PUMA00 %in% app_pumas) %>% 
  filter(PUMA00 == PUMA10) %>% 
  distinct()

# Create vector of unique PUMAs that should roughly be in Appalachia
pumas_2010_app <- unique(puma_crosswalk_app_pumas$PUMA00)


# Obtain list of PUMAs for Appalchian states
options(tigris_use_cache = TRUE)
puma_geoms_list <- lapply(state_list, function(x) {
  pumas(state = x, cb = T)
})

puma_geoms <- rbind_tigris(puma_geoms_list)

# Filter to only include PUMAs that should roughly be in Appalachia
puma_app_geoms <- puma_geoms %>% filter(PUMACE10 %in% pumas_2010_app)

# Plot 
ggplot(puma_app_geoms) + geom_sf(fill = "coral2") + theme_minimal() +
  coord_sf(datum = NA)
