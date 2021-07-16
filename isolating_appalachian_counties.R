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
ipums_2009_data <- read_ipums_ddi("usa_00006.xml")
ipums_2009 <- read_ipums_micro(ipums_2009_data)

# Filter data to only include individuals in Appalachia
app_data <- ipums_2009 %>% filter(APPAL != 0)

# Create table of unique Appalachian PUMAs and edit to match format of crosswalk
app_pumas <- app_data %>% group_by(STATEFIP, PUMA) %>% summarise() %>% ungroup()
app_pumas <- app_pumas %>% mutate(PUMA00 = PUMA) %>% 
  mutate(PUMA00 = as.character(PUMA00)) %>% 
  mutate(PUMA00 = if_else(
    nchar(PUMA00) == 3, paste0("00", PUMA00), PUMA00)) %>% 
  mutate(PUMA00 = if_else(nchar(PUMA00) == 4, paste0("0", PUMA00), PUMA00)) %>% 
  mutate(State00 = STATEFIP) %>% select(PUMA00, State00) 
app_pumas <- app_pumas %>%
  mutate(State00 = as.character(State00), PUMA00 = as.character(PUMA00)) %>% 
  mutate(State00 = if_else(nchar(State00) == 1, paste0("0", State00), State00))

# Read in crosswalk and condense to only include PUMAs and associated states
puma_crosswalk <- read_excel("PUMA2000_PUMA2010_crosswalk.xls")
puma_crosswalk_condensed <- puma_crosswalk %>% 
  select(State00, PUMA00, State10, PUMA10)

# Create table of all year 2000 app PUMAs and States w their corresponding 2010 values
puma_crosswalk_app <- 
  inner_join(app_pumas, puma_crosswalk_condensed, by = c("State00", "PUMA00") )

# Create vector of unique appalachian 2010 PUMAs
pumas_2010_app <- puma_crosswalk_app %>% group_by(State10, PUMA10) %>% 
  summarise() %>% distinct()


# Obtain list of PUMA sfs for Appalchian states
options(tigris_use_cache = TRUE)
puma_geoms_list <- lapply(state_list, function(x) {
  pumas(state = x, cb = T)
})

puma_geoms <- rbind_tigris(puma_geoms_list)

# Filter to only include PUMA sfs that should roughly be in Appalachia
pumas_2010_app <- pumas_2010_app %>%
  rename(STATEFP10 = State10, PUMACE10 = PUMA10)
puma_app_geoms <- semi_join(as.data.frame(puma_geoms), pumas_2010_app)

# Plot 
ggplot(st_as_sf(puma_app_geoms)) + geom_sf(fill = "coral2") + theme_minimal() +
  coord_sf(datum = NA)
