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


# Appalachian Skills------------------------------------------------------------
## Process of obtaining 2019 IPUMS info for Appalachia ---------------------------------------------

# The following outlines how the IPUMS data was read in and turned
# Into a csv that could be stored on github


# Read in IPUMS data extract for Appalachia from large local files
ddi <- read_ipums_ddi("usa_00007.xml")

data <- read_ipums_micro(ddi)

### Process of Isolating 2010 PUMAs to Appalachia -------------------------------------------

# The following outlines how 2000 Appalachian PUMA information was used to create
# 2010 Appalachian PUMAs, which are the PUMAs used in the 2019 IPUMS data

# Read in IPUMS data with APPAL (variable denotes if year 2000 PUMA is in Appalachia) 
# and PUMA variables from large local file
ipums_2009_data <- read_ipums_ddi("usa_00006.xml")
ipums_2009 <- read_ipums_micro(ipums_2009_data)

# Filter data to only include individuals in Appalachia
app_data <- ipums_2009 %>% filter(APPAL != 0)

# Create table of unique Appalachian PUMAs and edit to match format of crosswalk
app_pumas <- app_data %>% group_by(STATEFIP, PUMA) %>% summarise() %>% ungroup()

app_pumas <- app_pumas %>% rename(PUMA00 = PUMA) %>% 
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

# Inner join to create table of all year 2000 app PUMAs and States w their corresponding 2010 values
puma_crosswalk_app <- 
  inner_join(app_pumas, puma_crosswalk_condensed, by = c("State00", "PUMA00") )

# Create vector of unique appalachian 2010 PUMAs
pumas_2010_app <- puma_crosswalk_app %>% group_by(State10, PUMA10) %>% 
  summarise() %>% distinct()


#### Map to check accuracy-------------------------------------------------------

# Obtain list of PUMA sfs for Appalchian states
options(tigris_use_cache = TRUE)
counties<-read.csv("ALMV_counties_all.csv", header=T) %>%
  rename(state_code=State)%>%
  mutate(County=str_replace_all(County,"\'|\\.",""))%>%
  mutate(County=str_trim(County, side="both"))
counties$state_code=as.character(counties$state_code)
state_list<-unique(counties$state_code)
state_list[1] <- "01"
puma_geoms_list <- lapply(state_list, function(x) {
  pumas(state = x, cb = T)
})

puma_geoms <- rbind_tigris(puma_geoms_list)

# Filter to only include PUMA sfs that should roughly be in Appalachia
pumas_2010_app <- pumas_2010_app %>%
  rename(STATEFP10 = State10, PUMACE10 = PUMA10)
puma_app_geoms <- semi_join(as.data.frame(puma_geoms), pumas_2010_app)

# Obtain ARC definition of Appalachian county polygons

#Obtain polygons of Appalachian states only 
app_counties <- counties(state = state_list, cb = T)
app_counties <- app_counties %>%
  unite(STATEFP, COUNTYFP, col = "FIP", sep = "") 
app_counties <- as.data.frame(app_counties)

# Obtain list of FIPS for Appalachian counties
fips<-read.csv("fips_codes.csv", header=T) %>%
  mutate(state_code= str_sub(FIPS, 1, -4))%>%
  rename(County=Name)
fips_merge<-left_join(counties, fips, by=c("County", "state_code"))
fip_list<-sprintf("%05d",fips_merge$FIPS)
fip_list <- as_tibble(fip_list) %>% rename(FIP = value)

#Limit app_counties to the 420 Appalachian counties
app_counties <- semi_join(app_counties, fip_list)

# Plot to compare 2000 to 2010 definitions of Appalachia
ggplot() + geom_sf(data = st_as_sf(puma_app_geoms), fill = "coral2", color = "black" ) + 
  theme_minimal() +
  geom_sf(data = st_as_sf(app_counties), fill = "grey", color = NA) + 
  coord_sf(datum = NA)

## Isolate IPUMS info to Appalachia--------------------------------------------------

# Read in 2010 Appalachian PUMAs to Isolate IPUMS data to Appalachia
app_pumas_2010 <- read_csv("2010_PUMAs_App.csv")
app_pumas_2010 <- app_pumas_2010 %>%
  rename(PUMA = PUMACE10, STATEFIP = STATEFP10) %>%
  mutate(PUMA = as.numeric(PUMA), STATEFIP = as.numeric(STATEFIP))

# Semi-join IPUMS info with PUMA info to obtain only Appalachian IPUMS 
app_ipums <-
  semi_join(data, app_pumas_2010)


# Filter out the unemployed and exchange OCCSOC's
## that have X's or Y's for 9's
app_ipums <- app_ipums %>% filter(OCCSOC > 0)
app_ipums <- app_ipums %>%
  mutate(OCCSOC = str_replace_all(OCCSOC, "XXX", "199")) %>%
  mutate(OCCSOC = str_replace_all(OCCSOC, "XX", "99")) %>%
  mutate(OCCSOC = str_replace_all(OCCSOC, "X", "9")) %>%
  mutate(OCCSOC = str_replace_all(OCCSOC, "Y", "9"))

app_ipums <- app_ipums %>% mutate(STATEFIP = as.character(STATEFIP), PUMA = as.character(PUMA)) %>%
  mutate(STATEFIP = if_else(str_count(STATEFIP) == 1, str_c("0", STATEFIP), STATEFIP)) %>%
  mutate(PUMA = if_else(
    nchar(PUMA) == 3, paste0("00", PUMA), PUMA)) %>%
  mutate(PUMA = if_else(nchar(PUMA) == 4, paste0("0", PUMA), PUMA))%>%
  mutate(PERWT = as.numeric(PERWT)) %>%
  unite(STATEFIP, PUMA, col = "PUMA", sep = "") %>%
  select(YEAR, PUMA, OCCSOC, PERWT)


# Write IMPUS info to csv to be stored on github 
# write_csv(app_ipums, "2019-Appalachian_IPUMS.csv")


## Create SOC Frequency Counts-------------------------------------------------

# read in IPUMS data for Appalachia in 2019
app_ipums <- read_csv("2019-Appalachian_IPUMS.csv")


# Create tibble of soc's with their associated frequencies by PUMA
socfreq_by_puma <- app_ipums %>% 
  group_by(PUMA, OCCSOC) %>% 
  summarise(socfreq = sum(PERWT)) %>% 
  pivot_wider(names_from = PUMA, values_from = socfreq) %>% 
  rename(soc = OCCSOC) %>% ungroup()
View(socfreq_by_puma)


## Creating Usable Skills Information------------------------------------------ 

#Read and adjust skills data
skills <- read_excel("Skills_Onet.xlsx")
colnames(skills)[1] <- "soc"
colnames(skills)[4] <- "skillname"
colnames(skills)[5] <- "id"
skills <- mutate(skills, soc = substr(soc,1,7))
skills <- mutate(skills, soc = gsub("-", "", x = soc))
skills <- mutate(skills, skillname = gsub(" ", "",skillname))


# Change soc codes from 2010 to 2019
new_socs <- read.csv("2010_to_2019_Crosswalk.csv")

new_socs <- new_socs %>%
  rename(soc = `O.NET.SOC.2010.Code`) %>%
  rename(soc_2019 =`O.NET.SOC.2019.Code` ) %>%
  mutate(soc = substr(soc,1,7)) %>%
  mutate(soc = gsub("-", "", x = soc)) %>%
  mutate(soc_2019 = substr(soc_2019,1,7)) %>%
  mutate(soc_2019 = gsub("-", "", x = soc_2019)) %>%
  select(soc, soc_2019) %>% distinct()

skills <- inner_join(skills, new_socs, by = "soc") %>%
  mutate(soc = soc_2019) %>%
  select(-soc_2019)
skills <- distinct(skills)


# View(skills)

# Create skills tibble with individual columns for importance and level
# Select only the soc's, skillnames and their associated importance and level

skills_wide <-
  skills %>%
  pivot_wider(names_from = `Scale Name`, values_from = `Data Value`) %>%
  fill(Importance, .direction = "down") %>%
  fill(Level, .direction = "up") %>%
  select(soc, skillname, Importance, Level) %>%
  distinct()
skills_wide <- skills_wide %>%
  group_by(soc, skillname) %>%
  mutate(Importance = mean(Importance)) %>%
  mutate(Level = mean(Level)) %>%
  distinct() %>% ungroup()

# Write skills to csv
# write_csv(skills_wide, file = "2019-ONet_Skills_Tidy.csv")


# Alter SOC codes to match O*Net's similar "OCCSOC's" 

# Find soc's not present in O*net data
skills_wide$soc <- as.numeric(skills_wide$soc)

# Find soc's not present in O*net data by PUMA 
socs_na_by_PUMA <- anti_join(socfreq_by_puma, skills_wide) %>%
  mutate(soc = as.character(soc))

# change those soc's to end in 1 that end in 0 by PUMA
altered_na_socs_by_puma <- socs_na_by_PUMA %>%
  mutate(soc = str_replace_all(soc, "00$", "99")) %>%
  mutate(soc = str_replace_all(soc, "0$", "1"))

# Identify all socs that exsist in O*Net data by PUMA
soc_not_na_by_PUMA <- semi_join(socfreq_by_puma, skills_wide)

# Combine similar and altered socs into one to have matching SOCS
altered_na_socs_by_puma$soc <- as.numeric(altered_na_socs_by_puma$soc)
altered_socs_freq_by_puma<- bind_rows(soc_not_na_by_PUMA, altered_na_socs_by_puma)


# Write to csv
# write_csv(altered_socs_freq_by_puma, file = "2019-Appalachian_Occupation_Breakdown.csv")


# Read csv
altered_socs_freq_by_puma <- read_csv("2019-Appalachian_Occupation_Breakdown.csv")


## Index Creation ------------------------------------------------------------------

# Read in skills data
skills_wide <- read_csv("2019-ONet_Skills_Tidy.csv")

# View(skills_wide)


# Create a standardized table with new "Importance level" column,
# created through the product of each importance and level ranking 
# on a scale from 0 to 1.   

skills_standardized <- skills_wide  %>% 
  mutate(Level = (Level / max(skills_wide$Level)), Importance = Importance / 5) %>% 
  # Consider dividing by 6.01(max value observed in App)
  mutate(`Importance Level` = Importance * Level) %>% 
  select(-Importance, -Level) %>% 
  unique()

# Create table with one index per SOC skill by averaging importance
# levels of "duplicate" (due to limited granularity) soc & skill combinations
skills_indexed <- skills_standardized %>% 
  group_by(soc, skillname) %>% 
  mutate(`index` = mean(`Importance Level`)) %>% 
  select(-`Importance Level`) %>% 
  unique() %>% ungroup()


# Create a tibble with skills for each soc and their index

skills_indexed_counts_by_PUMA <- right_join(skills_indexed, altered_socs_freq_by_puma)
View(skills_indexed_counts_by_PUMA)

# Determine which soc's are null
null_socs_by_PUMA <-  skills_indexed_counts_by_PUMA %>% filter(is.na(index))
View(null_socs_by_PUMA)

# Create an indexed counts with only soc's in common to those in Appalachia
skills_index_common_by_PUMA <- inner_join(skills_indexed, altered_socs_freq_by_puma)
View(skills_index_common_by_PUMA)



#Create weighted index of skills in Appalachian Labor Market
skills_index_common_by_PUMA_weighted <- skills_index_common_by_PUMA %>% 
  mutate(across(4:232, function(x) index * x))

app_weighted_skills_by_PUMA <- skills_index_common_by_PUMA_weighted %>% 
  mutate(across(4:232, ~replace_na(.x, 0))) %>% 
  group_by(skillname) %>% 
  summarize(across(4:231, sum)) %>% 
  mutate(across(2:229, function(x) (x / sum(x)) * 100 )) %>% 
  pivot_longer(cols = 2:229, names_to = "PUMA", values_to = "Normalized Index")


# Write to csv 
# write_csv(app_weighted_skills_by_PUMA, "App_weighted_skills_by_PUMA.csv")



## Skills of the Future ---------------------------------------------------------

# Read in jobs of future
future_jobs <- read_excel(
  "Rapid_Growth.xls", skip = 4, col_names = c("soc", "occupation"))

# Alter soc codes to match our format
future_jobs <- future_jobs %>% 
  mutate(soc = str_replace_all(soc, pattern = "-", replacement = "")) %>% 
  mutate(soc = str_sub(soc, 1, 6))
# semi_join to obtain skill indices only in jobs of the future
future_jobs$soc <- as.numeric(future_jobs$soc)
future_jobs_skills <- semi_join(skills_indexed, future_jobs)

# Sum indices for each skill and soc combination to obtain 
# indices for skills of the future. 
skills_future <- future_jobs_skills %>% 
  group_by(skillname) %>% 
  summarize(index = sum(index))

skills_future <- skills_future %>% mutate(`Normalized Index` = (index/max(skills_future$index)))

# Visualize skills of the future
skills_future %>% ggplot() +
  geom_col(aes(x = `Normalized Index`, y = reorder(skillname, `Normalized Index`)), fill = "coral2") +  
  labs(x = "Index", y = "Skillname", title = "Skills of the Future") +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0), limits = c(0, 1))



# Group by categories

# Create lists of skills for each category 
critical_thinking <- "Active Learning, Learning Strategies, Critical Thinking, Reading Comprehension, Complex Problem Solving, Troubleshooting, Mathematics, Science Quality Control Analysis, Systems Analysis, Systems Evaluation, Operation Analysis"
critical_thinking <- as_vector(str_split(critical_thinking, pattern = ", ")) %>% str_remove_all(" ")

communication <- "Speaking, Persuasion, Negotiation, Judgement and Decision Making, Social Perceptiveness, Instructing, Active Listening, Writing"
communication <- as_vector(str_split(communication, pattern = ", ")) %>% str_remove_all(" ")



labor <- "Coordination, Installation, Equipment Maintenance, Repairing, Service Orientation, Equipment Selection "
labor <- as_vector(str_split(labor, pattern = ", ")) %>% str_remove_all(" ")

technology <- "Technology Design, Programming "
technology <-  as_vector(str_split(technology, pattern = ", ")) %>% str_remove_all(" ")

organization <- "Management of Personnel Resources, Operation and Control, Monitoring, Time Management, Management of Financial Resources, Management of Financial Resources, Management of Personnel Resources, Management of Material Resources "
organization <- as_vector(str_split(organization, pattern = ", ")) %>% str_remove_all(" ")

# Break skills down into groups and add index value to graph
app_crit_think <- skills_future %>% filter(skillname %in% critical_thinking) %>% 
  select(skillname, `Normalized Index`)

crit_think_plot <- app_crit_think %>%
  ggplot(aes(x = `Normalized Index`, y = reorder(skillname, `Normalized Index`))) + 
  geom_col(fill = "coral") + labs(x = "Index", y = "Skill", title = "Critical Thinking Skills of the Future") +
  theme_minimal() + scale_x_continuous(expand = c(0,0), limits = c(0, max(app_crit_think$`Normalized Index`)))

app_communication <- skills_future %>%
  filter(skillname %in% communication) %>% 
  select(skillname, `Normalized Index`)

communication_plot <- app_communication %>%
  ggplot(aes(x = `Normalized Index`, y = reorder(skillname, `Normalized Index`))) + 
  geom_col(fill = "coral") + labs(x = "Index", y = "Skill", title = "Communication Skills of the Future") +
  theme_minimal() + scale_x_continuous(expand = c(0,0), limits = c(0, max(app_communication$`Normalized Index`)))

app_labor <- skills_future %>% 
  filter(skillname %in% labor) %>% 
  select(skillname, `Normalized Index`)


# Create plots

labor_plot <- app_labor %>%
  ggplot(aes(x = `Normalized Index`, y = reorder(skillname, `Normalized Index`))) + 
  geom_col(fill = "coral") + labs(x = "Index", y = "Skill", title = "Labor Skills of the Future") +
  theme_minimal() + scale_x_continuous(expand = c(0,0), limits = c(0, max(app_labor$`Normalized Index`)))

app_technology <- skills_future %>% 
  filter(skillname %in% technology) %>% 
  select(skillname, `Normalized Index`)

tech_plot <- app_technology %>%
  ggplot(aes(x = `Normalized Index`, y = reorder(skillname, `Normalized Index`))) + 
  geom_col(fill = "coral") + labs(x = "Index", y = "Skill", title = "Appalchian Technology Skills") +
  theme_minimal() + scale_x_continuous(expand = c(0,0), limits = c(0, max(app_technology$`Normalized Index`)))

app_organization <- skills_future %>% 
  filter(skillname %in% organization) %>% 
  select(skillname, `Normalized Index`)

organization_plot <- app_organization %>%
  ggplot(aes(x = `Normalized Index`, y = reorder(skillname, `Normalized Index`))) + 
  geom_col(fill = "coral") + labs(x = "Index", y = "Skill", title = "Organization Skills of the Future") +
  theme_minimal() + scale_x_continuous(expand = c(0,0), limits = c(0,max(app_organization$`Normalized Index`)))

app_skills_plots <- list(crit_think_plot,
                         communication_plot,
                         labor_plot, tech_plot,
                         organization_plot)
layout <- rbind(c(1, 2), c(3, 4), c(5))
# View plots and skills of the future to focus on 
grid.arrange(grobs = app_skills_plots, layout_matrix = layout)

## Map indices ------------------------------------------------------------------

# Obtain polygons to map onto leaflet for Appalachian PUMAs 
counties<-read.csv("ALMV_counties_all.csv", header=T) %>%
  rename(state_code=State)%>%
  mutate(County=str_replace_all(County,"\'|\\.",""))%>%
  mutate(County=str_trim(County, side="both"))
counties$state_code=as.character(counties$state_code)
state_list<-unique(counties$state_code)
state_list[1] <- "01"
options(tigris_use_cache = TRUE)
puma_geoms_list <- lapply(state_list, function(x) {
  pumas(state = x, cb = T)
})

puma_geoms <- rbind_tigris(puma_geoms_list)
puma_geoms <- puma_geoms %>% unite(STATEFP10, PUMACE10, col = "PUMA", sep = "")

app_pumas <- as_tibble(unique(app_ipums$PUMA)) %>% rename(PUMA = value)
puma_app_geoms <- semi_join(as.data.frame(puma_geoms), app_pumas)

map_data <- left_join(app_weighted_skills_by_PUMA, puma_app_geoms) %>% 
  select(skillname, PUMA, `Normalized Index`, geometry, NAME10)

### Map for technology-----------------------------------------------------

TechDesign_map_data <- map_data %>% filter(skillname == "TechnologyDesign")

TechDesign_map_data <- st_as_sf(TechDesign_map_data) 

TechDesign_map_pal <- colorNumeric(palette = "viridis", domain = TechDesign_map_data$`Normalized Index`)

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
### Map for Critical Thinking ------------------------------------------
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

### Map for Organization ------------------------------------------
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

### Map for Labor----------------------------------------------------
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

### Map for Communication ----------------------------------------------------
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

# ACS Demographics-----------------------------------------------------

## Loading in Data--------------------------------------------------------------

# Create list of FIPS state and county

counties<-read.csv("All_Mining.csv", header=T)

#Note Yellowstone National Park did not have ACS data so drop it.
counties <- filter(counties,FIPS!= 30113)

#only keep Appalachia
counties <- filter(counties, ï..coal_region== 1)

fip_list<-sprintf("%05d",counties$FIPS)
fips  <- data.frame(fip_list)
state_list<-unique(sprintf("%02d", counties$state_code))
states <- data.frame(state_list)

# Create dataframe with all desired variables 

almv_acs_var <- function(varcode){
  get_acs(geography="county",
          state=state_list,
          variables =varcode,
          year=2019, 
          survey = "acs5",
          cache_table = TRUE,
          geometry = F) %>%
    filter(GEOID %in% fip_list) %>%
    select(NAME, estimate)
}

#pull only variable of interest at the county level
disability <- almv_acs_var("S1810_C03_001") %>%  rename(Pct.Dis=estimate)
HI_insured <-  almv_acs_var("S2701_C03_001") %>%  rename(Pct.HI=estimate)
Unemployment <- almv_acs_var("S2301_C04_001") %>%  rename(Pct.Unemp=estimate)

#Make housing percent for ownhome
Housing  <- almv_acs_var("S2502_C04_001") %>%  rename(Housing.OwnOcc=estimate)
Housing2  <- almv_acs_var("S2502_C01_001") %>%  rename(Housing.Total=estimate)
housing<- Reduce(function(x, y) merge(x, y, all=TRUE), list(Housing,Housing2))
housing$OwnHome <- round(100* housing$Housing.OwnOcc/housing$Housing.Total)
#keep to bind
dff000 = subset(housing, select = c(NAME,OwnHome))

#Use these numbers to calculate estimate
SCounty.Pop <- almv_acs_var("B15003_001")  %>%  rename(SCountyPop2=estimate)
Ed0 <- almv_acs_var("B15003_025")  %>%  rename(Ed.PhD=estimate)
Ed1 <- almv_acs_var("B15003_024")  %>%  rename(Ed.Prof=estimate)
Ed2 <- almv_acs_var("B15003_023")  %>%  rename(Ed.Mast=estimate)
Ed3 <- almv_acs_var("B15003_022")  %>%  rename(Ed.Bach=estimate)
Ed4 <- almv_acs_var("B15003_021")  %>%  rename(Ed.Assoc=estimate)
Ed5 <- almv_acs_var("B15003_020")  %>%  rename(Ed.SColl=estimate)
Ed6 <- almv_acs_var("B15003_019")  %>%  rename(Ed.SCollLT1=estimate)
Ed7 <- almv_acs_var("B15003_018")  %>%  rename(Ed.GED=estimate)
Ed8 <- almv_acs_var("B15003_017")  %>%  rename(Ed.HSDip=estimate)
Ed9 <- almv_acs_var("B15003_016")  %>%  rename(Ed.HSNoDip=estimate)

# Education
Education<- Reduce(function(x, y) merge(x, y, all=TRUE), list(Ed0,Ed1,Ed2,Ed3,Ed4,Ed5,Ed6,Ed7,Ed8,Ed9, SCounty.Pop))
Education$Below11 <- Education$SCountyPop2 - (Education$Ed.HSNoDip +Education$Ed.GED+Education$Ed.HSDip+Education$Ed.SColl+Education$Ed.Assoc+Education$Ed.SCollLT1+Education$Ed.Bach+Education$Ed.Prof+Education$Ed.Mast+Education$Ed.PhD)
Education$LT_HS <- round(100*(Education$Ed.HSNoDip+Education$Below11)/Education$SCountyPop2)
Education$HS_Dip <- round((100/(Education$SCountyPop2))*(Education$Ed.GED+Education$Ed.HSDip))
Education$SomeColl<-round(100*(Education$Ed.SColl+Education$Ed.Assoc+Education$Ed.SCollLT1)/Education$SCountyPop2)
Education$Coll_Plus <- round((100/Education$SCountyPop2)*(Education$Ed.Bach+Education$Ed.Prof+Education$Ed.Mast+Education$Ed.PhD))


#keep to bind
dff00 = subset(Education, select = c(NAME,LT_HS, HS_Dip, SomeColl, Coll_Plus,SCountyPop2))

CountyHH <- almv_acs_var("S2801_C01_001") %>%  rename(County.HH=estimate)
HHCompdev <- almv_acs_var("S2801_C02_002") %>%  rename(HH.Pct.compdev=estimate)
HHComputer <- almv_acs_var("S2801_C02_003") %>%  rename(HH.Pct.computer=estimate)
HHInternet <- almv_acs_var("S2801_C02_012") %>%  rename(HH.Pct.internet=estimate)
HH_BB <- almv_acs_var("S2801_C02_014") %>%  rename(HH.Pct.BroadBand=estimate)
HH_NoInternet <- almv_acs_var("S2801_C02_019") %>%  rename(HH.Pct.Nointernet=estimate)

CountyMedianAge <- almv_acs_var("S0101_C01_032") %>%  rename(County.MedAge=estimate) 
CountyMedianInc <- almv_acs_var("S1901_C01_013") %>%  rename(County.MedInc=estimate) 

#Use these numbers to calculate estimate
Age1  <- almv_acs_var("S0101_C02_002") %>%  rename(Pct.Under5=estimate) 
Age2  <- almv_acs_var("S0101_C02_003") %>%  rename(Pct.Bet5_9=estimate) 
Age3  <- almv_acs_var("S0101_C02_004") %>%  rename(Pct.Bet10_14=estimate) 
Age4  <- almv_acs_var("S0101_C02_005") %>%  rename(Pct.Bet15_19=estimate) 
Age5  <- almv_acs_var("S0101_C02_006") %>%  rename(Pct.Bet20_24=estimate) 
Age6  <- almv_acs_var("S0101_C02_007") %>%  rename(Pct.Bet25_29=estimate) 
Age7  <- almv_acs_var("S0101_C02_008") %>%  rename(Pct.Bet30_34=estimate) 
Age8  <- almv_acs_var("S0101_C02_009") %>%  rename(Pct.Bet35_39=estimate) 
Age9  <- almv_acs_var("S0101_C02_010") %>%  rename(Pct.Bet40_44=estimate) 
Age10  <- almv_acs_var("S0101_C02_011") %>%  rename(Pct.Bet45_49=estimate) 
Age11  <- almv_acs_var("S0101_C02_012") %>%  rename(Pct.Bet50_54=estimate) 
Age12  <- almv_acs_var("S0101_C02_013") %>%  rename(Pct.Bet55_59=estimate) 
Age13  <- almv_acs_var("S0101_C02_014") %>%  rename(Pct.Bet60_64=estimate) 
Age14  <- almv_acs_var("S0101_C02_015") %>%  rename(Pct.Bet65_69=estimate) 

#Calculate Age.70plus by subtracting all other variables from 100 when you do percent
age <- Reduce(function(x, y) merge(x, y, all=TRUE), list(Age1,Age2,Age3,Age4,Age5,Age6,Age7,Age8,Age9,Age10,Age11,Age12,Age13,Age14))

age$age0_14<-age$Pct.Under5+age$Pct.Bet5_9+age$Pct.Bet10_14

age$age15_64<-(age$Pct.Bet15_19+age$Pct.Bet20_24+age$Pct.Bet25_29+age$Pct.Bet30_34+age$Pct.Bet35_39+age$Pct.Bet40_44+age$Pct.Bet45_49+age$Pct.Bet50_54+age$Pct.Bet55_59+age$Pct.Bet60_64)

age$age65plus<-100-age$age0_14-age$age15_64

#keep to bind
dff0 = subset(age, select = c(NAME,age0_14, age15_64, age65plus) )

#LT20K_NoInternet<- almv_acs_var("S2801_C02_023") %>%  rename(LT20K.Pct.Nointernet=estimate)
#LT2075K_NoInternet<- almv_acs_var("S2801_C02_027") %>%  rename(LT2075K.Pct.Nointernet=estimate)
#GT75K_NoInternet<- almv_acs_var("S2801_C02_031") %>%  rename(GT75K.Pct.Nointernet=estimate)

#Employment by industry. Use these numbers to calculate  percent
TotEmp <- almv_acs_var("S2404_C01_001") %>%  rename(TotEmp=estimate)
IndEmp1 <- almv_acs_var("S2404_C01_003") %>%  rename(AgEmp=estimate)
IndEmp2 <- almv_acs_var("S2404_C01_004") %>%  rename(MiningEmp=estimate)
IndEmp3 <- almv_acs_var("S2404_C01_005") %>%  rename(ConstrEmp=estimate)
IndEmp4 <- almv_acs_var("S2404_C01_006") %>%  rename( ManufEmp=estimate)
IndEmp5 <- almv_acs_var("S2404_C01_007") %>%  rename( WholeEmp=estimate)
IndEmp6 <- almv_acs_var("S2404_C01_008") %>%  rename( RetailEmp=estimate)
IndEmp7 <- almv_acs_var("S2404_C01_009") %>%  rename( TranWhUtilEmp=estimate)
IndEmp8 <- almv_acs_var("S2404_C01_012") %>%  rename( InfoEmp=estimate)
IndEmp9 <- almv_acs_var("S2404_C01_013") %>%  rename( FnInRERntEmp=estimate)
IndEmp10 <- almv_acs_var("S2404_C01_016") %>%  rename( ProfSciMngAdEmp=estimate)
IndEmp11 <- almv_acs_var("S2404_C01_021") %>%  rename( EdEmp=estimate)
IndEmp12 <- almv_acs_var("S2404_C01_022") %>%  rename( HCSAEmp=estimate)
IndEmp13 <- almv_acs_var("S2404_C01_023") %>%  rename( ArtEntRecAccFdEmp=estimate)

industry <- Reduce(function(x, y) merge(x, y, all=TRUE), list(TotEmp,IndEmp1,IndEmp2,IndEmp3,IndEmp4,IndEmp5,IndEmp6,IndEmp7,IndEmp8,IndEmp9,IndEmp10,IndEmp11,IndEmp12,IndEmp13))

#create percents in each industry at the county
industry$I1 <-round(100* industry$AgEmp/industry$TotEmp)
industry$I2<-round(100*(industry$MiningEmp)/industry$TotEmp)
industry$I3<-round(100*(industry$ConstrEmp)/industry$TotEmp)
industry$I4<-round(100*(industry$ManufEmp)/industry$TotEmp)
industry$I5<-round(100*(industry$WholeEmp)/industry$TotEmp)
industry$I6<-round(100*(industry$RetailEmp)/industry$TotEmp)
industry$I7<-round(100*(industry$TranWhUtilEmp)/industry$TotEmp)
industry$I8<-round(100*(industry$InfoEmp)/industry$TotEmp)
industry$I9<-round(100*(industry$FnInRERntEmp)/industry$TotEmp)
industry$I10<-round(100*(industry$ProfSciMngAdEmp)/industry$TotEmp)
industry$I11<-round(100*(industry$EdEmp)/industry$TotEmp)
industry$I12<-round(100*(industry$HCSAEmp)/industry$TotEmp)
industry$I13<-round(100*(industry$ArtEntRecAccFdEmp)/industry$TotEmp)

#keep to bind
dff1 = subset(industry, select = c(NAME,I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I13) )

#travel time to work. #Use these numbers to calculate your own percent
t0 <- almv_acs_var("B08303_001") %>%  rename(tcat0=estimate)
t1 <- almv_acs_var("B08303_002") %>%  rename(tcat1=estimate)
t2 <- almv_acs_var("B08303_003") %>%  rename(tcat2=estimate)
t3 <- almv_acs_var("B08303_004") %>%  rename(tcat3=estimate)

t4 <- almv_acs_var("B08303_005") %>%  rename(tcat4=estimate)
t5 <- almv_acs_var("B08303_006") %>%  rename(tcat5=estimate)
t6 <- almv_acs_var("B08303_007") %>%  rename(tcat6=estimate)

t7 <- almv_acs_var("B08303_008") %>%  rename(tcat7=estimate)
t8 <- almv_acs_var("B08303_009") %>%  rename(tcat8=estimate)
t9 <- almv_acs_var("B08303_010") %>%  rename(tcat9=estimate)

t10 <- almv_acs_var("B08303_011") %>%  rename(tcat10=estimate)
t11 <- almv_acs_var("B08303_012") %>%  rename(tcat11=estimate)
t12 <- almv_acs_var("B08303_013") %>%  rename(tcat12=estimate)

travel <- Reduce(function(x, y) merge(x, y, all=TRUE), list(t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12))
travel$tt014<-round((travel$tcat1 + travel$tcat2 + travel$tcat3)/travel$tcat0)
travel$tt1529<-round((travel$tcat4 + travel$tcat5 + travel$tcat6)/travel$tcat0)
travel$tt3044<-round((travel$tcat7 + travel$tcat8 + travel$tcat9)/travel$tcat0)
travel$tt4559<-round(travel$tcat10/travel$tcat0)
travel$tt60plus<- round((travel$tcat11 + travel$tcat12)/travel$tcat0)
dff2 = subset(travel, select = c(NAME,tt014,tt1529,tt3044,tt4559,tt60plus) )
#Keep dff2 dataframe to bind below.


almv_acs_table <- function(varcode){
  get_acs(geography="county",
          state=state_list, cache_table = TRUE, geometry = TRUE,
          table = varcode, 
          year=2019, survey = "acs5") %>%
    filter(GEOID %in% fip_list)  %>%    select(GEOID, NAME, estimate, geometry)
  
}

#Call the function to pull data from ACS
PerCapita <- almv_acs_table("B19301") %>%  rename(PerCapInc=estimate)
#write.csv(PerCapita$GEOID,'PerCapita.csv')

all_ACS5 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(PerCapita,dff000, dff00, dff0, dff1, dff2, disability, HI_insured, Unemployment, HHCompdev, HHComputer, HHInternet, HH_BB, HH_NoInternet, CountyMedianAge, CountyMedianInc))

# Adding the Urban-Rural Classification from USDA
urb_ru <- read.csv("ruralurbancodes2013.csv", header=T)

#add in a leading zero and make it into a character format 
urb_ru$GEOID <- sprintf("%05d",urb_ru$ï..FIPS)

#merge in dataset & keep only Appalachia
appal <-Reduce(function(x, y) merge(x, y, all=TRUE), list(urb_ru,all_ACS5)) %>%  filter(!is.na(NAME))

#summary(appal)
appal$age0_14 <- round(appal$age0_14)
appal$age15_64 <- round(appal$age15_64)
appal$age65plus <- round(appal$age65plus)
#create a new variable nonmetro for nonmetro counties
appal$nonmetro <- ifelse(appal$RUCC_2013>=4,1,0)
levels(appal$nonmetro)
levels(appal$nonmetro) <- c("Nonmetro", "Metro")

#Create a factor variable for rural
appal$nonmetro.f <- factor(appal$nonmetro, labels = c("Metro", "Nonmetro"))
#is.factor(appal$nonmetro.f)

appal2 <- appal[order(appal$GEOID),]
appal2$observation <- 1:nrow(appal2) 

#group_means <- appal2 %>% group_by(nonmetro) %>%
# summarise(mean = mean(age65plus)) 


d <- appal2 %>%
  group_by(nonmetro.f) %>%
  summarise_at(vars(age0_14, age15_64, age65plus, LT_HS, HS_Dip, SomeColl,Coll_Plus, OwnHome, Pct.HI, Pct.Unemp, Pct.Dis, PerCapInc, tt014,tt1529,tt3044,tt4559,tt60plus), funs(mean(., na.rm=TRUE)))

vars <- names(d)
library(gdata)
g <- rename.vars(d, from=vars, to=paste0("M_", vars))
g$nonmetro.f<- g$M_nonmetro.f

appal2 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(appal2,g))  

# Write appal2 and g to RDS

# saveRDS(appal2, "data/appal2.RDS") 
# saveRDS(g, "data/g.RDS")

load("ShinyApp/data/ACS_Objects.RData")
appal2 <- readRDS("ShinyApp/data/appal2.RDS")
g <- readRDS("ShinyApp/data/g.RDS")
industry <- readRDS("ShinyApp/data/industry.Rds")


## Creating plots of ACS Data----------------------------------------------------

### Chart for Unemployment  ----------------------------------------------------
unemployed <- ggplotly(ggplot(data = appal2, aes(x = observation,
                                                 y = Pct.Unemp, 
                                                 colour = nonmetro.f, 
                                                 names=NAME, text = str_c(NAME,": ", Pct.Unemp))) + 
                         geom_point() +  geom_hline(data = g, aes(yintercept=M_Pct.Unemp, color="black")) + 
                         facet_wrap( nonmetro.f~.)  + 
                         theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),
                                           axis.title.x = element_text(color="black", size=8, face="bold"),
                                           axis.title.y = element_text(color="black", size=10, face="bold")) +
                         xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') + ggtitle("% of Population: Unemployed") + scale_color_viridis_d(), tooltip = "text")
unemployed


### Chart for Per Capita Income----------------------------------------------------

PerCapitaIncome <- ggplotly(ggplot(data = appal2, aes(x = observation, y = PerCapInc, colour = nonmetro.f, names=NAME, text = str_c(NAME,": $", format(PerCapInc, big.mark = ",", scientific = F)))) + geom_point()  +  
                              geom_hline(data = g, aes(yintercept=M_PerCapInc, color="black")) + facet_wrap( nonmetro.f~.)  +
                              theme_bw()+ theme(axis.text.x = element_blank(), 
                                                legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),
                                                axis.title.x = element_text(color="black", size=8, face="bold"),
                                                axis.title.y = element_text(color="black", size=10, face="bold")) +
                              scale_y_continuous(labels = scales::dollar_format()) +
                              xlab("County") + ylab("Income") + labs(color='County Classification') + 
                              ggtitle("Per Capita Income") + scale_color_viridis_d(), tooltip = "text")

PerCapitaIncome

### Age Charts ----------------------------------------------------
#### Under 15----------------------------------------------------
AgeUnder15 <- ggplotly(ggplot(data = appal2, aes(x = observation, y = age0_14, colour = nonmetro.f, names=NAME, text = str_c(NAME,": ", age0_14))) + 
                         geom_point()  +  
                         geom_hline(data = g, aes(yintercept=M_age0_14, color= "black")) + 
                         facet_wrap( nonmetro.f~.)  + theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),  
                                                                        axis.title.x = element_text(color="black", size=8, face="bold"), 
                                                                        axis.title.y = element_text(color="black", size=10, face="bold")) +
                         xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') + 
                         ggtitle("% of Population: Age 0-14") +
                         scale_color_viridis_d(), tooltip = "text")
AgeUnder15

#### 15 to 64----------------------------------------------------
Age15_64<- p2 <- ggplotly(ggplot(data = appal2, aes(x = observation, y = age15_64, colour = nonmetro.f, names=NAME, text = str_c(NAME,": ", age15_64))) + geom_point() +  
                            geom_hline(data = g, aes(yintercept=M_age15_64, color= "black")) + facet_wrap( nonmetro.f~.)  + 
                            theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),                                                                                                                                                                                                                                               axis.title.y = element_text(color="black", size=10, face="bold")) +
                            xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') +
                            ggtitle("% of Population: Age 15-64") +
                            scale_color_viridis_d(), tooltip = "text")
Age15_64

#### 65 Plus ---------------------------------------------------- 
Age65Plus <- ggplotly(ggplot(data = appal2, aes(x = observation, y = age65plus, colour = nonmetro.f, names=NAME, text = str_c(NAME,": ", age15_64))) + geom_point()  +  
                        geom_hline(data = g, aes(yintercept=M_age65plus, color="black")) + facet_wrap( nonmetro.f~.)  + 
                        theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),
                                          axis.title.x = element_text(color="black", size=8, face="bold"),
                                          axis.title.y = element_text(color="black", size=10, face="bold")) +
                        xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') +
                        ggtitle("% of Population: Age 65+") + scale_color_viridis_d(), tooltip = "text")


Age65Plus


## Arrange 
subplot(AgeUnder15, Age15_64, Age65Plus, nrows = 3,  shareY=FALSE, titleX = TRUE, titleY=TRUE)


### Education Charts ----------------------------------------------------

#### LT HS ----------------------------------------------------
EducationLTHS <- ggplotly(ggplot(data = appal2, aes(x = observation, y = LT_HS, colour = nonmetro.f, names=NAME, text = str_c(NAME, ": ", LT_HS))) + 
                            geom_point()  +  
                            geom_hline(data = g, aes(yintercept=M_LT_HS, color= "black")) + 
                            facet_wrap( nonmetro.f~.)  + 
                            theme_bw()+ 
                            theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),                                                                                                                                                                                                                             axis.title.y = element_text(color="black", size=10, face="bold")) +
                            xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') + 
                            ggtitle("% of Population: Less Than High School") + scale_color_viridis_d(), tooltip = "text")
EducationLTHS

#### HS Diploma ----------------------------------------------------
EducationHSDP <- ggplotly(ggplot(data = appal2, aes(x = observation, y = HS_Dip, colour = nonmetro.f, names=NAME, text = str_c(NAME, ": ", HS_Dip))) + 
                            geom_point()  + 
                            geom_hline(data = g, aes(yintercept=M_HS_Dip, color="black")) + 
                            facet_wrap( nonmetro.f~.)  + 
                            theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),                                                                                                                                                                                                                                 axis.title.y = element_text(color="black", size=10, face="bold")) +
                            xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') + 
                            ggtitle("% of Population: HS Dip") + scale_color_viridis_d(), tooltip = "text")
EducationHSDP


#### College and above ----------------------------------------------------
EducationCollPlus <- ggplotly(ggplot(data = appal2, aes(x = observation, y = Coll_Plus, colour = nonmetro.f, names=NAME, text = str_c(NAME, ": ", Coll_Plus))) + geom_point()  + 
                                geom_hline(data = g, aes(yintercept=M_Coll_Plus, color= "black")) + 
                                facet_wrap( nonmetro.f~.)  + 
                                theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),                                                                                                                                                                                                                                          axis.title.y = element_text(color="black", size=10, face="bold")) +
                                xlab("County") + ylab("Percent in County (%)") + 
                                labs(color='County Classification') + 
                                ggtitle("% of Population: College or More") + scale_colour_viridis_d(), tooltip = "text")

EducationCollPlus

### Home Ownership----------------------------------------------------
HomeOwnership <- ggplotly(ggplot(data = appal2, aes(x = observation, y = OwnHome, colour = nonmetro.f, names=NAME, text = str_c(NAME, ": ", observation))) + 
                            geom_point()  +  geom_hline(data = g, aes(yintercept=M_OwnHome, color= "black")) + 
                            facet_wrap( nonmetro.f~.)  + theme_bw()+ 
                            theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),                                                                                                                                                                                                                                            axis.title.y = element_text(color="black", size=10, face="bold")) +
                            xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') + 
                            ggtitle("% of Population: Owns a Home") + scale_color_viridis_d(), tooltip = "text")
HomeOwnership

### Disability & Health Insurance ----------------------------------------------------
Disabled <- ggplotly(ggplot(data = appal2, aes(x = observation, y = Pct.Dis, colour = nonmetro.f, names=NAME, text = str_c (NAME, ": ", Pct.Dis))) +
                       geom_point()  +  
                       geom_hline(data = g, aes(yintercept=M_Pct.Dis, color="black")) + facet_wrap( nonmetro.f~.)  + 
                       theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),                                                                                                                                                                                                                                   axis.title.y = element_text(color="black", size=10, face="bold")) +
                       xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') + 
                       ggtitle("% of Population: Disability") + scale_colour_viridis_d(), tooltip = "text")

Disabled

HealthInsurance <- ggplotly(ggplot(data = appal2, aes(x = observation, y = Pct.HI, colour = nonmetro.f, names=NAME, text = str_c(NAME, ": ", Pct.HI))) + 
                              geom_point()  +  
                              geom_hline(data = g, aes(yintercept=M_Pct.HI, color= "black")) + facet_wrap( nonmetro.f~.)  + 
                              theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),                                                                                                                                                                                                                                             axis.title.y = element_text(color="black", size=10, face="bold")) +
                              xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') + 
                              ggtitle("% of Population: Health Insurance Coverage") + scale_color_viridis_d(), tooltip = "text")

HealthInsurance




## Industry Data  ----------------------------------------------------



industry <- appal2[c(1,2,9,20:32)] %>% 
  pivot_longer(cols = c(I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13),
               names_to = "Industry",
               values_to = "Percent") %>% 
  mutate(PercentOfTotal= Percent/420) 

industry$Industry[industry$Industry == "I1"] <- "Agriculture"
industry$Industry[industry$Industry == "I2"] <- "Mining"
industry$Industry[industry$Industry == "I3"] <- "Construction"
industry$Industry[industry$Industry == "I4"] <- "Manufacturing"
industry$Industry[industry$Industry == "I5"] <- "Wholesale Trade"
industry$Industry[industry$Industry == "I6"] <- "Retail"
industry$Industry[industry$Industry == "I7"] <- "Logistics and Utilities"
industry$Industry[industry$Industry == "I8"] <- "Information"
industry$Industry[industry$Industry == "I9"] <- "Finance and Real Estate"
industry$Industry[industry$Industry == "I10"] <- "Professional"
industry$Industry[industry$Industry == "I11"] <- "Education"
industry$Industry[industry$Industry == "I12"] <- "Healthcare"
industry$Industry[industry$Industry == "I13"] <- "Entertainment"

View(industry)

# SaveRDS
# saveRDS(industry, "ShinyApp/data/industry.Rds")

### Graph ----------------------------------------------------
industry <- readRDS("ShinyApp/data/industry.Rds")
industry_composition <- ggplot(data = industry, aes(x = Industry, 
                                                    y = PercentOfTotal, 
                                                    group = nonmetro.f, 
                                                    fill = nonmetro.f)) +
  geom_col() + theme_bw()+ theme(plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),                                                                                                                                                                                                                                             axis.title.y = element_text(color="black", size=10, face="bold")) +
  xlab("Industry") + ylab("Percent in Industry (%)") + labs(color='County Classification') + 
  ggtitle("% of Industry") + scale_fill_viridis_d(name = element_blank()) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,20)) + 
  coord_flip() 

industry_composition
