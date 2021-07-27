library(ipumsr)
library(NLP)
library(tm)
library(SnowballC)
library(wordcloud)
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


# Process of obtaining 2019 IPUMS info for Appalachia--------------------------
# #Read in IPUMS data and PUMAs for Appalachia
# ddi <- read_ipums_ddi("usa_00007.xml")
# 
# data <- read_ipums_micro(ddi)
# app_pumas_2010 <- read_csv("2010_PUMAs_App.csv")
# app_pumas_2010 <- app_pumas_2010 %>%
#   rename(PUMA = PUMACE10, STATEFIP = STATEFP10) %>%
#   mutate(PUMA = as.numeric(PUMA), STATEFIP = as.numeric(STATEFIP))

# semi-join to obtain only appalachian IPUMS info
# app_ipums <-
#   semi_join(data, app_pumas_2010)


# Filter out unemployed and exchange OCCSOC's
## that have X's or Y's for 9's
# app_ipums <- app_ipums %>% filter(OCCSOC > 0)
# app_ipums <- app_ipums %>%
#   mutate(OCCSOC = str_replace_all(OCCSOC, "XXX", "199")) %>%
#   mutate(OCCSOC = str_replace_all(OCCSOC, "XX", "99")) %>%
#   mutate(OCCSOC = str_replace_all(OCCSOC, "X", "9")) %>%
#   mutate(OCCSOC = str_replace_all(OCCSOC, "Y", "9"))
# 
# app_ipums <- app_ipums %>% mutate(STATEFIP = as.character(STATEFIP), PUMA = as.character(PUMA)) %>%
#   mutate(STATEFIP = if_else(str_count(STATEFIP) == 1, str_c("0", STATEFIP), STATEFIP)) %>% 
#   mutate(PUMA = if_else(
#     nchar(PUMA) == 3, paste0("00", PUMA), PUMA)) %>% 
#   mutate(PUMA = if_else(nchar(PUMA) == 4, paste0("0", PUMA), PUMA))%>% 
#   mutate(PERWT = as.numeric(PERWT)) %>% 
#   unite(STATEFIP, PUMA, col = "PUMA", sep = "")

# write_csv(app_ipums, "2019-Appalachian_IPUMS.csv")
# SOC Frequency Counts -----------------------------------------------------------

# read in ipums data for Appalachia in 2019
app_ipums <- read_csv("2019-Appalachian_IPUMS.csv")

#Read in IPUMS data and PUMAs for Appalachia

ddi <- read_ipums_ddi("usa_00008.xml")

data <- read_ipums_micro(ddi)
app_pumas_2010 <- read_csv("2010_PUMAs_App.csv")
app_pumas_2010 <- app_pumas_2010 %>% 
  rename(PUMA = PUMACE10, STATEFIP = STATEFP10) %>% 
  mutate(PUMA = as.numeric(PUMA), STATEFIP = as.numeric(STATEFIP))
app_ipums <- 
  semi_join(data, app_pumas_2010)


# Filter out unemployed and exchange OCCSOC's 
## that have X's for 9's
app_ipums <- app_ipums %>% filter(OCCSOC > 0)
app_ipums <- app_ipums %>% 
  mutate(OCCSOC = str_replace_all(OCCSOC, "XXX", "199")) %>% 
  mutate(OCCSOC = str_replace_all(OCCSOC, "XX", "99")) %>% 
  mutate(OCCSOC = str_replace_all(OCCSOC, "X", "9")) %>% 
  mutate(OCCSOC = str_replace_all(OCCSOC, "YY", "99")) 


# Filter for appalachian areas in individual states
va_ipums <- app_ipums %>% filter(STATEFIP == 51)
ky_ipums <- app_ipums %>% filter(STATEFIP == 21)
wv_ipums <- app_ipums %>% filter(STATEFIP == 54)

#Add column for frequency of SOC code (socamt)
app_ipums <- app_ipums %>%  group_by(OCCSOC) %>% mutate(socamt = n())



#Create tibble of soc's with their associated frequencies in Appalachia 
socfreq <- app_ipums %>%
  group_by(OCCSOC) %>%
  summarise(socfreq = sum(PERWT)) %>%
  rename(soc = OCCSOC) %>% ungroup()
View(socfreq)

# Create tibble of soc's with their associated frequencies in all PUMAs
# by PUMA
socfreq_by_puma <- app_ipums %>% 
  group_by(PUMA, OCCSOC) %>% 
  summarise(socfreq = sum(PERWT)) %>% 
  pivot_wider(names_from = PUMA, values_from = socfreq) %>% 
  rename(soc = OCCSOC) %>% ungroup()
View(socfreq_by_puma)


# Skills -----------------------------------------------------------------------

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


View(skills)

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
<<<<<<< HEAD
  distinct() %>% ungroup()
=======
  distinct()
>>>>>>> 7639b7ceb87a5ceaf764b2999b1f4d30111d3a18

View(skills_wide)
normalize <- skills_wide %>% mutate(Importance = Importance/5) %>% mutate(Level = Level/6.12)
View(normalize)
ggplot(normalize,aes(x=Importance,y = Level)) + geom_point() + geom_smooth(method = lm)
skilllevelreg <- lm(normalize$Importance ~ normalize$Level)
summary(skilllevelreg)
# Change soc's to match skills info, making socs not in o*net 
# end in 1 rather than 0. 

## Find soc's only in socfreq
skills_wide$soc <- as.numeric(skills_wide$soc)

socs_na <- anti_join(socfreq, skills_wide)

# Do the same by PUMA 
socs_na_by_PUMA <- anti_join(socfreq_by_puma, skills_wide) %>% 
  mutate(soc = as.character(soc))



## change those soc's to end in 1 that end in 0
altered_na_socs <- socs_na %>% 
  mutate(soc = str_replace_all(soc, "00$", "99")) %>% 
  mutate(soc = str_replace_all(soc, "0$", "1")) %>% 
  select(soc, socfreq)

# Do the same by PUMA 
altered_na_socs_by_puma <- socs_na_by_PUMA %>% 
  mutate(soc = str_replace_all(soc, "00$", "99")) %>% 
  mutate(soc = str_replace_all(soc, "0$", "1")) 

## Aggregate all similar socs
soc_not_na <- semi_join(socfreq, skills_wide)

# Do the same by PUMA
soc_not_na_by_PUMA <- semi_join(socfreq_by_puma, skills_wide)

## Combine similar and altered socs into one
altered_na_socs$soc <- as.numeric(altered_na_socs$soc)
altered_socs_freq <- bind_rows(soc_not_na, altered_na_socs)

# Do the same by PUMA 
altered_na_socs_by_puma$soc <- as.numeric(altered_na_socs_by_puma$soc)
altered_socs_freq_by_puma<- bind_rows(soc_not_na_by_PUMA, altered_na_socs_by_puma)


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
# with associated soc count. 
skills_indexed_counts <- right_join(skills_indexed, altered_socs_freq)
View(skills_indexed_counts)

# Do the same by PUMA 
skills_indexed_counts_by_PUMA <- right_join(skills_indexed, altered_socs_freq_by_puma)
View(skills_indexed_counts_by_PUMA)

# Determine which soc's are null
null_socs <- skills_indexed_counts %>% filter(is.na(index))
View(null_socs)
# Do the same by PUMA
null_socs_by_PUMA <-  skills_indexed_counts_by_PUMA %>% filter(is.na(index))
View(null_socs_by_PUMA)

# Create an indexed counts with only soc's in common to those in Appalachia
skills_index_common <- inner_join(skills_indexed, altered_socs_freq)
View(skills_index_common)

# Do the same by PUMA
skills_index_common_by_PUMA <- inner_join(skills_indexed, altered_socs_freq_by_puma)
View(skills_index_common_by_PUMA)



#Create weighted index of skills in Appalachian Labor Market
skills_index_common <- mutate(skills_index_common, weighted = index*socfreq)


app_weighted_skills <- skills_index_common %>% 
  group_by(skillname) %>% 
  summarize(skillweight = sum(weighted)) %>%
  mutate(normalized = skillweight / max(skillweight))
View(app_weighted_skills)

# Do the same by PUMA 

skills_index_common_by_PUMA_weighted <- skills_index_common_by_PUMA %>% 
  mutate(across(4:232, function(x) index * x))


app_weighted_skills_by_PUMA <- skills_index_common_by_PUMA_weighted %>% 
  mutate(across(4:232, ~replace_na(.x, 0))) %>% 
  group_by(skillname) %>% 
  summarize(across(4:231, sum)) %>% 
  mutate(across(2:229, function(x) x / max(x))) %>% 
  pivot_longer(cols = 2:229, names_to = "PUMA", values_to = "Normalized Index")

# Find range 
range <- app_weighted_skills_by_PUMA %>% 
  group_by(skillname) %>% 
  summarise(range = max(`Normalized Index`) - min(`Normalized Index`))
View(range)

# Map OperationandControl by PUMA
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

# Map for technology-----------------------------------------------------

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


# Create NAICS info to leaflets--------------------------------------------------------

# List variables needed for Industry summary
# total_workers_var <- "C24050_001"
  # industry_varspt1 <- paste0("C24050_00", 2:9)
# industry_varspt2 <- paste0("C24050_0", 10:14)
# industry_vars <- append(industry_varspt1, industry_varspt2)

# Pull data from ACS
# industry_breakdown <- get_acs(geography = "public use microdata area", 
#         state = state_list, 
#         variables = industry_vars,
#         year = 2019, survey = "acs5", 
#         geometry = F, summary_var = total_workers_var)
industry_breakdown <- rename(industry_breakdown, PUMA = GEOID)

# Semi join to isolate industry breakdown to appalachian PUMAs only
# industry_breakdown_app_PUMAs <- semi_join(industry_breakdown, app_pumas)

# Rename variables 
# industry_names <- variables %>% filter(name %in% industry_vars) 
# industry_names <- str_remove(industry_names$label, "Estimate!!Total:!!") 
# levels(industry_breakdown_app_PUMAs$variable) <- industry_names
# mapping <- setNames(industry_vars, industry_names)
# args <- c(list(industry_breakdown_app_PUMAs$variable), mapping)
# industry_breakdown_app_PUMAs$variable <- do.call(fct_recode, args)

# Create relative frequency variable 
# industry_breakdown_app_PUMAs <- industry_breakdown_app_PUMAs %>% 
#   mutate(relfreq = estimate / summary_est)

# Write to CSV for later use 
# write_csv(industry_breakdown_app_PUMAs, file = "NAICs_by_PUMA_2019.csv")

# Create ggplot piecharts for each unique PUMA ------------------------------------
industry_breakdown_app_PUMAs <- read_csv("NAICs_by_PUMA_2019.csv")
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





# Plot weighted skills for Appalachia and each state of interest 
app_weighted_skills %>% ggplot() + 
  geom_col(aes(x = normalized, y = reorder(skillname, normalized)), fill = "coral") +
  labs(x = "Density Index", y = "Skillname", title = "Weighted Appalachian Skills") + 
  theme_minimal() + 
  scale_x_continuous(
    expand = c(0,0), limits = c(0, max(app_weighted_skills$normalized)))

# Group by categories 
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


# plot
app_crit_think <- app_weighted_skills %>% filter(skillname %in% critical_thinking) %>% 
  select(skillname, normalized)

crit_think_plot <- app_crit_think %>%
  ggplot(aes(x = normalized, y = reorder(skillname, normalized))) + 
  geom_col(fill = "coral") + labs(x = "Index", y = "Skill", title = "Appalchian Critical Thinking Skills") +
  theme_minimal() + scale_x_continuous(expand = c(0,0), limits = c(0, max(app_crit_think$normalized)))
  
app_communication <- app_weighted_skills %>%
  filter(skillname %in% communication) %>% 
  select(skillname, normalized)

communication_plot <- app_communication %>%
  ggplot(aes(x = normalized, y = reorder(skillname, normalized))) + 
  geom_col(fill = "coral") + labs(x = "Index", y = "Skill", title = "Appalchian Communication Skills") +
  theme_minimal() + scale_x_continuous(expand = c(0,0), limits = c(0, max(app_communication$normalized)))

app_labor <- app_weighted_skills %>% 
  filter(skillname %in% labor) %>% 
  select(skillname, normalized)



labor_plot <- app_labor %>%
  ggplot(aes(x = normalized, y = reorder(skillname, normalized))) + 
  geom_col(fill = "coral") + labs(x = "Index", y = "Skill", title = "Appalchian Labor Skills") +
  theme_minimal() + scale_x_continuous(expand = c(0,0), limits = c(0, max(app_labor$normalized)))

app_technology <- app_weighted_skills %>% 
  filter(skillname %in% technology) %>% 
  select(skillname, normalized)

tech_plot <- app_technology %>%
  ggplot(aes(x = normalized, y = reorder(skillname, normalized))) + 
  geom_col(fill = "coral") + labs(x = "Index", y = "Skill", title = "Appalchian Technology Skills") +
  theme_minimal() + scale_x_continuous(expand = c(0,0), limits = c(0, max(app_technology$normalized)))

app_organization <- app_weighted_skills %>% 
  filter(skillname %in% organization) %>% 
  select(skillname, normalized)

organization_plot <- app_organization %>%
  ggplot(aes(x = normalized, y = reorder(skillname, normalized))) + 
  geom_col(fill = "coral") + labs(x = "Index", y = "Skill", title = "Appalchian Organization Skills") +
  theme_minimal() + scale_x_continuous(expand = c(0,0), limits = c(0, max(app_organization$normalized)))

app_skills_plots <- list(crit_think_plot, 
                         communication_plot, 
                         labor_plot, tech_plot, 
                         organization_plot)
layout <- rbind(c(1, 2), c(3, 4), c(5))
grid.arrange(grobs = app_skills_plots, layout_matrix = layout)


#


# Create table having importance and level with counts 
skills_importance_level_common <- 
  inner_join(skills_wide, altered_socs_freq) 
View(skills_importance_level_common)

# Jobs of Future ---------------------------------------------------------------
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


# plot
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


# Compare 
comparison <- bind_cols(skills_future, app_weighted_skills$pctweight)
comparison <- comparison %>% mutate(diff = pctweight - ...4)

# ----------------- Curiosity---------------------------------------------------
# Index ------------------------------------------------------------------------
# Frequencies of jobs with soc and job titles
job_freq <- inner_join(altered_socs_freq, skills) %>% select(soc, Title, socfreq) %>% distinct()

# Summarize index values to determine arbitrary cutoff 
summary(skills_index_common$index)
index_boxplot <- 
  skills_index_common %>% ggplot() + 
  geom_boxplot(aes(x = index), fill = "coral") + 
  labs(title = "Summary of Index values") + theme_minimal()

# Index is currently halway between 3rd Quarter and Max. 
high_index_cutoff <- (0.78359 + 0.28971) / 2

# Create data table of soc and skill combinations with high index ratings
high_index_skills <- skills_index_common %>% filter(index > high_index_cutoff)

View(high_index_skills)

# Create counts of individuals who have high indexed skills 
high_index_skill_counts <- high_index_skills %>% 
  group_by(skillname) %>% 
  summarise(count = sum(socfreq))
View(high_index_skill_counts)

# Create data table of soc and skill combinations with low index ratings
# Note that 0.2 is about the median, .15 was chosen to obtain skills that 
# people do at least possess
low_index_skills <- skills_index_common %>% filter(index < 0.2 & index > 0.15)
View(low_index_skills)

# Create counts of individuals who have low indexed skills 
low_index_skill_counts <- low_index_skills %>% 
  group_by(skillname) %>% 
  summarise(count = sum(socfreq))
View(low_index_skill_counts)

# Impotance and Level ---------------------------------------------------------

# Create summaries of importance and level ratings to create
# arbitrary cutoffs
summary(skills_importance_level_common$Importance)
summary(skills_importance_level_common$Level)

# Alter data table to best graph
skills_long <-  
  skills_importance_level_common %>%
  pivot_longer(cols = c(Importance, Level), 
               names_to = "Index", 
               values_to = "Rating")
# Graph importance and level boxplots to better assess cutoffs 
importance_level_boxplot <- skills_long %>%
  ggplot(aes(x = Rating, fill = Index)) + 
  geom_boxplot() + 
  facet_wrap(~Index) +
  theme_bw() + 
  theme(legend.position = "none") 
# Create cutoffs (currently halfway between max and 3rd qtr.)
importance_cutoff_high<- (3.120 + 4.880 ) / 2
level_cutoff_high <- (3.250 + 5.880 )/2

# Create soc and skill combinations with high importance and level
high_importance_and_level <- skills_importance_level_common %>% 
  filter(Importance > importance_cutoff_high & Level > level_cutoff_high)

# Get counts of individuals with those skills 
high_importance_and_level_counts <- 
  high_importance_and_level %>% 
  group_by(skillname) %>% 
  summarise(count = sum(socfreq))
View(high_importance_and_level_counts)


# ---------------- stop of Austin work -----------------------------------------
<<<<<<< HEAD
# Filter to only include PUMA sfs that should roughly be in Appalachia
pumas_2010_app <- pumas_2010_app %>%
  rename(STATEFP10 = State10, PUMACE10 = PUMA10)
puma_app_geoms <- semi_join(as.data.frame(puma_geoms), pumas_2010_app)
=======

#Read in and adjust future jobs data
futurejobs <- read_excel("Rapid_Growth.xls")[-c(1:3),1]
colnames(futurejobs) <- "soc"
futurejobs <- mutate(futurejobs, soc = substr(soc,1,7))
futurejobs <- mutate(futurejobs, soc = gsub("-", "", x = soc))

#filter skills list
app_skills <- skills %>% filter(soc %in% app_ipums$OCCSOC)  %>% filter(id == "IM")
no_app_skills <- skills %>% subset(!(soc %in% app_ipums$OCCSOC))
app_future_jobs <- app_ipums %>% filter(OCCSOC %in% futurejobs$soc)
app_future_skills <- skills %>% filter(soc %in% app_future_jobs$OCCSOC)

#Assigning frequency

app_skills_freq <- left_join(app_skills, socfreq, by = "soc") %>% 
  select(soc, skillname, socfreq) 

app_skills_list <- c(rep(app_skills_freq$skillname, app_skills_freq$socfreq))




app_skills_freq %>% 
  group_by(skillname) %>% 
  summarise(total = sum(socfreq)) -> skillfreq 



#Making a word cloud function

make_a_word_cloud <- function(pdfname){
  txt_corpus <- Corpus(VectorSource(pdfname))
  txt_corpus_clean <- tm_map(txt_corpus, tolower)
  txt_corpus_clean <- tm_map(txt_corpus_clean, removePunctuation)
  txt_corpus_clean <- tm_map(txt_corpus_clean, stripWhitespace)
  txt_corpus_clean <- tm_map(txt_corpus_clean, removeWords, stopwords())
  inspect(txt_corpus_clean[1:10])
  wordcloud(txt_corpus_clean, min.freq = 1000)
}



#Printing the word clouds
skills <- make_a_word_cloud(app_skills_list)
noskills <- make_a_word_cloud(no_app_skills$skillname)
skillsoffuture <- make_a_word_cloud(app_future_skills$skillname)

wordcloud(app_skills_list, min.freq = 10000)

grid.arrange(skills, noskills, skillsoffuture)



## Potential useful functions for later (Austin)

# Create weighted skills vector 
app_skills_repeated <- c(rep(skill_weights$skillname, skill_weights$frequency))


# Below is code to filter data to include only those who live in 
# identifiable Appalachian Counties. 

# Tibble with data only on those who can be identified as living in an Appalachian County
# data_full_fips <- data %>% 
#   mutate(STATEFIP = as.character(STATEFIP)) %>% 
#   mutate(COUNTYFIP = as.character(COUNTYFIP)) %>% 
#   filter(COUNTYFIP != "0") %>% 
#   mutate(
#     STATEFIP = if_else(nchar(STATEFIP) == 1, paste0("0", STATEFIP), STATEFIP)) %>% 
#   mutate(
#     COUNTYFIP = if_else(nchar(COUNTYFIP) == 1, paste0("00", COUNTYFIP), COUNTYFIP)) %>%
#   mutate(
#     COUNTYFIP = if_else(nchar(COUNTYFIP) == 2, paste0("0", COUNTYFIP), COUNTYFIP)) %>% 
#   unite(STATEFIP, COUNTYFIP, col = "FIP", sep = "") %>% 
#   mutate(FIP = as.numeric(FIP))

# app_ipums <- data_full_fips %>% 
#   filter(FIP %in% fip_list) 
# View(app_ipums) 




<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
>>>>>>> 7639b7ceb87a5ceaf764b2999b1f4d30111d3a18
=======
>>>>>>> 7639b7ceb87a5ceaf764b2999b1f4d30111d3a18
=======
>>>>>>> 7639b7ceb87a5ceaf764b2999b1f4d30111d3a18
=======
>>>>>>> 7639b7ceb87a5ceaf764b2999b1f4d30111d3a18
