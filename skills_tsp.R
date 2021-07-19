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
library(tidyr)
library(tidyverse)

#Read in IPUMS data and PUMAs for Appalachia
ddi <- read_ipums_ddi("usa_00007.xml")

data <- read_ipums_micro(ddi)
app_pumas_2010 <- read_csv("2010_PUMAs_App.csv")
app_pumas_2010 <- app_pumas_2010 %>% 
  rename(PUMA = PUMACE10, STATEFIP = STATEFP10) %>% 
  mutate(PUMA = as.numeric(PUMA), STATEFIP = as.numeric(STATEFIP))
app_ipums <- 
  semi_join(data, app_pumas_2010)


# Filter out unemployed and exchange X's for 9's or 199's
app_ipums <- app_ipums %>% filter(OCCSOC > 0)
app_ipums <- app_ipums %>% filter(EMPSTAT == 1)
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

# Do the same for each state 
va_ipums <- va_ipums %>%  group_by(OCCSOC) %>% mutate(socamt = n()) 
ky_ipums <- ky_ipums %>%  group_by(OCCSOC) %>% mutate(socamt = n()) 
wv_ipums <- wv_ipums %>%  group_by(OCCSOC) %>% mutate(socamt = n()) 

#Create tibble of soc's with their associated frequencies in Appalachia 
socfreq <- app_ipums[,c("OCCSOC", "socamt")]
colnames(socfreq) <- c("soc", "socfreq")
socfreq <- distinct(socfreq)
View(socfreq)

# Do the same for each state 
va_socfreq <- va_ipums[,c("OCCSOC", "socamt")]
colnames(va_socfreq) <- c("soc", "socfreq")
va_socfreq <- distinct(va_socfreq)
View(va_socfreq)

ky_socfreq <- ky_ipums[,c("OCCSOC", "socamt")]
colnames(ky_socfreq) <- c("soc", "socfreq")
ky_socfreq <- distinct(ky_socfreq)
View(ky_socfreq)

wv_socfreq <- wv_ipums[,c("OCCSOC", "socamt")]
colnames(wv_socfreq) <- c("soc", "socfreq")
wv_socfreq <- distinct(wv_socfreq)
View(wv_socfreq)



#Read and adjust skills data
skills <- read_excel("Data/Skills_Onet.xlsx")
colnames(skills)[1] <- "soc"
colnames(skills)[4] <- "skillname"
colnames(skills)[5] <- "id"
skills <- mutate(skills, soc = substr(soc,1,7))
skills <- mutate(skills, soc = gsub("-", "", x = soc))

skills <- mutate(skills, skillname = gsub(" ", "",skillname))

View(new_socs)

# Change soc codes from 2010 to 2019
new_socs <- read.csv("Data/2010_to_2019_Crosswalk.csv")
new_socs <- new_socs %>% 
  mutate(soc = `O.NET.SOC.2010.Code`) %>% 
  mutate(soc_2019 =`O.NET.SOC.2019.Code` ) %>% 
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
  distinct()

# Change soc's to match skills info, making socs not in o*net 
# end in 1 rather than 0. 

## Find soc's only in socfreq
socs_na <- anti_join(socfreq, skills_wide)

# Do the same for each state 
va_socs_na <- anti_join(va_socfreq, skills_wide)
wv_socs_na <- anti_join(wv_socfreq, skills_wide)
ky_socs_na <- anti_join(ky_socfreq, skills_wide)

## change those soc's to end in 1 that end in 0
altered_na_socs <- socs_na %>% 
  mutate(soc = str_replace_all(soc, "00$", "99")) %>% 
  mutate(soc = str_replace_all(soc, "0$", "1")) %>% 
  select(soc, socfreq)

# Do the same for each state 
va_altered_na_socs <- va_socs_na %>% 
  mutate(soc = str_replace_all(soc, "00$", "99")) %>% 
  mutate(soc = str_replace_all(soc, "0$", "1")) %>% 
  select(soc, socfreq)
wv_altered_na_socs <- wv_socs_na %>% 
  mutate(soc = str_replace_all(soc, "00$", "99")) %>% 
  mutate(soc = str_replace_all(soc, "0$", "1")) %>% 
  select(soc, socfreq)
ky_altered_na_socs <- ky_socs_na %>% 
  mutate(soc = str_replace_all(soc, "00$", "99")) %>% 
  mutate(soc = str_replace_all(soc, "0$", "1")) %>% 
  select(soc, socfreq)

## Aggregate all similar socs
soc_not_na <- semi_join(socfreq, skills_wide)

# And for each state 
va_soc_not_na <- semi_join(va_socfreq, skills_wide)
wv_soc_not_na <- semi_join(wv_socfreq, skills_wide)
ky_soc_not_na <- semi_join(ky_socfreq, skills_wide)

## Combine similar and altered socs into one
altered_socs_freq <- bind_rows(soc_not_na, altered_na_socs)

va_altered_socs_freq <- bind_rows(va_soc_not_na, va_altered_na_socs)
wv_altered_socs_freq <- bind_rows(wv_soc_not_na, wv_altered_na_socs)
ky_altered_socs_freq <- bind_rows(ky_soc_not_na, ky_altered_na_socs)

# Create a standardized table with new "Importance level" column,
# created through the product of each importance and level ranking 
# on a scale from 0 to 1.   

skills_standardized <- skills_wide  %>% 
  mutate(Level = (Level / 7), Importance = Importance / 5) %>% 
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

# Determine which soc's are null
null_socs <- skills_indexed_counts %>% filter(is.na(index))
View(null_socs)

# Create an indexed counts with only soc's in common to those in Appalachia
skills_index_common <- inner_join(skills_indexed, altered_socs_freq)
View(skills_index_common)

# Do the same for each state 
va_skills_index_common <- inner_join(skills_indexed, va_altered_socs_freq)
View(va_skills_index_common)

wv_skills_index_common <- inner_join(skills_indexed, wv_altered_socs_freq)
View(wv_skills_index_common)

ky_skills_index_common <- inner_join(skills_indexed, ky_altered_socs_freq)
View(ky_skills_index_common)


#Create weighted index of skills in Appalachian Labor Market
skills_index_common <- mutate(skills_index_common, weighted = index*socfreq)
va_skills_index_common <- mutate(va_skills_index_common, weighted = index*socfreq)
wv_skills_index_common <- mutate(wv_skills_index_common, weighted = index*socfreq)
ky_skills_index_common <- mutate(ky_skills_index_common, weighted = index*socfreq)

app_weighted_skills <- skills_index_common %>% group_by(skillname) %>% summarize(skillweight = sum(weighted))
va_app_weighted_skills <- va_skills_index_common %>% group_by(skillname) %>% summarize(skillweight = sum(weighted))
wv_app_weighted_skills <- wv_skills_index_common %>% group_by(skillname) %>% summarize(skillweight = sum(weighted))
ky_app_weighted_skills <- ky_skills_index_common %>% group_by(skillname) %>% summarize(skillweight = sum(weighted))
View(app_weighted_skills)
View(va_app_weighted_skills)
View(wv_app_weighted_skills)
View(ky_app_weighted_skills)

app_weighted_skills <- mutate(app_weighted_skills, pctweight = (skillweight / sum(skillweight) * 100))
View(app_weighted_skills)

va_app_weighted_skills <- mutate(va_app_weighted_skills, pctweight = (skillweight / sum(skillweight) * 100))
View(va_app_weighted_skills)

wv_app_weighted_skills <- mutate(wv_app_weighted_skills, pctweight = (skillweight / sum(skillweight) * 100))
View(wv_app_weighted_skills)

ky_app_weighted_skills <- mutate(ky_app_weighted_skills, pctweight = (skillweight / sum(skillweight) * 100))
View(ky_app_weighted_skills)

# Plot weighted skills for Appalachia and each state of interest 
app_weighted_skills %>% ggplot() + 
  geom_col(aes(x = pctweight, y = reorder(skillname, pctweight)), fill = "coral") +
  labs(x = "Density Index", y = "Skillname", title = "Weighted Appalachian Skills") + 
  theme_minimal() + 
  scale_x_continuous(
    expand = c(0,0), limits = c(0, max(app_weighted_skills$pctweight)))

va_app_weighted_skills %>% ggplot() + 
  geom_col(aes(x = pctweight, y = reorder(skillname, pctweight)), fill = "seagreen") +
  labs(x = "Density Index", y = "Skillname", title = "Weighted Appalachian Virginia Skills") + 
  theme_minimal() + 
  scale_x_continuous(
    expand = c(0,0), limits = c(0, max(va_app_weighted_skills$pctweight)))

wv_app_weighted_skills %>% ggplot() + 
  geom_col(aes(x = pctweight, y = reorder(skillname, pctweight)), fill = "goldenrod1") +
  labs(x = "Density Index", y = "Skillname", title = "Weighted West Virginia Skills") + 
  theme_minimal() + 
  scale_x_continuous(
    expand = c(0,0), limits = c(0, max(wv_app_weighted_skills$pctweight)))

ky_app_weighted_skills %>% ggplot() + 
  geom_col(aes(x = pctweight, y = reorder(skillname, pctweight)), fill = "orangered") +
  labs(x = "Density Index", y = "Skillname", title = "Weighted Kentucky Appalachian Skills") + 
  theme_minimal() + 
  scale_x_continuous(
    expand = c(0,0), limits = c(0, max(ky_app_weighted_skills$pctweight)))


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
future_jobs_skills <- semi_join(skills_indexed, future_jobs)

# Sum indices for each skill and soc combination to obtain 
# indices for skills of the future. 
skills_future <- future_jobs_skills %>% 
  group_by(skillname) %>% 
  summarize(index = sum(index))

# Visualize skills of the future
skills_future %>% ggplot() +
  geom_col(aes(x = index, y = reorder(skillname, index)), fill = "salmon")  + 
  labs(x = "Index", y = "Skillname", title = "Skills of the Future") + 
  theme_minimal() + 
  scale_x_continuous(expand = c(0,0), limits = c(0, 130))

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
