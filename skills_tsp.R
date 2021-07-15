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



#Read in IPUMS data
ddi <- read_ipums_ddi("usa_00003.xml")
data <- read_ipums_micro(ddi)
app_ipums <- data %>% filter(STATEFIP %in% state_list) 



# Filter out unemployed and exchange X's for 9's or 199's
app_ipums <- data %>% 
  filter(STATEFIP %in% state_list) 
app_ipums <- app_ipums %>% filter(OCCSOC > 0)
app_ipums <- app_ipums %>% filter(EMPSTAT == 1)
app_ipums <- app_ipums %>% 
  mutate(OCCSOC = str_replace_all(OCCSOC, "XXX", "199")) %>% 
  mutate(OCCSOC = str_replace_all(OCCSOC, "XX", "99")) %>% 
  mutate(OCCSOC = str_replace_all(OCCSOC, "X", "9")) %>% 
  mutate(OCCSOC = str_replace_all(OCCSOC, "YY", "99")) 



#Add column for frequency of SOC code (socamt)
app_ipums <- app_ipums %>%  group_by(OCCSOC) %>% mutate(socamt = n()) 

#Create tibble of soc's with their associated frequencies in Appalachian states
socfreq <- app_ipums[,c("OCCSOC", "socamt")]
colnames(socfreq) <- c("soc", "socfreq")
socfreq <- distinct(socfreq)
View(socfreq)


#Read and adjust skills data
skills <- read_excel("Skills_Onet.xlsx")
colnames(skills)[1] <- "soc"
colnames(skills)[4] <- "skillname"
colnames(skills)[5] <- "id"
skills <- mutate(skills, soc = substr(soc,1,7))
skills <- mutate(skills, soc = gsub("-", "", x = soc))

skills <- mutate(skills, skillname = gsub(" ", "",skillname))


# Change soc codes from 2010 to 2019
new_socs <- read_csv("2010_to_2019_Crosswalk.csv")
new_socs <- new_socs %>% 
  mutate(soc = `O*NET-SOC 2010 Code`) %>% 
  mutate(soc_2019 =`O*NET-SOC 2019 Code` ) %>% 
  mutate(soc = substr(soc,1,7)) %>% 
  mutate(soc = gsub("-", "", x = soc)) %>% 
  mutate(soc_2019 = substr(soc_2019,1,7)) %>% 
  mutate(soc_2019 = gsub("-", "", x = soc_2019)) %>% 
  select(soc, soc_2019) %>% distinct()

skills <- inner_join(skills, new_socs, by = "soc") %>% 
  mutate(soc = soc_2019) %>% 
  select(-soc_2019)
skills <- distinct(skills)




# Create skills tibble with individual columns for importance and level
# Select only the soc's, skillnames and their associated importance and level
skills_wide <- 
  skills %>% 
  pivot_wider(names_from = `Scale Name`, values_from = `Data Value`) %>% 
  fill(Importance, .direction = "down") %>% 
  fill(Level, .direction = "up") %>% 
  select(soc, Title, skillname, Importance, Level) %>% 
  unique()
skills_wide <- skills_wide %>% 
  group_by(soc, skillname) %>% 
  mutate(Importance = mean(Importance)) %>% 
  mutate(Level = mean(Level)) %>% 
  distinct()

# Change soc's to match skills info, making socs not in o*net 
# end in 1 rather than 0. 
socs <- right_join(skills_wide, socfreq) %>% select(soc, Importance, socfreq)
## Find soc's only in socfreq
socs_na <- socs %>% filter(is.na(Importance))
## change those soc's to end in 1 that end in 0
altered_na_socs <- socs_na %>% 
  mutate(soc = str_replace_all(soc, "00$", "99")) %>% 
  mutate(soc = str_replace_all(soc, "0$", "1")) %>% 
  select(soc, socfreq)
## Aggregate all similar socs
soc_not_na <- socs %>% filter(!is.na(Importance)) %>% select(soc, socfreq) %>% distinct()
## Combine similar and altered socs into one
altered_socs_freq <- bind_rows(soc_not_na, altered_na_socs)

# Create a standardized table with new "Importance level" column,
# created through the product of each importance and level ranking 
# on a scale from 0 to 1.   

skills_standardized <- skills_wide  %>% 
  mutate(Level = (Level / 7), Importance = Importance / 5) %>% 
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

# Create an indexed counts with only soc's in common 
skills_index_common <- inner_join(skills_indexed, altered_socs_freq)
View(skills_index_common)

# Create table having importance and level with counts 
skills_importance_level_common <- 
  inner_join(skills_wide, altered_socs_freq) %>% 
  mutate(Importance = Importance/5, Level = Level/7)
View(skills_importance_level_common)
# ----------------- Curiosity---------------------------------------------------
summary(skills_index_common$index)
high_index_skills <- skills_index_common %>% filter(index > 0.4)
View(high_index_skills)
high_index_skill_counts <- high_index_skills %>% 
  group_by(skillname) %>% 
  summarise(count = sum(socfreq))
View(high_index_skill_counts)
low_index_skills <- skills_index_common %>% filter(index < 0.2 & index > 0.15)
View(low_index_skills)
low_index_skill_counts <- low_index_skills %>% 
  group_by(skillname) %>% 
  summarise(count = sum(socfreq))
View(low_index_skill_counts)



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
