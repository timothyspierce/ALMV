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

ddi <- read_ipums_ddi("usa_00003.xml")

data <- read_ipums_micro(ddi)

data %>% 
  filter(STATEFIP %in% state_list) -> app_ipums
View(app_ipums)

skills <- read_excel("Skills_Onet.xlsx")
colnames(skills)[1] <- "soc"
colnames(skills)[4] <- "skillname"
skills <- mutate(skills, soc = substr(soc,1,7))
skills <- mutate(skills, soc = gsub("-", "", x = soc))
#skills <- mutate(skills, skillname = gsub(" ", "",skillname))


futurejobs <- read_excel("Rapid_Growth.xls")[-c(1:3),1]
colnames(futurejobs) <- "soc"
futurejobs <- mutate(futurejobs, soc = substr(soc,1,7))
futurejobs <- mutate(futurejobs, soc = gsub("-", "", x = soc))

app_ipums <- app_ipums %>% filter(OCCSOC > 0)
app_skills <- skills %>% filter(soc %in% app_ipums$OCCSOC)
no_app_skills <- skills %>% subset(!(soc %in% app_ipums$OCCSOC))
app_future_jobs <- app_ipums %>% filter(OCCSOC %in% futurejobs$soc)
app_future_skills <- skills %>% filter(soc %in% app_future_jobs$OCCSOC)


app_ipums <- mutate(app_ipums, missing = ifelse(substr(app_ipums$OCCSOC, 6,6)=="X", 1,0))
sum(app_ipums$missing)

skills <- mutate(skills, missing = ifelse(substr(skills$soc, 6,6)=="X", 1,0))
View(skills)

make_a_word_cloud <- function(pdfname){
  txt_corpus <- Corpus(VectorSource(pdfname))
  txt_corpus_clean <- tm_map(txt_corpus, tolower)
  txt_corpus_clean <- tm_map(txt_corpus_clean, removePunctuation)
  txt_corpus_clean <- tm_map(txt_corpus_clean, stripWhitespace)
  txt_corpus_clean <- tm_map(txt_corpus_clean, removeWords, stopwords())
  inspect(txt_corpus_clean[1:10])
  wordcloud(txt_corpus_clean, min.freq = 8)
}

skills <- make_a_word_cloud(app_skills$skillname)
noskills <- make_a_word_cloud(no_app_skills$skillname)
skillsoffuture <- make_a_word_cloud(app_future_skills$skillname)

grid.arrange(skills, noskills, skillsoffuture)
