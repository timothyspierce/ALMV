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
library(readxl)

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

# Exchange X's for 9's or 199's
app_ipums_no_x <- app_ipums %>% 
  mutate(OCCSOC = str_replace_all(OCCSOC, "XXX", "199")) %>% 
  mutate(OCCSOC = str_replace_all(OCCSOC, "X", "9")) 


app_skills <- skills %>% filter(soc %in% app_ipums_no_x$OCCSOC)
no_app_skills <- skills %>% subset(!(soc %in% app_ipums_no_x$OCCSOC))
app_future_jobs <- app_ipums_no_x %>% filter(OCCSOC %in% futurejobs$soc)
app_future_skills <- skills %>% filter(soc %in% app_future_jobs$OCCSOC)



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
