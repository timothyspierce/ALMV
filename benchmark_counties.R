library(tidyverse)

# Obtain RUC list and modify
ruccodes <- read_csv("ruc.csv")
ruccodes <- ruccodes %>% select(-1)

# Get a glimpse of what RUC codes Appalachia most commonly has 
counties_appalachia <- ruccodes %>% filter(FIPS %in% fip_list)
counties_appalachia_rucc_counts <- counties_appalachia %>% 
  group_by(RUCC_2013) %>% count() %>% arrange(desc(wt = n))
counties_appalachia_rucc_counts

#Create list of counties not in appalachia
counties_not_appalachia <- ruccodes %>% filter(!(FIPS %in% fip_list))

# Here is where things get subjective, select counties in the 
# top few RUC codes that App. has
comparison_RUCCs <- c(6, 2, 7, 3, 4)
counties_comparison <- counties_not_appalachia %>% 
  filter(RUCC_2013 %in% comparison_RUCCs) %>% select(1:3)
