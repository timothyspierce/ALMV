load("ShinyApp/data/ACS_Objects.RData")
appal2 <- readRDS("ShinyApp/data/appal2.RDS")
g <- readRDS("ShinyApp/data/g.RDS")
industry <- readRDS("ShinyApp/data/industry.Rds")


#### Create Chart for Unemployment  ----
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


#### Chart for Per Capita Income ----

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

#### Age Charts ----
#Under 15
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

#15 to 64
Age15_64<- p2 <- ggplotly(ggplot(data = appal2, aes(x = observation, y = age15_64, colour = nonmetro.f, names=NAME, text = str_c(NAME,": ", age15_64))) + geom_point() +  
  geom_hline(data = g, aes(yintercept=M_age15_64, color= "black")) + facet_wrap( nonmetro.f~.)  + 
  theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),                                                                                                                                                                                                                                               axis.title.y = element_text(color="black", size=10, face="bold")) +
  xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') +
    ggtitle("% of Population: Age 15-64") +
    scale_color_viridis_d(), tooltip = "text")
Age15_64

#65 Plus 
Age65Plus <- ggplotly(ggplot(data = appal2, aes(x = observation, y = age65plus, colour = nonmetro.f, names=NAME, text = str_c(NAME,": ", age15_64))) + geom_point()  +  
  geom_hline(data = g, aes(yintercept=M_age65plus, color="black")) + facet_wrap( nonmetro.f~.)  + 
  theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),
                    axis.title.x = element_text(color="black", size=8, face="bold"),
                    axis.title.y = element_text(color="black", size=10, face="bold")) +
  xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') +
    ggtitle("% of Population: Age 65+") + scale_color_viridis_d(), tooltip = "text")


Age65Plus


# Arrange 
subplot(AgeUnder15, Age15_64, Age65Plus, nrows = 3,  shareY=FALSE, titleX = TRUE, titleY=TRUE)


#### Education ----------------------------

# LT HS
EducationLTHS <- ggplotly(ggplot(data = appal2, aes(x = observation, y = LT_HS, colour = nonmetro.f, names=NAME, text = str_c(NAME, ": ", LT_HS))) + 
                 geom_point()  +  
  geom_hline(data = g, aes(yintercept=M_LT_HS, color= "black")) + 
  facet_wrap( nonmetro.f~.)  + 
  theme_bw()+ 
  theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),                                                                                                                                                                                                                             axis.title.y = element_text(color="black", size=10, face="bold")) +
  xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') + 
  ggtitle("% of Population: Less Than High School") + scale_color_viridis_d(), tooltip = "text")
EducationLTHS

# HS Diploma
EducationHSDP <- ggplotly(ggplot(data = appal2, aes(x = observation, y = HS_Dip, colour = nonmetro.f, names=NAME, text = str_c(NAME, ": ", HS_Dip))) + 
  geom_point()  + 
  geom_hline(data = g, aes(yintercept=M_HS_Dip, color="black")) + 
  facet_wrap( nonmetro.f~.)  + 
  theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),                                                                                                                                                                                                                                 axis.title.y = element_text(color="black", size=10, face="bold")) +
  xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') + 
    ggtitle("% of Population: HS Dip") + scale_color_viridis_d(), tooltip = "text")
EducationHSDP


# College and above
EducationCollPlus <- ggplotly(ggplot(data = appal2, aes(x = observation, y = Coll_Plus, colour = nonmetro.f, names=NAME, text = str_c(NAME, ": ", Coll_Plus))) + geom_point()  + 
  geom_hline(data = g, aes(yintercept=M_Coll_Plus, color= "black")) + 
  facet_wrap( nonmetro.f~.)  + 
  theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),                                                                                                                                                                                                                                          axis.title.y = element_text(color="black", size=10, face="bold")) +
  xlab("County") + ylab("Percent in County (%)") + 
  labs(color='County Classification') + 
  ggtitle("% of Population: College or More") + scale_colour_viridis_d(), tooltip = "text")

EducationCollPlus

#### Home Ownership -----------------------------
HomeOwnership <- ggplotly(ggplot(data = appal2, aes(x = observation, y = OwnHome, colour = nonmetro.f, names=NAME, text = str_c(NAME, ": ", observation))) + 
  geom_point()  +  geom_hline(data = g, aes(yintercept=M_OwnHome, color= "black")) + 
  facet_wrap( nonmetro.f~.)  + theme_bw()+ 
  theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),                                                                                                                                                                                                                                            axis.title.y = element_text(color="black", size=10, face="bold")) +
  xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') + 
  ggtitle("% of Population: Owns a Home") + scale_color_viridis_d(), tooltip = "text")
HomeOwnership

#### Disability & Health Insurance -------------------
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




#### Industry Data ----



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

saveRDS(industry, "ShinyApp/data/industry.Rds")

#Graph is here
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


#### Time to commute to work ---- 


tt014 % who travel less than 15 mins to work,
tt1529 % who travel between 15 to 30 mins
tt3044 % who travel 30 to 45
tt4559 % who travel between 45 to 60 mins
tt60plus % who travel more than 60 mins 




commute <- appal2[c(1,2,9,33:37)] %>% 
  pivot_longer(cols = c(tt014, tt1529, tt3044, tt4559, tt60plus),
               names_to = "Commute",
               values_to = "Total") %>% View()

#### Education Levels ----
