load("ShinyApp/data/ACS_Objects.RData")
readRDS("ShinyApp/data/all_ACS5.rds") 
appal2 <- readRDS("ShinyApp/data/appal2.RDS")
g <- readRDS("ShinyApp/data/g.RDS")


#### Create Chart for Unemployment  ----
unemployed <- ggplotly(ggplot(data = appal2, aes(x = observation, y = Pct.Unemp, colour = nonmetro.f, names=NAME)) + geom_point() +  geom_hline(data = g, aes(yintercept=M_Pct.Unemp, color="black")) + 
facet_wrap( nonmetro.f~.)  + 
theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),
axis.title.x = element_text(color="black", size=8, face="bold"),
axis.title.y = element_text(color="black", size=10, face="bold")) +
  xlab("County") + ylab("Percent in County") + labs(color='County Classification') + ggtitle("% of Population: Unemployed") + scale_color_viridis_d())
unemployed %>%
  add_trace(
    text = appal2$NAME,
    hovertemplate = 'text'
  )


#### Chart for Per Capita Income ----

PerCapitaIncome <- ggplot(data = appal2, aes(x = observation, y = PerCapInc, colour = nonmetro.f, names=NAME)) + geom_point()  +  
  geom_hline(data = g, aes(yintercept=M_PerCapInc, color="black")) + facet_wrap( nonmetro.f~.)  +
  theme_bw()+ theme(axis.text.x = element_blank(), 
                    legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),
 axis.title.x = element_text(color="black", size=8, face="bold"),
axis.title.y = element_text(color="black", size=10, face="bold")) +
  xlab("County") + ylab("Income") + labs(color='County Classification') + ggtitle("Per Capita Income") + scale_color_viridis_d()

PerCapitaIncome <- ggplotly(p2) 
PerCapitaIncome <- PerCapitaIncome %>%
  add_trace(
    text = appal2$NAME,
    hoverinfo = 'text'
  )
PerCapitaIncome

#### Age Charts ----
#Under 15
p1 <- ggplot(data = appal2, aes(x = observation, y = age0_14, colour = nonmetro.f, names=NAME)) + geom_point()  +  geom_hline(data = g, aes(yintercept=M_age0_14, color=nonmetro.f)) + facet_wrap( nonmetro.f~.)  + theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),
                                                                                                                                                                                                                                      axis.title.x = element_text(color="black", size=8, face="bold"),
                                                                                                                                                                                                                                      axis.title.y = element_text(color="black", size=10, face="bold")) +
  xlab("County") + ylab("Percent in County") + labs(color='County Classification') + ggtitle("% of Population: Age 0-14") 

ggplotly(p1) 
p1 <- p1 %>%
  add_trace(
    text = appal2$NAME,
    hoverinfo = 'text'
  )

#15 to 64
p2 <- ggplot(data = appal2, aes(x = observation, y = age15_64, colour = nonmetro.f, names=NAME)) + geom_point()  +  geom_hline(data = g, aes(yintercept=M_age15_64, color=nonmetro.f)) + facet_wrap( nonmetro.f~.)  + theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),
                                                                                                                                                                                                                                        axis.title.x = element_text(color="black", size=8, face="bold"),
                                                                                                                                                                                                                                        axis.title.y = element_text(color="black", size=10, face="bold")) +
  xlab("County") + ylab("Percent in County") + labs(color='County Classification') + ggtitle("% of Population: Age 15-64") 

ggplotly(p2) 
p2 <- p2 %>%
  add_trace(
    text = appal2$NAME,
    hoverinfo = 'text'
  )


#65 Plus 
Age65Plus <- ggplot(data = appal2, aes(x = observation, y = age65plus, colour = nonmetro.f, names=NAME)) + geom_point()  +  
  geom_hline(data = g, aes(yintercept=M_age65plus, color="black")) + facet_wrap( nonmetro.f~.)  + 
  theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),
                    axis.title.x = element_text(color="black", size=8, face="bold"),
                    axis.title.y = element_text(color="black", size=10, face="bold")) +
  xlab("County") + ylab("Percent in County") + labs(color='County Classification') + ggtitle("% of Population By Metro Classification") + scale_color_viridis_d()

Age65Plus <- ggplotly(Age65Plus)  %>%
  add_trace(
    text = appal2$NAME,
    hoverinfo = 'text'
  )
Age65Plus
