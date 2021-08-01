rm(list=ls())

library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(sjmisc)
library(shinythemes)
library(DT)
library(data.table)
library(rsconnect)
library(shinycssloaders)
library(readxl)
library(readr)
library(stringr)
library(shinyjs)
library(ipumsr)
library(tm)
library(pdftools)
library(gridExtra)
library(tigris)
library(htmltools)
library(leafpop)
library(rvest)
library(plotly)
library(ggpubr)


prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 7)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")


# Data -----------------------------------------------------------

# Data: Maps------------------------------------------------------
    ## Read in index data ----------------------------------------------------------------
    app_weighted_skills_by_PUMA <- read_csv(paste0(getwd(),"/data/App_weighted_skills_by_PUMA.csv"))
    
    ### Obtain polygons to map onto leaflet for Appalachian PUMAs---------------------
    
    # Narrow down to Appalachian States
    counties<-read.csv(paste0(getwd(),"/data/ALMV_counties_all.csv"), header=T) %>%
      rename(state_code=State)%>%
      mutate(County=str_replace_all(County,"\'|\\.",""))%>%
      mutate(County=str_trim(County, side="both"))
    counties$state_code=as.character(counties$state_code)
    state_list<-unique(counties$state_code)
    state_list[1] <- "01"
    
    # Pull polygons from tigris
    options(tigris_use_cache = TRUE)
    puma_geoms_list <- lapply(state_list, function(x) {
      pumas(state = x, cb = T)
    })
    puma_geoms <- rbind_tigris(puma_geoms_list)
    
    # Make PUMAS unique by combining STATEFIP and PUMA
    puma_geoms <- puma_geoms %>% unite(STATEFP10, PUMACE10, col = "PUMA", sep = "")
    
    #Obtain list of Appalachian PUMAS
    app_pumas <- as_tibble(unique(app_weighted_skills_by_PUMA$PUMA)) %>% rename(PUMA = value)
    
    #Limit tigris data to Appalachia
    puma_app_geoms <- semi_join(as.data.frame(puma_geoms), app_pumas)
    
    # Associate index values with geoms 
    map_data <- left_join(app_weighted_skills_by_PUMA, puma_app_geoms) %>% 
      select(skillname, PUMA, `Normalized Index`, geometry, NAME10)


# Data: Labor Market Profile
    load("data/ACS_Objects.RData")
    appal2 <- readRDS("data/appal2.RDS")
    g <- readRDS("data/g.RDS")
    industry <- readRDS("data/industry.Rds")
    


# CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY
jscode <- "function getUrlVars() {
                var vars = {};
                var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) {
                    vars[key] = value;
                });
                return vars;
            }

           function getUrlParam(parameter, defaultvalue){
                var urlparameter = defaultvalue;
                if(window.location.href.indexOf(parameter) > -1){
                    urlparameter = getUrlVars()[parameter];
                    }
                return urlparameter;
            }

            var mytype = getUrlParam('type','Empty');

            function changeLinks(parameter) {
                links = document.getElementsByTagName(\"a\");

                for(var i = 0; i < links.length; i++) {
                   var link = links[i];
                   var newurl = link.href + '?type=' + parameter;
                   link.setAttribute('href', newurl);
                 }
            }

           var x = document.getElementsByClassName('navbar-brand');

           if (mytype != 'economic') {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/events/symposium2020/poster-sessions\">' +
                              '<img src=\"DSPG_black-01.png\", alt=\"DSPG 2020 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('dspg');
           } else {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights/case-studies\">' +
                              '<img src=\"AEMLogoGatesColorsBlack-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('economic');
           }
           "



ui <- navbarPage(title = "ALMV",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),inverse = T,
                 
                 # Tab Overview-----------------------------------------------------------

                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   # br("", style = "padding-top:2px;"),
                                   # img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                   br(""),

                                   h1(strong("Appalachian Labor Markets: Preparing for Jobs of the Future"),
                                      br(""),
                                      h4("Data Science for the Public Good Program"),
                                      h4("Virginia Tech"),


                                      br()
                                   )
                          ),
                          fluidRow(style = "margin: 6px;",
                                   column(4,
                                          h2(strong("Overview Appalachia")),
                                          p("Ranging from southern New York to Northern Mississippi, the region is commonly referred to as Appalachia due to its location within the namesake mountain range. Encompassing 420 counties, across 13 states, Appalachia is home to over 25 million residents spread over 205,000 square miles. The overall regions are split into five subregions:"),
                                          p(),
                                          tags$ul(
                                            tags$li("Northern, including counties in New York, Pennsylvania, Maryland, Ohio, and West Virginia"),
                                            tags$li("North Central, including counties in Ohio and West Virginia"),
                                            tags$li("Central, including counties in West Virginia, Kentucky, Virginia, and Tennessee"),
                                            tags$li("South Central, including counties in Virginia, North Carolina, and Tennessee"),
                                            tags$li("Southern, including counties in South Carolina, Georgia, Alabama, and Mississippi")
                                          ),
                                          p(),
                                          p("Appalachia has long been known for its diverse culture, often sensationalized by popular media negatively. Commonly associated with lasting myths and misrepresentation regarding the behavior and isolation of the inhabitants, popular media throughout the decades have focused on the more lurid aspects of the region's culture. Ranging from tales of moonshining escapades and violent clan wars to the idea that the region is home to violent and uneducated people, Appalachia has been wrongfully portrayed in the media for years."),
                                          p(),
                                          p("Rich in natural resources such as coal, natural gas, iron, petroleum, and lumber, Appalachia has been primarily home to industries related to these resources. Large-scale mining and logging operations sustained the region's job market for years and brought along with it a new age of modernization in the territory. However, even with its wealth of natural resources, Appalachia has long struggled economically and has been associated with poverty.")
                                   ),

                                   column(4,
                                          h2(strong("Motivation for the Project ")),
                                          p("Despite close to 60 years of direct federal aid, Appalachia continues to be characterized by immense poverty. While immeasurable progress has occurred since the signing of the Appalachian Development Act in 1965 and the creation of the Appalachian Regional Commission that oversees economic development in the region, one of the issues that remain is that Appalachian labor markets are still vulnerable to shifts. These shifts could lead to and have already led to large amounts of unemployment and poverty."),
                                          p(),
                                          p("As the United States aims to move toward a greener more sustainable future, the job market will evolve accordingly, whether by choice or by government mandate. Heavily polluting or environmentally harmful jobs will have to either go green or risk elimination. This risk is what is most significantly affecting the Appalachian region. Rich in natural resources such as coal, natural gas, and lumber, the Appalachian labor market has taken advantage of these resources to create jobs for generations. As these jobs are phased out for a multitude of factors, the region is challenged with a troubling reality: residents must now find new jobs of the future that are often unrelated to anything they have ever done in the past. This is the focus of our project.")
                                   ),
                                   column(4,
                                          h2(strong("Objective for the Project")),
                                          p("Upon completion of this project, our research team aims to create a baseline understanding of the Appalachian labor market's past, present, and potential future, to create targeted recommendations for jobs of the future utilizing skills already present with the residents of the region. "),
                                          p("We implemented the Data Science Process to identify our problem, acquire, process, and explore publicly available data, and perform in-depth analysis, to provide the Appalachian region with our data-driven findings."),
                                          p("We used a labor supply and demand framework to understand the underlying drivers of skill content within each county. Focusing on five distinct supply factors:"),
                                          tags$ul(
                                            tags$li("Education and Educational Attainment"),
                                            tags$li("Age Distribution"),
                                            tags$li("Health and Disability"),
                                            tags$li("Health Insurance Availability"),
                                            tags$li("Internet/Broadband and Computer Access (education)")
                                          ),
                                          p("Through in-depth analysis of these factors, as it relates to Appalachia and similar regions, we can understand what the Appalachian workforce currently offers to a potentially emerging new labor market."),
                                          p("Additionally, we explored demand factors that will impact what new jobs and industries are potentially viable in the Appalachian region. Focusing on three main factors:"),

                                          tags$ul(
                                            tags$li("Internet/Broadband and Computer Access (employment)"),
                                            tags$li("Income Per Capita"),
                                            tags$li("Underlying Industrial Sectors")
                                          ),
                                          p("These factors drive demand in the region for goods and services and give a better idea of what new jobs are possible to implement for the future growth of the Appalachian labor market."),
                                          p("This dashboard compiles our findings and allows all interested parties to explore the information dynamically."),
                                          p(),
                                          p()
                                   )
                          # ),
                          # fluidRow(align="left",
                          #          p(strong("Reference")),
                          #          P(a("Appalachian Regional Commission",href = "https://www.arc.gov/about-the-appalachian-region/", target = "_blank")),
                          #          p(a("Economic Redevelopment in Appalachia: The Appalachian Regional Commission",href = "http://www.umich.edu/~econdev/arc/", target = "_blank")),
                          #          p(a("Infoplease: Appalachian Mountains",href = "https://www.infoplease.com/encyclopedia/places/north-america/us-geography/appalachian-mountains", target = "_blank")  )

                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2021'))))
                 ),
                 # Tab Labor Market -----------------------------------------------------------
                 tabPanel("Appalachian Labor Market", value = "labor",
                          
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Appalachian Labor Market Charateristics"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(6,
                                          tabsetPanel(
                                            tabPanel("Supply",
                                                     p(""),
                                                     ## Input: supply-----------------
                                                     selectInput("supply", "", width = "100%",selected="Education", choices = c(
                                                       "Education" = "supply1",
                                                       "Age Distribution" = "supply2",
                                                       "Disability Rates" = "supply3",
                                                       "Health Insurance Coverage" = "supply4",
                                                       "Access to Technology" = "supply5"
                                                     )),
                                                     p(strong("Ryan with the help of Leo-Allen")),
                                                     ## Output: 1-----------------
                                                     withSpinner(plotlyOutput("supplyoutput")),
                                                     p(tags$small("Data Sources: Homeland Infrastructure Foundation-Level Data, 2010; CoreLogic, 2019; TravelTime API."))
                                            ),
                                            tabPanel("Demand",
                                                     p(""),
                                                     ## Input: demand-----------------
                                                     selectInput("demand", "", width = "100%",selected="Income per Capita", choices = c(
                                                       "Income per Capita" = "demand1",
                                                       "Unemployment" = "demand2", 
                                                       "Industrial sectors" = "demand3"
                                                     )),
                                                     p(strong("Ryan with the help of Leo-Allen")),
                                                     ## Output: 2-----------------
                                                     withSpinner(plotlyOutput("demandoutput")),
                                                     p(tags$small("Data Sources: Homeland Infrastructure Foundation-Level Data, 2010; CoreLogic, 2019; TravelTime API."))
                                            ),
                                            
                                            tabPanel("Others",
                                                     p(""),
                                                     ## Input: others-----------------
                                                     selectInput("other", " ", selected = "Home Ownership",width = "100%", choices = c(
                                                       "Home Ownership" = "other1",
                                                       "Renters" = "other2",
                                                       "Vehicle Ownership" = "other3"
                                                     )),
                                                     p(strong("Ryan with the help of Leo-Allen")),
                                                     ## Output: 3-----------------
                                                     withSpinner(plotlyOutput("otheroutput")),
                                                     p(tags$small("Data Sources: Homeland Infrastructure Foundation-Level Data, 2010; CoreLogic, 2019; TravelTime API."))
                                            )
                                            
                                          )
                                   ),
                                   column(6,
                                          h4(strong("Supply Side Factors in the Appalachian Labor Market")),
                                    
                                          p(strong("Education:"),"These graphs display the percent of the population based on their highest level of schooling in metropolitan vs. nonmetropolitan areas throughout Appalachia. All states in the region appear to have metropolitan counties that score high at each education level and some that score low. Overall, nonmetropolitan counties in Appalachia have significantly more residents who solely have a high school degree than metropolitan counties. Metropolitan counties in Appalachia have significantly more residents who have a college degree or higher than nonmetropolitan counties. The percent of people with a high school diploma and some college certificates is similar between metropolitan and nonmetropolitan areas. However, significantly more people hold less than a high school diploma in nonmetropolitan areas, while in contrast, more people have acquired a college degree or higher in metropolitan areas. It is worth noting that there is a spike in percent of the population in counties throughout Appalachia that house universities regardless of whether they are metropolitan or nonmetropolitan."),
                                         
                                          h4(strong("Demand Side Factors in the Appalachian Labor Market")),
                                          p(strong("Per Capita Income:"), "This interactive graph illustrates per capita income in each county in metropolitan vs. nonmetropolitan areas throughout Appalachia. Grouped by state alphabetically from left to right, positive spikes in per capita income in metropolitan areas are visible closer to population centers. Negative spikes in per capita income in nonmetropolitan areas that are farthest away from population centers. This indicates a potential connection between urbanicity and per capita income which can be seen as the average per capita income level between metropolitan and nonmetropolitan areas are quite significant. "),
                                          p(),
                                          p(strong("Unemployment:"), "This interactive graph presents the percent of the population that is unemployed in each county in metropolitan vs. nonmetropolitan areas throughout Appalachia. Grouped by state alphabetically from left to right, positive spikes in unemployment in metropolitan areas are seen in Alabama and West Virginia, while similar increases in nonmetropolitan areas are in Kentucky and West Virginia. While relatively small, on average the percent of the population that is unemployed tends to be higher in nonmetropolitan counties. "),
                                          p(),
                                          p(strong("Industrial Makeup:"), " to be written"),
                                          h4(strong("Other Factors in the Appalachian Labor Market")),
                                          p(strong("Home Ownership:"), "This interactive graph displays the percent of the population who own their home in each county in metropolitan vs. nonmetropolitan areas throughout Appalachia. Grouped by state alphabetically from left to right, a large amount of variation is noted in metropolitan and nonmetropolitan areas of Appalachia. It is worth noting that negative spikes in homeownership are seen throughout Appalachia particularly in counties that house universities. Overall, nonmetropolitan counties in Appalachia have more residents who own their homes than metropolitan counties.  "),
                                          p(),
                                          p(strong("Renters:"), "to be written "),
                                          p(),
                                          p(strong("Vehicle Ownership:"), " to be written")
             
                                   )
                                  
                          )
                 ),
                 
                 #Tab Data -----------------------------------------------------------
                 tabPanel("Data and Measures", value = "data",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Data and Measures"), align = "center"),
                                   br()
                          ),
                          tabsetPanel(
                            tabPanel("Measures",
                                     h3(strong(""), align = "center"),
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("The Skills Index"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              h4(strong("The Skills Index")),
                                              p("Our project creates measures of the current employment and skill prevalence for communities in Appalachia. We build on work by Autor, Levy, and Murname (2003) and Berger and Frye (2016) and use individual-level data to understand the occupations and industries of Appalachia. The most recently available national data is the American Community Survey (ACS) 2019-5 year data. Our analysis is conducted at the PUMA level, the lowest level of geographic identifier available in the ACS."),
                                              p("We use 6-digit SOC codes extracted from the ACS to obtain estimates of occupation prevalence in each area."),
                                              p("The project uses O*Net data to describe and quantify the skill content of Appalachian labor. The O*Net data lists 35 skills by order of importance and proficiency for 772 unique SOC occupation codes. Using 2019 skill rankings, we matched O*Net skills to the occupations found in Appalachia."),
                                              p("Our team constructs an index that is used to understand the distribution of occupations in Appalachia. Our index identifies the skills that are the most prevalent in Appalachian communities. For each occupation (SOC), the 35 skills are assigned a value based on their importance and the total number of individuals employed in that position in a PUMA."),
                                              p("The skill index that we construct can be interpreted as a measure of the prevalence of a skill in a community. Suppose the skill is very important to a very popular occupation in the community. In that case, workers in this community must have very high levels of proficiency in that skill, and it has a higher skill index. If a skill is not as essential and in an uncommon occupation, it is assigned a lower skill index. The skill prevalence index, therefore, represents the relative importance of skills within a PUMA. The index is used to compare and rank the skill content of PUMAs in Appalachia. It should not be used to answer the question 'by how much does one PUMA differ from another' but instead how prevalent in a community is the skill in question."),
                                              h4(strong("A Simplified Example of the Index")),
                                              p("Consider a community that has workers in two occupations—math teachers and coal miners. Each occupation requires only two skills—public speaking and manual labor rated the proficiency of the required skill is rated on a scale from zero to one. As displayed in Table 1, assume math teachers require proficiency in public speaking of 0.95 and manual labor of 0.3. Coal miners require public speaking skills of 0.1, and manual labor of 0.8.  "),
                                              
                                             
                                              p("Without knowing the distribution (count) of coal miners and math teachers in a community, we conclude that public speaking is the most prevalent skill in this labor market. However, suppose this labor market has 100 coal miners and only 5 math teachers, then this assumption would be incorrect. If we consider the distribution of labor within occupations, we multiply each skill index for each occupation by the number of individuals employed in this occupation. For the hypothetical labor market, math teachers' weighted public speaking value is 0.95 x 5 = 4.75, and coal miners' weighted public speaking value is 0.1 x 100 = 10. Similarly, the weighted manual labor value for math teachers is 1.5, and for coal miners is 80. This community's raw public speaking skills index is 10 + 4.75 = 14.75, and the raw manual labor skills index is 81.5. The normalized or Public Speaking Skill Index is then 14.75/96.25 = .15, and for manual labor is 0.85.  For this community, the Skill indices show that manual labor with a value of 0.85 is much more prevalent than public speaking at 0.15 for this community."),
                                              p("This simple illustration does not account for skill importance in an occupation. The skill prevalence index we construct does account for skill importance as well. Importance and proficiency are highly correlated in our data, but there are occupations where this differs. For example, consider a job that requires a low proficiency in typing even though typing is important because the employees must type a few things every week. We multiply the importance level by the proficiency necessary to compensate for some of these unusual occupations.  "),
                                             h4("Table 1: An Illustration of Construction of Skill Index", align = "center"),
                                              plotOutput("example_table"),
                                               h4(strong("Technical Description of Index Creation")),
                                              p("To understand the distribution of occupations, we examined occupational counts by PUMA. The data were grouped by PUMA and SOC, creating groups of unique PUMA and SOC combinations and the total number of workers in each occupation for each PUMA in Appalachia. "),
                                              p("The SOC’s available from O*Net’s 2019 Skills rankings use the 2010 SOC classification. We used the O*Net 2010 to 2019 SOC crosswalk to transform all 2010 SOC’s to 2019 SOC’s. The Skills data is used to create a unique normalized ranking for each soc and skill combination. "),
                                              p("To account for both skill importance and proficiency within an SOC code, a new index was created by taking the normalized values of the product of Importance and Proficiency. As outlined in the illustration table above, we weighted these measures by population in an occupation and normalized it to create a skill index ranging from 0 to 1. This index now accounts for the prevalence of an occupation and better represents the importance and level of a skill within a community. "),
                                              p("To understand how the current skill content of Appalachian communities is suited to occupations of the future, we used the O*Net’s Bright Outlook Occupations. We merged these data by soc code in the ACS data. We then created a skill index limited to only these occupations creating what we refer to as “job skills of the future.” We then create an index for each of the skills in the manner described earlier.  We followed Autor et al. (199?) and grouped these skills into X categories: NAME THEM….. For the sake of exposition and brevity, we present only the most relevant skill in each of these categories here as measured by their importance and proficiency.  "),
                                              p("We present the indices of the relative importance of skills of the future across Appalachian PUMAs, using a series of interactive maps were created. The color of each PUMA represents its respective index value for the user-chosen skill of the future. Popups for the PUMA allow the user to visualize the industry breakdown of each PUMA to understand the effects of the PUMAs industry makeup on its bright skills index value. The industry data came from the 2019 5-year ACS “Industry by Occupation for the Civilian Population” table at the PUMA level from the 2019 5-year ACS estimates. Pie Charts for the proportion of the population in each industry are also available in the pop-ups when you hover on a community. ")
                                              
                                     )
                                     
                                     
                            ),
                            tabPanel("Data Sources",
                                     h3("", align = "center"),
                                     br(""),
                                     column(4,
                                            img(src = "data-acs.png", style = "display: inline; float: left;", width = "200px"),
                                            p(strong("American Community Survey."), "The American Community Survey (ACS) is an ongoing yearly survey conducted by the U.S Census
                                            Bureau. ACS samples households to compile 1-year and 5-year datasets providing information on population sociodemographic and
                                            socioeconomic characteristics including employment, disability, and health insurance coverage. We used 2019 ACS 5-year datasets."),
                                            br(""),
                                            img(src = "data-onet.png", style = "display: inline; float: left;", width = "120px"),
                                            p(strong("O*Net."), "The O*NET database contains hundreds of standardized and occupation-specific
                                              descriptors on almost 1,000 occupations covering the entire U.S. economy. We used the O*Net's mapping of O*NET-SOC codes (occupations) to Skill ratings
                                              from the 25.3 database. 
                                              "),
                                            br(""),
                                            img(src = "data-IPUMS.png", style = "display: inline; float: left;", width = "120px"),
                                            p(strong("IPUMS."), "IPUMS provides census and survey data from around the world integrated 
                                              across time and space. We created a data extract from IPUMS USA at the Public Use Microdata 
                                              Area level using 2019 ACS 5-year sample information providing information on occupation and 
                                              location.")
                                     ),
                                     
                                    
                            )
                          
                          )
                 ),
                 # Tab Skills-----------------------------------------------------------
                 tabPanel("Skill Prevalence", value = "skills",
                          
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Skills Prevalence of the Region"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                          p("Our dashboard presents the skill prevalence index for five skills that we identified as important to the future economy.  A skill prevalence index was created for all 35 skills, but we present only a subset for brevity. The skills we chose to show are needed in what is known as 'Bright Outlook Occupations,' defined as occupations that are 'projected to grow faster than average (employment increase of 5% or more)' and 'to have 100,000 or more job openings over the period 2019-2029 for the US nationwide'. The five skills that we present in the dashboard below are: Technology Design,   Reading Comprehension, Monitoring, Coordination, Active Listening. "),
                                          p("The map for Technology Design displays the prevalence of this skill within each Appalachian PUMA. The legend shows that the Skill Prevalence Index for the entire region ranges between 0.006 to 0.009. This range suggests that for each PUMA in Appalachia, Technology Design skills are not the dominant skills needed for the occupations in that community relative to other skills in that community.  In other words there are very few jobs that require this skill in each PUMA. The highest prevalence of Technology Design skills is noted in PUMAs that contain large metro areas. For example, if you over over the yellow region on the map, a pop-up will show the Huntsville North and Madison East PUMA’s. This community has the highest prevalence of Technology Design Skills and it includes the city of Huntsville, AL which is the blue circle on the map. Huntsville is a city with a large number of high technology jobs due to space and aeronautics industry @@@ describe popup of industry histogram – not showing up on currently deployed website.  Discuss this nicely. "),
                                          p("For Reading Comprehension, Monitoring, and Coordination, the index values suggest that these skills are prevalent within each community. Reading Comprehension exhibits the clustering pattern described for Technology. Skills such as Coordination and Monitoring do not appear to follow any typical pattern.   "),
                                          p("final thought statement tbd"),
                                  
                                          ## Input: skills------------
                                         selectInput("skills", "Select the Skill:", width = "100%", choices = c(
                                                      "Technology Design"="TechnologyDesign",
                                                      "Reading Comprehension"="ReadingComprehension",
                                                       "Monitoring",
                                                      "Coordination",
                                                         "Active Listening"="ActiveListening")),
                                               
                                         ## Output: skillsoutput ------------------------
                                          withSpinner(leafletOutput("skillsoutput")),
                                         p(tags$small("Data Sources: Constructed by Authors")),

                          
                                   
                                            )
                     
                          ),
                 
  

                 # Tab contact -----------------------------------------------------------
                 tabPanel("Meet the Team", value = "contact",
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                                   h1(strong("Team Members"), align = "center"),
                                   br(),
                                   h4(strong("Virginia Tech Data Science for the Public Good")),
                                   p("The", a(href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"),
                                     "is a summer immersive program held at the", a(href = 'https://aaec.vt.edu/s', 'Virginia Tech Department of Agricultural and Applied Economics.'),
                                     "In its second year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around
                              critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences
                              to determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program
                              highlights, how to apply, and our annual symposium, please visit", a(href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html', 'the official VT DSPG website.', target = "_blank")),
                                   p("", style = "padding-top:10px;")
                          ),
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                                 
                                   column(6, align = "center",
                                          h4(strong("DSPG Team Members")),
                                          img(src = "fellow-pierce.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "fellow-yang.jpeg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-jacobs.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-burcham.jpeg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),

                                          p(a(href = "www.linkedin.com/in/timothyspierce", 'Timothy Pierce', target = '_blank'),"(Virginia Tech, Department of Agricultural and Applied Economics);"),
                                          p(a(href = "https://www.linkedin.com/in/yang-cheng-200118191/", 'Yang Cheng', target = '_blank'),"(Virginia Tech, Department of Agricultural and Applied Economics)."),
                                          p(a(href = "https://www.linkedin.com/in/ryan-jacobs-bb5727174/", 'Ryan Jacobs', target = '_blank'),"(Virginia Tech, Environmental Economics);"),
                                          p(a(href = "https://www.linkedin.com/in/austin-burcham-9b32a81ab/", 'Austin Burcham', target = '_blank'),"(Virginia Tech, Computational Modeling and Data Analytics)."),
                                          
                                          
                                          
                                          p("", style = "padding-top:10px;")
                                   ),                                  
                                   
                                   column(6, align = "center",
                                          h4(strong("VT AAEC Team Members")),
                                          img(src = "faculty-chen.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "faculty-gupta.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = "https://aaec.vt.edu/people/faculty/chen-susan.html", 'Dr. Susan Chen', target = '_blank'),"(Virginia Tech, Department of Agricultural and Applied Economics);"),
                                          p(a(href = "https://aaec.vt.edu/people/faculty/gupta-anubhab.html", 'Dr. Anubhab Gupta', target = '_blank'),"(Virginia Tech, Department of Agricultural and Applied Economics)."),

                                          p("", style = "padding-top:10px;")
                                   )
                          )
                      )
                 )
              

# server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  runjs(jscode)
  
  # skills map: in progress -----------------------------------------------------

  ## Create piecharts for map------------------------------------------------------
  industry_breakdown_app_PUMAs <- read_csv(paste0(getwd(),"/data/2019-App_NAICS.csv"))
  industry_breakdown_app_PUMAs <- industry_breakdown_app_PUMAs %>%
    mutate(relfreq = estimate / summary_est)
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
  
  ## Add city points-----------------------------------------------------------
  # cities_link <- "https://en.wikivoyage.org/wiki/Appalachia"
  # page <- read_html(cities_link)
  # city <- page %>%  html_nodes("ol li") %>% html_text()
  # city <- as_tibble(city) %>% separate(value, sep = " ", into  = c("City", "State"))
  # city <- city %>% mutate(State = str_replace_all(State, pattern = "\\)|\\(| ", ""))
  # city <- city %>% mutate(City = str_c(City, "city", sep = " ")) %>%
  #   unite(City, State, col = "NAME", sep = ", ")
  # city_info <- get_acs(geography = "place", variables = "B01003_001",
  #         year = 2019, survey = "acs5")
  # city_info <- semi_join(as.data.frame(city_info), city) %>% 
  #   select(NAME, estimate) %>% rename(City = NAME, Population = estimate)
  # city_info <- city_info %>%
  #   mutate(City = str_replace(City, pattern = " city", ""))
  # write_csv(city, "Appalachian_cities.csv")
  # city_coords <- read_csv("geocoded_cities.csv")
  # city_coords <- city_coords %>% select(location, lon, lat) %>% 
  #   rename(City = location )
  # city_info <- inner_join(city_info, city_coords, by = "City")
  # write_csv(city_info, "2019-Appalachian_cities_and_population")
  city_info <- read_csv(paste0(getwd(),"/data/2019-Appalachian_cities_and_population.csv"))
  labels = lapply(str_c("<strong>", city_info$City,"</strong>","<br/>", "Population: ", 
                        formatC(city_info$Population, format = "f", big.mark = ",", digits = 0)), 
                  htmltools::HTML)
  
  ## input: skills-----------
  supplyvar <- reactive({
    input$supply
  })
  
  ## output:laboroutput---------------
  output$supplyoutput <- renderPlotly({
    
    ##### Education ------
    if(supplyvar() == "supply1") {
      EducationLTHS <- ggplotly(ggplot(data = appal2, aes(x = observation, y = LT_HS, colour = nonmetro.f, names=NAME, text = str_c(NAME, ": ", LT_HS))) + 
                                  geom_point()  +  
                                  geom_hline(data = g, aes(yintercept=M_LT_HS, color= "black")) + 
                                  facet_wrap( nonmetro.f~.)  + 
                                  theme_bw()+ 
                                  theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),                                                                                                                                                                                                                             axis.title.y = element_text(color="black", size=10, face="bold")) +
                                  xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') + 
                                  ggtitle("% of Population: Less Than High School") + scale_color_viridis_d(), tooltip = "text")

    
      EducationHSDP <- ggplotly(ggplot(data = appal2, aes(x = observation, y = HS_Dip, colour = nonmetro.f, names=NAME, text = str_c(NAME, ": ", HS_Dip))) + 
                                  geom_point()  + 
                                  geom_hline(data = g, aes(yintercept=M_HS_Dip, color="black")) + 
                                  facet_wrap( nonmetro.f~.)  + 
                                  theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),                                                                                                                                                                                                                                 axis.title.y = element_text(color="black", size=10, face="bold")) +
                                  xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') + 
                                  ggtitle("% of Population: HS Dip") + scale_color_viridis_d(), tooltip = "text")

      
      
      # College and above
      EducationCollPlus <- ggplotly(ggplot(data = appal2, aes(x = observation, y = Coll_Plus, colour = nonmetro.f, names=NAME, text = str_c(NAME, ": ", Coll_Plus))) + geom_point()  + 
                                      geom_hline(data = g, aes(yintercept=M_Coll_Plus, color= "black")) + 
                                      facet_wrap( nonmetro.f~.)  + 
                                      theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),                                                                                                                                                                                                                                          axis.title.y = element_text(color="black", size=10, face="bold")) +
                                      xlab("County") + ylab("Percent in County (%)") + 
                                      labs(color='County Classification') + 
                                      ggtitle("% of Population: Less than High School, High School, College or More") + scale_colour_viridis_d(), tooltip = "text")
      



 subplot( EducationLTHS,   EducationHSDP, EducationCollPlus, nrows = 3,  shareY=FALSE, titleX = TRUE, titleY=TRUE)                     
      
    }
#### Age ----
    else if(supplyvar() == "supply2"){
      AgeUnder15 <- ggplotly(ggplot(data = appal2, aes(x = observation, y = age0_14, colour = nonmetro.f, names=NAME, text = str_c(NAME,": ", age0_14))) + 
                               geom_point()  +  
                               geom_hline(data = g, aes(yintercept=M_age0_14, color= "black")) + 
                               facet_wrap( nonmetro.f~.)  + theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),  
                                                                              axis.title.x = element_text(color="black", size=8, face="bold"), 
                                                                              axis.title.y = element_text(color="black", size=10, face="bold")) +
                               xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') + 
                               ggtitle("% of Population: Age 0-14") +
                               scale_color_viridis_d(), tooltip = "text")
 
      
      #15 to 64
      Age15_64<- p2 <- ggplotly(ggplot(data = appal2, aes(x = observation, y = age15_64, colour = nonmetro.f, names=NAME, text = str_c(NAME,": ", age15_64))) + geom_point() +  
                                  geom_hline(data = g, aes(yintercept=M_age15_64, color= "black")) + facet_wrap( nonmetro.f~.)  + 
                                  theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),                                                                                                                                                                                                                                               axis.title.y = element_text(color="black", size=10, face="bold")) +
                                  xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') +
                                  ggtitle("% of Population: Age 15-64") +
                                  scale_color_viridis_d(), tooltip = "text")

      
      #65 Plus 
      Age65Plus <- ggplotly(ggplot(data = appal2, aes(x = observation, y = age65plus, colour = nonmetro.f, names=NAME, text = str_c(NAME,": ", age15_64))) + geom_point()  +  
                              geom_hline(data = g, aes(yintercept=M_age65plus, color="black")) + facet_wrap( nonmetro.f~.)  + 
                              theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),
                                                axis.title.x = element_text(color="black", size=8, face="bold"),
                                                axis.title.y = element_text(color="black", size=10, face="bold")) +
                              xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') +
                              ggtitle("% of Population: 0-14, 15-65, Age 65+") + scale_color_viridis_d(), tooltip = "text")

      # Arrange 
      subplot(AgeUnder15, Age15_64, Age65Plus, nrows = 3, shareY=FALSE, titleX = TRUE, titleY=TRUE)
    }
    #### Disability -----
    else if(supplyvar() == "supply3"){
      Disabled <- ggplotly(ggplot(data = appal2, aes(x = observation, y = Pct.Dis, colour = nonmetro.f, names=NAME, text = str_c (NAME, ": ", Pct.Dis))) +
                             geom_point()  +  
                             geom_hline(data = g, aes(yintercept=M_Pct.Dis, color="black")) + facet_wrap( nonmetro.f~.)  + 
                             theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),                                                                                                                                                                                                                                   axis.title.y = element_text(color="black", size=10, face="bold")) +
                             xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') + 
                             ggtitle("% of Population: Disability") + scale_colour_viridis_d(), tooltip = "text")
      
      Disabled
    }
    
    else if(supplyvar() == "supply4"){
      HealthInsurance <- ggplotly(ggplot(data = appal2, aes(x = observation, y = Pct.HI, colour = nonmetro.f, names=NAME, text = str_c(NAME, ": ", Pct.HI))) + 
                                    geom_point()  +  
                                    geom_hline(data = g, aes(yintercept=M_Pct.HI, color= "black")) + facet_wrap( nonmetro.f~.)  + 
                                    theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),                                                                                                                                                                                                                                             axis.title.y = element_text(color="black", size=10, face="bold")) +
                                    xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') + 
                                    ggtitle("% of Population: Health Insurance Coverage") + scale_color_viridis_d(), tooltip = "text")
      
      HealthInsurance
    }
    
    else if(supplyvar() == "supply5"){
      HealthInsurance <- ggplotly(ggplot(data = appal2, aes(x = observation, y = Pct.HI, colour = nonmetro.f, names=NAME, text = str_c(NAME, ": ", Pct.HI))) + 
                                    geom_point()  +  
                                    geom_hline(data = g, aes(yintercept=M_Pct.HI, color= "black")) + facet_wrap( nonmetro.f~.)  + 
                                    theme_bw()+ theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),                                                                                                                                                                                                                                             axis.title.y = element_text(color="black", size=10, face="bold")) +
                                    xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') + 
                                    ggtitle("% of Population: Health Insurance Coverage") + scale_color_viridis_d(), tooltip = "text")
      
      HealthInsurance
    }
  
  })
  
  
  ######## End of Supply Variables
  
  demandvar <- reactive({
    input$demand
  })
  
  ## output:laboroutput---------------
  output$demandoutput <- renderPlotly({
    
    ##### Income ------
    if(demandvar() == "demand1") {
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
    }
    
    #### Age ----
    else if(demandvar() == "demand2"){
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
    }
    
    else if(demandvar() == "demand3"){
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
    }
    
    
  })
  
  
  othervar <- reactive({
    input$other
  })
  
  output$otheroutput <- renderPlotly({
    
    if(othervar() == "other1") {
      HomeOwnership <- ggplotly(ggplot(data = appal2, aes(x = observation, y = OwnHome, colour = nonmetro.f, names=NAME, text = str_c(NAME, ": ", observation))) + 
                                  geom_point()  +  geom_hline(data = g, aes(yintercept=M_OwnHome, color= "black")) + 
                                  facet_wrap( nonmetro.f~.)  + theme_bw()+ 
                                  theme(axis.text.x = element_blank(), legend.position = "none", plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),                                                                                                                                                                                                                                            axis.title.y = element_text(color="black", size=10, face="bold")) +
                                  xlab("County") + ylab("Percent in County (%)") + labs(color='County Classification') + 
                                  ggtitle("% of Population: Owns a Home") + scale_color_viridis_d(), tooltip = "text")
      HomeOwnership
    }
    
    else if(othervar() == "other2"){
   
    }
    
    else if(othervar() == "other3"){
    
    }
    
    
  })
  
  #skills example  table ------------------------------------------------------------
  output$example_table <- renderPlot({
  
    example_table <- matrix(data = NA, nrow = 3, ncol  =7)

   colnames(example_table) <- c("Number of Workers", "Public Speaking Proficiency", "Labor Distribution \n Weighted Public Speaking \n Skill Prevalance Value", "Normalized Weighted\n Public Speaking Skill \n Prevalence Index", "Manuel Labor Proficiency","Labor Distribution \n Weighted Manual Labor \n Skill Prevalence Value", "Normalized Weighted \n Manual Labor \n Skill Prevalence Index") 
  row.names(example_table) <- c("Teacher", "Coal Miner", "Community Skill Index")
  example_table[1,] <- c(5, 0.95, 4.75, "  ", 0.3, 1.5, "  ")
  example_table[2,] <- c(100, 0.1, 10, "  ", 0.8, 80, "  ")
  example_table[3,] <- c(105, " ", 14.75, 0.15, " ", 81.5, 0.85)
  example_table <- tableGrob(example_table)   
  example_table <- as_ggplot(example_table)
    example_table
  },
  height = 150
  )
  

  
  
  ############# END LABOR MARKET FILES, BEGIN SKILLS FILES
  var <- reactive({
    input$skills
  })
  ## output:skillsoutput---------------
  output$skillsoutput <- renderLeaflet({
     # Map for technology design ----
    if(var() == "TechnologyDesign") {
      
      # Filter to just Tech Design
      TechDesign_map_data <- map_data %>% filter(skillname == "TechnologyDesign")
      
      # Make simple feature
      TechDesign_map_data <- st_as_sf(TechDesign_map_data) 
      
      # Create palette for map
      TechDesign_map_pal <- colorNumeric(palette = "viridis", domain = TechDesign_map_data$`Normalized Index`)
      
      # Add labels 
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
      # add popup
      TechDesign_map <- TechDesign_map %>% 
        # addPopupGraphs(popup_plot, group = "PUMAs", width = 700, height = 350) %>% 
        addCircleMarkers(data = city_info,lng = ~lon,
                         lat = ~lat,
                         label = labels, 
                         radius = ~Population/50000, 
                         color = "blue")
      TechDesign_map
      
      # Map for Critical Thinking -------
    }else if(var() == "ReadingComprehension"){
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
      # add popup
      ReadingComp_map <- ReadingComp_map %>% 
        # addPopupGraphs(popup_plot, group = "PUMAs", width = 700, height = 350)%>% 
        addCircleMarkers(data = city_info,lng = ~lon,
                         lat = ~lat,
                         label = labels, 
                         radius = ~Population/50000, 
                         color = "blue")
      ReadingComp_map
      
      #### Map for Organization  ------
    }else if(var() == "Monitoring"){
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
      #add popup
      Monitoring_map <- Monitoring_map %>%
        # addPopupGraphs(popup_plot, group = "PUMAs", width = 700, height = 350)%>% 
        addCircleMarkers(data = city_info,lng = ~lon,
                         lat = ~lat,
                         label = labels,
                         radius = ~Population/50000,
                         color = "blue")
      Monitoring_map
      
      #Map for labor
    }else if(var() == "Coordination"){
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
      #add popup
      Coordination_map <- Coordination_map %>% 
        # addPopupGraphs(popup_plot, group = "PUMAs", width = 700, height = 350)%>% 
        addCircleMarkers(data = city_info,lng = ~lon,
                         lat = ~lat,
                         label = labels, 
                         radius = ~Population/50000, 
                         color = "blue")
      Coordination_map
      
      # Communication map
    }else if(var() == "ActiveListening"){
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
      
      # add popup
      ActiveList_map <-  ActiveList_map %>% 
        # addPopupGraphs(popup_plot, group = "PUMAs", width = 700, height = 350)%>% 
        addCircleMarkers(data = city_info,lng = ~lon,
                         lat = ~lat,
                         label = labels, 
                         radius = ~Population/50000, 
                         color = "blue")
      
      ActiveList_map
    }
  })
}


# Run the App-------------

shinyApp(ui = ui, server = server)

