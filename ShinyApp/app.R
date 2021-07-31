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
                                          p(strong("Education:"), "Blurb 1"),
                                          p("Blurb 2"),
                                          p("Blurb 1"),
                                          p("Blurb 2"),
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
                 # Tab Skills-----------------------------------------------------------
                 tabPanel("Skills", value = "skills",
                          
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Skills(need new title)"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                  
                                          h4(strong("This is a title")),
                                          p("Our project aims to create meaningful measures of employment and skills in the Appalachian labor market. We use individualized and anonymized 2019 5-Year American Community Survey to understand the occupations and industries of Appalachia. The most granular level for these data is the Public Use Microdata Area (PUMA) available in the Integrated Public Use Microdata Series (IPUMS). We use 6-digit SOC codes extracted from the ACS to obtain estimates of occupation prevalence in each area. "),
                                          br(),
                                          p("To understand and quantify the skills content of Appalachian labor markets, our project uses O*Net skills data. O*Net skills data ranks each of their 35 listed skills by importance and proficiency for 772 unique SOC occupation codes. Using 2019 skill rankings, we matched O*Net skills to the occupations found in Appalachia."),
                                           br(),
                                           p("Our team constructs an index to gain an understanding of the distribution of occupations in Appalachia. Our index identifies which skills are the most and the least prevalent in Appalachian communities. Within every occupation, each skill is assigned a value based on its importance to the job and the total number of individuals employed in that position. "),
                                           br(),
                                           p("Our index is to be interpreted as follows: If there is a skill that is very important to a very popular occupation and employees must have very high levels of proficiency in that skill, it will be assigned a higher value. If there is a skill that is not very important to an uncommon occupation and the required proficiency is very low, it will be assigned a lower value. These values represent relative importance to other skills within the PUMA. The index is limited in its ability to compare across areas. We can not say that one area is better at public speaking than another. We are able to compare the strengths and weaknesses of areas. "),
                                           br(),
                                           p("Our dashboard presents the prevalence of five skills we identified as important to the future economy. We use O*Net’s Bright Outlook Occupations—occupations which are “projected to grow faster than average (employment increase of 5% or more)” and “to have 100,000 or more job openings over the period 2019-2029 for the US nationwide”. Using these occupations, filtered and ranked skills to find and present those most relevant to our project goals."),
                                           br(),
                                           p("Moving forward, we anticipate that these indices can be consolidated and contribute to a multi-dimensional index that quantifies vulnerabilities ."),
                                          
                                  
                                          ## Input: skills------------
                                         selectInput("skills", "Select the Skill:", width = "100%", choices = c(
                                                      "Technology Design"="TechnologyDesign",
                                                      "Reading Comprehension"="ReadingComprehension",
                                                       "Monitoring",
                                                      "Coordination",
                                                         "Active Listening"="ActiveListening")),
                                                     p(strong("This is a title")),
                                         ## Output: skillsoutput ------------------------
                                          withSpinner(leafletOutput("skillsoutput")),
                                         p(tags$small("Data Sources: American Conmunity Survey??????????????????????????????????????")),
                                         h4(strong("A Simplified Example of the Index")),
                                         p("For example, suppose a labor market has two occupations—math teachers and coal miners— and each occupation has only two skill measures—public speaking and manual labor. Suppose the required proficiency of public speaking skills for math teachers is 0.95 on a 0-1 scale and the required proficiency of manual labor is 0.3 on a 0-1 scale. For coal miners, public speaking may only be 0.1 on a 0-1 scale and manual labor may be 0.8 on a 0-1 scale. Assume, for now, that the importance level is the same as the required proficiency. The data suggest that this is true with minimal variance. Without knowing how many coal miners and math teachers there are, we may assume that public speaking is the most prevalent skill in this labor market. However, suppose this labor market has 100 coal miners and only 5 math teachers. "),
                                         br(),
                                         p("We multiply each skill index for each occupation by the number of individuals employed in this occupation. For this hypothetical labor market, the weighted public speaking value for math teachers is 4.75 and the weighted public speaking value for coal miners is 10. The weighted manual labor value for math teachers is 1.5 and the weighted manual labor value for coal miners is 80. The public speaking skills index for this labor market is 14.75 and the manual labor skills index 81.5. To normalize these indices, we take the percentage of the whole and find that public speaking receives a value of .15 and manual labor receives a value of .85. In this economy, manual labor is much more prevalent than public speaking."),
                                         p("Our index also removes the assumption that importance to the occupation is the same as required proficiency. A job may require a low proficiency in typing even though it’s essential the employees type a few things everyday. In the construction of our index, we multiply the importance level by the required proficiency to compensate for some of these variances although they are uncommon. ")
                                            )
                     
                          ),
                 
                 #Tab Data -----------------------------------------------------------
                 tabPanel("Data and Measures", value = "data",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Data and Measures"), align = "center"),
                                   br()
                          ),
                          tabsetPanel(
                            tabPanel("Data Sources",
                                     h3("", align = "center"),
                                     br(""),
                                     column(4,
                                            img(src = "data-hifld.png", style = "display: inline; float: left;", width = "100px"),
                                            p(strong("Homeland Infrastructure Foundation-Level Data."), "Homeland Infrastructure Foundation-Level Data (HIFLD) is a collection of public
                                              source datasets at property level provided by the Department of Homeland Security. Since 2002, this HIFLD has provided quarterly
                                              updated layers on topics from education to energy, including on health care facilities. We used HIFLD emergency medical services
                                              station data at the latitude and longitude geographic level in our analyses."),
                                            br(""),
                                            img(src = "data-gmaps.png", style = "display: inline; float: left;", width = "130px"),
                                            p(strong("Google Maps."), "Google Maps is a comprehensive web mapping service created by Google. Its goal is to provide an interactive map
                                              of all the geographical contents of the world. This resource has a variety of uses, ranging from examining all service locations within
                                              a city to finding the quickest route between locations. It provides data at latitude and longitude level. We used Google Maps to locate
                                              all supermarkets, convenience stores, and farmers’ markets in Patrick County, and subsequently employed the information in calculating
                                              grocery access and coverage isochrones.")
                                     ),
                                     column(4,
                                            img(src = "data-acs.png", style = "display: inline; float: left;", width = "200px"),
                                            p(strong("American Community Survey."), "The American Community Survey (ACS) is an ongoing yearly survey conducted by the U.S Census
                                            Bureau. ACS samples households to compile 1-year and 5-year datasets providing information on population sociodemographic and
                                            socioeconomic characteristics including employment, disability, and health insurance coverage. We used AC 2014/18 5-year
                                            estimates to obtain census tract and census block group-level to explore Patrick County resident characteristics."),
                                            br(""),
                                            img(src = "data-connect.png", style = "display: inline; float: left;", width = "150px"),
                                            p(strong("CommonwealthConnect."), "The Virginia Tech CommonwealthConnect Wi-Fi Hotspot Map is an interactive map of free, publicly
                                           available wi-fi hotspots in Virginia. Its goal is to provide an easily accessible map of areas where individuals can connect to the
                                           internet for free, decreasing the constraints placed on families that do not have internet access at home. We used the 2020 wi-fi
                                           hotspot map data to retrieve hotspot locations in Patrick County and subsequently employed the information in calculating hotspot
                                           coverage isochrones."),
                                            br(""),
                                            img(src = "data-corelogic.png", style = "display: inline; float: left;", width = "120px"),
                                            p(strong("CoreLogic."), "CoreLogic is a supplier of proprietary US real estate and specialized business data at the property level.
                                           This company provides data spanning over 50 years at the latitude and longitude level. Information available in the dataset includes
                                           property characteristics, mortgage, foreclosures and performance. We used 2019 CoreLogic data to obtain the locations of all residential
                                           properties in Patrick County.")
                                     ),
                                     column(4,
                                            img(src = "data-traveltime.png", style = "display: inline; float: left;", width = "140px"),
                                            p(strong("TravelTime."), "TravelTime Application Programming Interface (API) aggregates data from OpenStreetMap, transport timetables and
                                           speed profiles to generate isochrones. An isochrone is a shape covering all locations that can be reached within the same timeframe
                                           given a start location, departure time, and a mode of transportation. We used the TravelTime API to produce isochrones of 10- and
                                           15-minute drive time interval from supermarkets, farmers' markets, and free wi-fi hotspots, and of 8-, 10-, and 12-minute drive
                                           time intervals from all emergency medical service stations in Patrick County."),
                                            br(""),
                                            img(src = "data-ers.png", style = "display: inline; float: left;", width = "120px"),
                                            p(strong("Food Access Research Atlas."), "The United State Department of Agriculture Food Access Research Atlas is a data resource
                                          created by the Economic Research Service that provides information on food access indicators at census tract level. The data allows
                                          individuals to understand food access in communities based on factors like age and socioeconomic status. We used the 2017 Food Access
                                          Research Atlas to examine Patrick County residents’ food access at multiple distance thresholds and by resident characteristics.")
                                     )
                            ),
                            tabPanel("Measures",
                                     h3(strong(""), align = "center"),
                                     p("Austin's index explanation. Updating."),
                                     
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Technical Creation of the Index"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              
                                              h4(strong("This is a title")),
                                              p("To gain an understanding of the distribution of occupations in Appalachia, a grouped summary was performed to obtain occupational counts by PUMA . The data was grouped by PUMA and SOC, creating groups of unique PUMA and SOC combinations.  An SOC Frequency summary column was created from the sums of the Person Weights in each group, producing the desired occupational counts by PUMA table."),
                                              p("The SOC’s available from O*Net’s 2019 Skills rankings use the 2010 SOC classification, which were largely incompatible with the SOC’s from the IPUMS data. To remedy this, the O*Net 2010 to 2019 SOC crosswalk was used to transform all 2010 SOC’s to 2019 SOC’s. The Skills data was then tidied using pivot_wider as to have each unique soc and skill combination take up one row in the Skills table and Importance and Level rankings were normalized on a 0 to 1 scale. "),
                                              p("The team thought it most practical to associate one value with each soc and skill combination, seeking to create an index that takes into account both the Importance and Level of a skill to its associated SOC. Treating Importance as a proxy for the probability of an individual in the SOC having the skill, a new index was created, taking the product of the normalized values of Importance and Level. To account for the prevalence of an occupation and better represent the importance and level of a skill as it pertains to Appalachian PUMAS, a density index was created. To create this, the socs, skills and associated indices were inner-joined with the occupational counts. By taking the product of the soc frequency in each PUMA and the index value of each skill and SOC combination, a new density index was created for each soc in every PUMA. This density index was then normalized on a scale from 0 to 1."),
                                              p("The Bright Outlook Occupations were read into R, and SOC codes were altered slightly to best fit the SOC codes of our skill index. The skill index was then limited to only include the SOC’s of those listed in the Bright Outlook Occupations, creating a “skills of the future” index table. To best rank these skills of the future, the table was grouped by skill name, and the indices summed, creating a new  importance index  for the skills of the future. Breaking these skills down into our categories, the team was able to identify the most relevant skills to the future in each skill category."),
                                              p("To visualize the distribution of relative importance of skills of the future across Appalachian  PUMAs, interactive choropleth maps were created, with the color of each PUMA representing its respective index value of the skill of the future. Popups were then added to the map, allowing the user to visualize the industry breakdown of each PUMA and possibly better understand the effects of the PUMAs industry makeup on its index value of each skill of the future. To do this, the team utilized the “Industry by Occupation for the Civilian Population” table at the PUMA level from 2019 5-year ACS estimates. Pie Charts for the estimate of proportion of PUMA population in each industry were added to each PUMA using the Leafpop library’s addPopupGraph function. ")
                                              
                                     )
                                     
                                     
                            )
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

