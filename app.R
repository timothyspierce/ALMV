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
                 useShinyjs(),
                 
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
                                          h2(strong("Project Background")),
                                          p(strong("The problem."), "Rural counties often face challenges in providing health care access to their residents given limited", a(href = "https://www.ruralhealthinfo.org/topics/hospitals", "health facilities", target = "_blank"),
                                            "available, lack of broadband infrastructure that makes it difficult to provide", a(href = "https://www.ruralhealthinfo.org/topics/telehealth", "telemedicine access", target = "_blank"), "or communicate health information, and individual-level",
                                            a(href = "https://www.ruralhealthinfo.org/topics/social-determinants-of-health", "inequalities", target = "_blank"), "that pose barriers to health care use and health
                                            behaviors. Identifying areas of high need or potential solutions may also be difficult for rural areas without adequate resources to acquire, analyze, and interpret
                                            relevant data."),
                                          p(),
                                          p(strong("The setting."), a(href = "https://www.co.patrick.va.us/", "Patrick County", target = "_blank"), "is a rural area in Virginia’s Central Piedmont, bordering North Carolina,
                                            with a declining population of approximately 17,600 people. Like many other rural areas in the United States, Patrick County is having difficulty meeting its residents’ health and quality of life needs.
                                            The county’s", a(href = "https://www.countyhealthrankings.org/app/virginia/2019/rankings/patrick/county/outcomes/overall/snapshot", "doctor to patient ratios", target = "_blank"),
                                            "of 3,530 to 1 for primary care providers, 8,840 to 1 for dentists, and 2,520 to 1 for mental health providers are 3-
                                            to 8-times higher than statewide, and the county’s only hospital closed in 2017. At the same time, the median income for Patrick County residents is $42,900,
                                            46% of children living in the county are eligible for free or reduced-price school lunch, and 12% of residents are food insecure."),
                                          p(),
                                          p(strong("The project."), "This University of Virginia", a(href = "https://biocomplexity.virginia.edu/social-decision-analytics", "Biocomplexity Institute", target = "_blank"),
                                            "Data Science for Public Good (DSPG) project aimed to build local capacity, leverage social and data science to address current and future resident well-being, and enhance
                                             data-driven decision making about rural health in Patrick County, Virginia.")
                                   ),
                                   column(4,
                                          h2(strong("Our Work")),
                                          p("Our research team worked closely with Patrick County Extension Office, Virginia Department of Health, and Healthy Patrick County coalition stakeholders
                                            to identify the county’s priority challenges in the area of health. The research team reviewed a prior", a(href = "https://www.vdh.virginia.gov/west-piedmont/2020/05/27/patrick-county-health-needs-improvement-plan-completed/",
                                                                                                                                                       "community health assessment,", target = "blank"), a(href = "https://www.pubs.ext.vt.edu/VCE/VCE-596/VCE-596-75/VCE-1002-75.html", "situation analysis", target = "_blank"),
                                            "relevant funding applications, and held a listening meeting with stakeholders to identify these challenges. Lack of
                                            data on health care access, food access as related to diabetes and heart disease prevalence, older adult health, and digital connectivity that would facilitate
                                            access to telemedicine emerged as key problems where providing actionable insights could address barriers to Patrick County residents’ health."),
                                          p(),
                                          p("We implemented the", a(href = "https://doi.org/10.1162/99608f92.2d83f7f5", "data science framework", target = "_blank"), "and identified, acquired, profiled, and used
                                            publicly available data to provide Patrick County with data-driven resources in each of the four priority areas. We:"),
                                          tags$li("Provided census tract- and census block group-level maps of Patrick County residents'", strong("sociodemographic and socioeconomic characteristics,"), " highlighting underprivileged areas."),
                                          tags$li("Created census tract-level maps on", strong("older adult health"), "to show the geographic distribution of older adults in the county by gender and
                                                  type of disability, identifying areas where providing telehealth or travelling preventive care services may be particularly important."),
                                          tags$li("Mapped residents'", strong("computing device and internet access"), "at census block group level, and constructed 10- and 15-minute isochrones (areas of equal travel time) from households to free
                                                  wifi hotspots to highlight internet gaps that could suggest where new wi-fi hotspots could be optimally placed to provide internet access to more residents."),
                                          tags$li("Calculated and mapped", strong("emergency medical service (EMS) station coverage"), "of households within 8-, 10-, and 12-minute travel times, identifying areas difficult to reach within
                                                   standard EMS travel thresholds."),
                                          tags$li("Constructed", strong("food access"), "maps by census tract, 10- and 15-minute isochrones from households to grocery stores and farmers markets, and maps of food security resources in the county,
                                                highlighting food deserts and areas that could benefit from programs facilitating access to fresh produce."),
                                          p(),
                                          p("This dashboard compiles our findings and allows extension professionals, stakeholders, and other users to explore the information interactively.")
                                   ),
                                   column(4,
                                          h2(strong("Dashboard Aims")),
                                          p("Our dashboard is aimed at:"),
                                          p(strong("Patrick County extension professionals and the communities they serve."), "Information available through the interface helps extension
                                            agents identify areas where residents may not have access to internet, or areas with a high smartphone ownership share, suggesting what channels agents may
                                            want to use to disseminate health-related information most effectively. Information on older adult populations and grocery store access can help extension agents
                                            better understand where underserved populations live and how to advocate on their behalf."),
                                          p(strong("Local health-related agencies and departments seeking data insights to inform their decision-making."), "For local stakeholders, identifying broadband
                                            access gaps that limit access to telemedicine, grocery store access gaps, and areas with high proportions of older adults with independent living difficulty can suggest
                                            optimal locations for placing free wifi hotspots, providing grocery delivery services, devising mobile health unit routes, or can inform other solutions that would benefit
                                            a broad population base."),
                                          p(strong("State government representatives in the Virginia Department of Health and the State Office of Rural Health."), "These and similar stakeholders may
                                            need small or rural area-specific insights that Centers for Disease Control and other county-level datasets cannot provide.")
                                   )
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2021'))))
                 ),
                 # Tab Labor Market -----------------------------------------------------------
                 tabPanel("Appalachian Labor Market", value = "labomarket",
                          
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Appalachian Labor Market Charateristics"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(4,
                                          h4(strong("Definition of Appalachian Region")),
                                          p("Blurb 1"),
                                          p("Blurb 2"),
                                          h4(strong("Appalachian Labor Market")),
                                          p("Blurb 1"),
                                          p("Blurb 2"),
                                          h4(strong("Comparisions with other resgion?")),
                                          p("Blurb 1"),
                                          p("Blurb 2")
                                          
                                   ),
                                   column(8,
                                          tabsetPanel(
                                            tabPanel("Supply",
                                                     p(""),
                                                     ## Input: supply-----------------
                                                     selectInput("supply", "", width = "100%",selected="Educaion", choices = c(
                                                       "Education" = "STUART VOLUNTEER FIRE DEPARTMENT",
                                                       "Educational attainment" = "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT",
                                                       "Age Distribution"="AGE",
                                                       "Disability %" = "STUART VOLUNTEER FIRE DEPARTMENT",
                                                       "Healthcare Coverage %" = "BLUE RIDGE VOLUNTEER RESCUE SQUAD",
                                                       "Internet Access" = "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT",
                                                       "Broadband Access" = "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT",
                                                       "Travel Time" = "STUART VOLUNTEER FIRE DEPARTMENT"
                                                       
                                                   )),
                                                   p(strong("Ryan with the help of Leo-Allen")),
                                                   ## Output: 1-----------------
                                                   withSpinner(tableOutput("output1")),
                                                   p(tags$small("Data Sources: Homeland Infrastructure Foundation-Level Data, 2010; CoreLogic, 2019; TravelTime API."))
                                            ),
                                            tabPanel("Demand",
                                                     p(""),
                                                     ## Input: demand-----------------
                                                     selectInput("demand", "", width = "100%",selected="Income per capita", choices = c(
                                                       "Income per capita" = "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT",
                                                       "Unemployment" = "BLUE RIDGE VOLUNTEER RESCUE SQUAD", 
                                                       "Industrial sectors" = "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT",
                                                       "Technology?????" = "BLUE RIDGE VOLUNTEER RESCUE SQUAD"
                                                     )),
                                                     p(strong("Ryan with the help of Leo-Allen")),
                                                     ## Output: 2-----------------
                                                     withSpinner(tableOutput("output2")),
                                                     p(tags$small("Data Sources: Homeland Infrastructure Foundation-Level Data, 2010; CoreLogic, 2019; TravelTime API."))
                                            ),
                           
                                            tabPanel("Others",
                                                     p(""),
                                                     ## Input: others-----------------
                                                     selectInput("others", " ", selected = "Home Ownership",width = "100%", choices = c(
                                                       "Home Ownership" = "STUART VOLUNTEER FIRE DEPARTMENT",
                                                       "Renters" = "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT",
                                                       "Vehicle Ownership" = "BLUE RIDGE VOLUNTEER RESCUE SQUAD"
                                                     )),
                                                     p(strong("Ryan with the help of Leo-Allen")),
                                                     ## Output: 3-----------------
                                                     withSpinner(tableOutput("output3")),
                                                     p(tags$small("Data Sources: Homeland Infrastructure Foundation-Level Data, 2010; CoreLogic, 2019; TravelTime API."))
                                            )
                                            
                                          )
                                   )
                          )
                 ),
                 # Tab Skills-----------------------------------------------------------
                 tabPanel("Skills", value = "skills",
                          
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Skills(need new title)"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(4,
                                          h4(strong("This is a title")),
                                          p("Blurb 1."),
                                          br()),
                                   column(8,
                                          h4(strong("Description of this visulization.")),
                                          p("Blurb 2"),
                                          br(),
                                          p(""),
                                          ## Input: skills------------
                                         selectInput("skills", "Select the Skill:", width = "100%", choices = c(
                                                      "TechnologyDesign",
                                                      "ReadingComprehension",
                                                       "Monitoring",
                                                      "Coordination",
                                                         "ActiveListening")),
                                                     p(strong("This is a title")),
                                                     ## Output: skillsoutput ------------------------
                                                     withSpinner(leafletOutput("skillsoutput")),
                                                     p(tags$small("Data Sources: CommonwealthConnect, 2020; CoreLogic, 2019; TravelTime API."))
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
                                     p("Austin's index explanation. In the word file, updating.")
                                     
                                     
                            )
                          )
                 ),
                 
                 # Tab contact -----------------------------------------------------------
                 tabPanel("Contact", value = "contact",
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                                   h1(strong("Contact"), align = "center"),
                                   br(),
                                   h4(strong("VT Data Science for the Public Good")),
                                   p("The", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics/dspg-program', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"),
                                     "is a summer immersive program held at the", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics', 'University of Virginia Biocomplexity Institute’s Social and Decision Analytics division (SDAD).'),
                                     "In its seventh year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around
                              critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences
                              to determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program
                              highlights, how to apply, and our annual symposium, please visit", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics/dspg-program', 'the official Biocomplexity DSPG website.', target = "_blank")),
                                   p("", style = "padding-top:10px;")
                          ),
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                                   column(6, align = "center",
                                          h4(strong("DSPG Team Members")),
                                          img(src = "team-morgan.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-tasfia.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-isabel.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = 'https://www.linkedin.com/in/morgan-stockham/', 'Morgan Stockham', target = '_blank'), "(Claremont Graduate University, Applied Microeconomics);",
                                            a(href = 'https://www.linkedin.com/in/tasfia-chowdhury-89005a1b2/', 'Tasfia Chowdhury', target = '_blank'), "(Indiana University Bloomington, Political Science);",
                                            a(href = 'https://www.linkedin.com/in/igomez-3099/', 'Isabel Gomez', target = '_blank'), "(Smith College, Statistical and Data Science)."),
                                          p("", style = "padding-top:10px;")
                                   ),
                                   column(6, align = "center",
                                          h4(strong("UVA SDAD Team Members")),
                                          img(src = "team-teja.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-brandon.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-sallie.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = "https://www.linkedin.com/in/tejapristavec/", 'Teja Pristavec', target = '_blank'), "(Project Lead, Research Assistant Professor);",
                                            a(href = "https://biocomplexity.virginia.edu/brandon-kramer", 'Brandon Kramer', target = '_blank'), "(Postdoctoral Research Associate);",
                                            a(href = 'https://biocomplexity.virginia.edu/sallie-keller', 'Sallie Keller', target = '_blank'), "(Division Director and Distinguished Professor)."),
                                          p("", style = "padding-top:10px;")
                                   )
                          ),
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                                   h4(strong("Project Stakeholders")),
                                   p(a(href = 'https://www.linkedin.com/in/nancy-bell-aa293810/', 'Nancy Bell', target = '_blank'), "(Virginia Department of Health);",
                                     a(href = 'https://www.linkedin.com/in/terri-alt-3138b4101/', 'Terri Alt', target = '_blank'), "(Virginia Cooperative Extension, Patrick County at Virginia Tech)."),
                                   p("", style = "padding-top:10px;"),
                                   h4(strong("Acknowledgments")),
                                   p("We would like to thank Healthy Patrick County, an association of concerned Patrick County residents, and Brandon Kramer for their input to this project.")
                          )
                 ),
                 inverse = T)

# server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  runjs(jscode)
  
  # skills map: in pgrogress -----------------------------------------------------

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
  
  ## input: kills-----------
  var <- reactive({
    input$skills
  })
  
  ## output:skillsoutput---------------
  output$skillsoutput <- renderLeaflet({
     # Map for technology design
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
      TechDesign_map %>% 
        addPopupGraphs(popup_plot, group = "PUMAs", width = 700, height = 350) %>% 
        addCircleMarkers(data = city_info,lng = ~lon,
                         lat = ~lat,
                         label = labels, 
                         radius = ~Population/50000, 
                         color = "blue")
      
      # Map for Critical Thinking
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
      ReadingComp_map %>% 
        addPopupGraphs(popup_plot, group = "PUMAs", width = 700, height = 350)%>% 
        addCircleMarkers(data = city_info,lng = ~lon,
                         lat = ~lat,
                         label = labels, 
                         radius = ~Population/50000, 
                         color = "blue")
      
      # Map for Organization 
    }else if(var() == "totalpop_trct"){
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
      Monitoring_map %>% 
        addPopupGraphs(popup_plot, group = "PUMAs", width = 700, height = 350)%>% 
        addCircleMarkers(data = city_info,lng = ~lon,
                         lat = ~lat,
                         label = labels, 
                         radius = ~Population/50000, 
                         color = "blue")
      
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
      Coordination_map %>% 
        addPopupGraphs(popup_plot, group = "PUMAs", width = 700, height = 350)%>% 
        addCircleMarkers(data = city_info,lng = ~lon,
                         lat = ~lat,
                         label = labels, 
                         radius = ~Population/50000, 
                         color = "blue")
      
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
        addPopupGraphs(popup_plot, group = "PUMAs", width = 700, height = 350)%>% 
        addCircleMarkers(data = city_info,lng = ~lon,
                         lat = ~lat,
                         label = labels, 
                         radius = ~Population/50000, 
                         color = "blue")
      
      
    }
  })
  
  
  # # data and measures table:  ----------------------------------------
  # var_topic <- reactive({
  #   input$topic
  # })
  # output$datatable <- renderDataTable({
  #   if(var_topic() == "All Measures"){
  #     table <- as.data.frame(measures_table)
  #     datatable(table, rownames = FALSE, options = list(pageLength = 15)) %>% formatStyle(0, target = 'row', lineHeight = '80%')
  #   }
  #   else{
  #     data <- switch(input$topic,
  #                    "Connectivity Measures" = "connectivity",
  #                    "Sociodemographic Measures" = "demographics",
  #                    "Food Access Measures" = "food access",
  #                    "Health Care Access Measures" = "health",
  #                    "Older Adult Population Measures" = "older adults")
  #     table <- subset(measures_table, Topic == data)
  #     table <- as.data.frame(table)
  #     datatable(table, rownames = FALSE, options = list(pageLength = 15)) %>% formatStyle(0, target = 'row', lineHeight = '80%')
  #   }
  # })
  # 
  # # device: done ---------------------------------------------------------
  # 
  # output$deviceplot <- renderLeaflet({
  #   data <- switch(input$devicedrop,
  #                  "nocomputer" = connectivity$nocomputer,
  #                  "laptop" = connectivity$laptop,
  #                  "smartphone" = connectivity$smartphone,
  #                  "tablet" = connectivity$tablet,
  #                  "nointernet" = connectivity$nointernet,
  #                  "satellite" = connectivity$satellite,
  #                  "cellular" = connectivity$cellular,
  #                  "broadband" = connectivity$broadband)
  #   
  #   device_spec <- switch(input$devicedrop,
  #                         "nocomputer" = "no computer",
  #                         "laptop" = "laptop",
  #                         "smartphone" = "smartphone",
  #                         "tablet" = "tablet",
  #                         "nointernet" = "no internet access",
  #                         "satellite" = "satellite internet",
  #                         "cellular" = "cellular internet",
  #                         "broadband" = "broadband internet")
  #   
  #   pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 6), right = TRUE)
  #   
  #   labels <- lapply(
  #     paste("<strong>Area: </strong>",
  #           connectivity$NAME.y,
  #           "<br />",
  #           "<strong>% Households with",
  #           device_spec,
  #           "access: </strong>",
  #           round(data, 2)),
  #     htmltools::HTML
  #   )
  #   
  #   leaflet(data = connectivity, options = leafletOptions(minZoom = 10))%>%
  #     addProviderTiles(providers$CartoDB.Positron) %>%
  #     addPolygons(fillColor = ~pal(data),
  #                 fillOpacity = 0.7,
  #                 stroke = TRUE, weight = 0.5, color = "#202020",
  #                 label = labels,
  #                 labelOptions = labelOptions(direction = "bottom",
  #                                             style = list(
  #                                               "font-size" = "12px",
  #                                               "border-color" = "rgba(0,0,0,0.5)",
  #                                               direction = "auto"
  #                                             ))) %>%
  #     addLegend("bottomleft",
  #               pal = pal,
  #               values =  ~(data),
  #               title = "Percent by<br>Quintile Group",
  #               opacity = 0.7,
  #               labFormat = function(type, cuts, p) {
  #                 n = length(cuts)
  #                 paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
  #               })
  # })
  # 
  # 
  # # Iso selector
  # output$wifiplot <- renderLeaflet({
  #   colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
  #   
  #   wifi_iso10 <- switch(input$wifidrop,
  #                        "Meadows of Dan Elementary School" = wifi_iso_10_1,
  #                        "Woolwine Elementary School" = wifi_iso_10_2,
  #                        "Patrick Springs Primary School" = wifi_iso_10_3,
  #                        "Blue Ridge Elementary School" = wifi_iso_10_4,
  #                        "Patrick County High School" = wifi_iso_10_5,
  #                        "Stuart Elementary School" = wifi_iso_10_6,
  #                        "Patrick County Branch Library" = wifi_iso_10_7,
  #                        "Hardin Reynolds Memorial School" = wifi_iso_10_8,
  #                        "Stuart Baptist Church" = wifi_iso_10_9,
  #                        "Patrick Henry Community College Stuart Campus" = wifi_iso_10_10)
  #   
  #   wifi_iso15 <- switch(input$wifidrop,
  #                        "Meadows of Dan Elementary School" = wifi_iso_15_1,
  #                        "Woolwine Elementary School" = wifi_iso_15_2,
  #                        "Patrick Springs Primary School" = wifi_iso_15_3,
  #                        "Blue Ridge Elementary School" = wifi_iso_15_4,
  #                        "Patrick County High School" = wifi_iso_15_5,
  #                        "Stuart Elementary School" = wifi_iso_15_6,
  #                        "Patrick County Branch Library" = wifi_iso_15_7,
  #                        "Hardin Reynolds Memorial School" = wifi_iso_15_8,
  #                        "Stuart Baptist Church" = wifi_iso_15_9,
  #                        "Patrick Henry Community College Stuart Campus" = wifi_iso_15_10)
  #   
  #   data <- switch(input$wifidrop,
  #                  "Meadows of Dan Elementary School" = 1,
  #                  "Woolwine Elementary School" = 2,
  #                  "Patrick Springs Primary School" = 3,
  #                  "Blue Ridge Elementary School" = 4,
  #                  "Patrick County High School" = 5,
  #                  "Stuart Elementary School" = 6,
  #                  "Patrick County Branch Library" = 7,
  #                  "Hardin Reynolds Memorial School" = 8,
  #                  "Stuart Baptist Church" = 9,
  #                  "Patrick Henry Community College Stuart Campus" = 10)
  #   
  #   labels <- lapply(
  #     paste("<strong>Name: </strong>",
  #           wifi_latlong[data, ]$name,
  #           "<br />",
  #           "<strong>Address:</strong>",
  #           wifi_latlong[data, ]$fulladdress,
  #           "<br />",
  #           "<strong>Notes:</strong>",
  #           wifi_latlong[data, ]$notes),
  #     htmltools::HTML
  #   )
  #   
  #   m1 <- leaflet(options = leafletOptions(minZoom = 10)) %>%
  #     addProviderTiles(providers$CartoDB.Positron) %>%
  #     addCircles(data = residential,
  #                fillColor = colors[5],
  #                fillOpacity = .8,
  #                stroke = FALSE,
  #                group = "Residential Properties") %>%
  #     addPolygons(data = wifi_iso10,
  #                 fillColor = colors[1],
  #                 fillOpacity = .8,
  #                 stroke = FALSE,
  #                 group = "10 Minute Isochrone") %>%
  #     addPolygons(data = wifi_iso15,
  #                 fillColor = colors[2],
  #                 fillOpacity = .8,
  #                 stroke = FALSE,
  #                 group = "15 Minute Isochrone") %>%
  #     addMarkers(data = wifi_latlong, ~longitude[data], ~latitude[data],
  #                label = labels,
  #                labelOptions = labelOptions(direction = "bottom",
  #                                            style = list(
  #                                              "font-size" = "12px",
  #                                              "border-color" = "rgba(0,0,0,0.5)",
  #                                              direction = "auto")))  %>%
  #     addLayersControl(
  #       position = "topright",
  #       overlayGroups = c("10 Minute Isochrone",
  #                         "15 Minute Isochrone",
  #                         "Residential Properties"),
  #       options = layersControlOptions(collapsed = FALSE))
  #   m1
  # })
  # 
  # # Coverage table
  # output$wifitable <- renderTable({
  #   data <- switch(input$wifidrop,
  #                  "Meadows of Dan Elementary School" = 1,
  #                  "Woolwine Elementary School" = 2,
  #                  "Patrick Springs Primary School" = 3,
  #                  "Blue Ridge Elementary School" = 4,
  #                  "Patrick County High School" = 5,
  #                  "Stuart Elementary School" = 6,
  #                  "Patrick County Branch Library" = 7,
  #                  "Hardin Reynolds Memorial School" = 8,
  #                  "Stuart Baptist Church" = 9,
  #                  "Patrick Henry Community College Stuart Campus" = 10)
  #   
  #   table <- read.csv(paste0("data/isochrones/tables/wifi_iso_table_",data,".csv"))
  #   table$Coverage <- paste0(round(table$Coverage, 2), " %")
  #   table
  # }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  # 
  # # Wifi deserts
  # output$allwifi <- renderLeaflet({
  #   
  #   labels <- lapply(
  #     paste("<strong>Name: </strong>",
  #           wifi_latlong$name,
  #           "<br />",
  #           "<strong>Address:</strong>",
  #           wifi_latlong$fulladdress,
  #           "<br />",
  #           "<strong>Notes:</strong>",
  #           wifi_latlong$notes),
  #     htmltools::HTML
  #   )
  #   
  #   leaflet(options = leafletOptions(minZoom = 10)) %>%
  #     addProviderTiles(providers$CartoDB.Positron) %>%
  #     addCircles(data = residential,
  #                fillColor = colors[5],
  #                fillOpacity = .5,
  #                stroke = FALSE,
  #                group = "Residential Properties") %>%
  #     addPolygons(data = wifi_iso_10_1,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "10 Minute Isochrones") %>%
  #     addPolygons(data = wifi_iso_10_2,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "10 Minute Isochrones") %>%
  #     addPolygons(data = wifi_iso_10_3,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "10 Minute Isochrones") %>%
  #     addPolygons(data = wifi_iso_10_4,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "10 Minute Isochrones") %>%
  #     addPolygons(data = wifi_iso_10_5,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "10 Minute Isochrones") %>%
  #     addPolygons(data = wifi_iso_10_6,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "10 Minute Isochrones") %>%
  #     addPolygons(data = wifi_iso_10_7,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "10 Minute Isochrones") %>%
  #     addPolygons(data = wifi_iso_10_8,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "10 Minute Isochrones") %>%
  #     addPolygons(data = wifi_iso_10_9,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "10 Minute Isochrones") %>%
  #     addPolygons(data = wifi_iso_15_1,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "15 Minute Isochrones") %>%
  #     addPolygons(data = wifi_iso_15_2,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "15 Minute Isochrones") %>%
  #     addPolygons(data = wifi_iso_15_3,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "15 Minute Isochrones") %>%
  #     addPolygons(data = wifi_iso_15_4,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "15 Minute Isochrones") %>%
  #     addPolygons(data = wifi_iso_15_5,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "15 Minute Isochrones") %>%
  #     addPolygons(data = wifi_iso_15_6,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "15 Minute Isochrones") %>%
  #     addPolygons(data = wifi_iso_15_7,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "15 Minute Isochrones") %>%
  #     addPolygons(data = wifi_iso_15_8,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "15 Minute Isochrones") %>%
  #     addPolygons(data = wifi_iso_15_9,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "15 Minute Isochrones") %>%
  #     addPolygons(data = wifi_iso_15_10,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "15 Minute Isochrones") %>%
  #     addMarkers(data = wifi_latlong,
  #                group = "Free Wi-Fi Locations",
  #                label = labels,
  #                labelOptions = labelOptions(direction = "bottom",
  #                                            style = list(
  #                                              "font-size" = "12px",
  #                                              "border-color" = "rgba(0,0,0,0.5)",
  #                                              direction = "auto")))  %>%
  #     addLayersControl(
  #       position = "topright",
  #       overlayGroups = c("Free Wi-Fi Locations",
  #                         "Residential Properties"),
  #       baseGroups = c("10 Minute Isochrones",
  #                      "15 Minute Isochrones"),
  #       options = layersControlOptions(collapsed = FALSE))
  # })
  # 
  # output$allwifitable <- renderTable({
  #   table <- read.csv("data/isochrones/tables/wifi_iso_table.csv")
  #   table$Coverage <- paste0(round(table$Coverage, 2), " %")
  #   table
  # }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  # 
  #         # ems: done ----
  # 
  # output$emsplot <- renderLeaflet({
  #   colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
  #   
  #   ems_iso8 <- switch(input$emsdrop,
  #                      "STUART VOLUNTEER FIRE DEPARTMENT" = ems_iso_8_1,
  #                      "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = ems_iso_8_2,
  #                      "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = ems_iso_8_3,
  #                      "VESTA RESCUE SQUAD" = ems_iso_8_4,
  #                      "ARARAT RESCUE SQUAD" = ems_iso_8_5,
  #                      "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = ems_iso_8_6,
  #                      "JEB STUART RESCUE SQUAD" = ems_iso_8_7,
  #                      "SMITH RIVER RESCUE SQUAD" = ems_iso_8_8,
  #                      "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = ems_iso_8_9)
  #   
  #   ems_iso10 <- switch(input$emsdrop,
  #                       "STUART VOLUNTEER FIRE DEPARTMENT" = ems_iso_10_1,
  #                       "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = ems_iso_10_2,
  #                       "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = ems_iso_10_3,
  #                       "VESTA RESCUE SQUAD" = ems_iso_10_4,
  #                       "ARARAT RESCUE SQUAD" = ems_iso_10_5,
  #                       "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = ems_iso_10_6,
  #                       "JEB STUART RESCUE SQUAD" = ems_iso_10_7,
  #                       "SMITH RIVER RESCUE SQUAD" = ems_iso_10_8,
  #                       "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = ems_iso_10_9)
  #   
  #   ems_iso12 <- switch(input$emsdrop,
  #                       "STUART VOLUNTEER FIRE DEPARTMENT" = ems_iso_12_1,
  #                       "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = ems_iso_12_2,
  #                       "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = ems_iso_12_3,
  #                       "VESTA RESCUE SQUAD" = ems_iso_12_4,
  #                       "ARARAT RESCUE SQUAD" = ems_iso_12_5,
  #                       "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = ems_iso_12_6,
  #                       "JEB STUART RESCUE SQUAD" = ems_iso_12_7,
  #                       "SMITH RIVER RESCUE SQUAD" = ems_iso_12_8,
  #                       "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = ems_iso_12_9)
  #   
  #   data <- switch(input$emsdrop,
  #                  "STUART VOLUNTEER FIRE DEPARTMENT" = 1,
  #                  "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = 2,
  #                  "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = 3,
  #                  "VESTA RESCUE SQUAD" = 4,
  #                  "ARARAT RESCUE SQUAD" = 5,
  #                  "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = 6,
  #                  "JEB STUART RESCUE SQUAD" = 7,
  #                  "SMITH RIVER RESCUE SQUAD" = 8,
  #                  "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = 9)
  #   
  #   labels <- lapply(
  #     paste("<strong>Name: </strong>",
  #           str_to_title(ems[data, ]$NAME),
  #           "<br />",
  #           "<strong>Address:</strong>",
  #           str_to_title(ems[data, ]$ADDRESS), ",", str_to_title(ems[data, ]$CITY), ", VA", ems[data, ]$ZIP,
  #           "<br />",
  #           "<strong>Type:</strong>",
  #           str_to_title(ems[data, ]$NAICSDESCR)),
  #     htmltools::HTML
  #   )
  #   
  #   m1 <- leaflet(options = leafletOptions(minZoom = 10)) %>%
  #     addProviderTiles(providers$CartoDB.Positron) %>%
  #     addCircles(data = residential,
  #                fillColor = colors[5],
  #                fillOpacity = .8,
  #                stroke = FALSE,
  #                group = "Residential Properties") %>%
  #     addPolygons(data = ems_iso8,
  #                 fillColor = colors[1],
  #                 fillOpacity = .8,
  #                 stroke = FALSE,
  #                 group = "8 Minute Isochrone") %>%
  #     addPolygons(data = ems_iso10,
  #                 fillColor = colors[2],
  #                 fillOpacity = .8,
  #                 stroke = FALSE,
  #                 group = "10 Minute Isochrone") %>%
  #     addPolygons(data = ems_iso12,
  #                 fillColor = colors[2],
  #                 fillOpacity = .8,
  #                 stroke = FALSE,
  #                 group = "12 Minute Isochrone") %>%
  #     addMarkers(data = ems, ~LONGITUDE[data], ~LATITUDE[data],
  #                group = "EMS Locations",
  #                label = labels,
  #                labelOptions = labelOptions(direction = "bottom",
  #                                            style = list(
  #                                              "font-size" = "12px",
  #                                              "border-color" = "rgba(0,0,0,0.5)",
  #                                              direction = "auto"))) %>%
  #     addLayersControl(
  #       position = "topright",
  #       overlayGroups = c("8 Minute Isochrone",
  #                         "10 Minute Isochrone",
  #                         "12 Minute Isochrone",
  #                         "Residential Properties"),
  #       options = layersControlOptions(collapsed = FALSE))
  #   m1
  # })
  # 
  # output$emstable <- renderTable({
  #   data <- switch(input$emsdrop,
  #                  "STUART VOLUNTEER FIRE DEPARTMENT" = 1,
  #                  "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = 2,
  #                  "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = 3,
  #                  "VESTA RESCUE SQUAD" = 4,
  #                  "ARARAT RESCUE SQUAD" = 5,
  #                  "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = 6,
  #                  "JEB STUART RESCUE SQUAD" = 7,
  #                  "SMITH RIVER RESCUE SQUAD" = 8,
  #                  "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = 9)
  #   
  #   
  #   table <- read.csv(paste0("data/isochrones/tables/ems_iso_table_",data,".csv"))
  #   table$Coverage <- paste0(round(table$Coverage, 2), " %")
  #   table
  # }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  # 
  # # EMS deserts
  # output$allems <- renderLeaflet({
  #   
  #   labels <- lapply(
  #     paste("<strong>Name: </strong>",
  #           str_to_title(ems$NAME),
  #           "<br />",
  #           "<strong>Address:</strong>",
  #           paste0(str_to_title(ems$ADDRESS), ", ", str_to_title(ems$CITY), ", VA ", ems$ZIP),
  #           "<br />",
  #           "<strong>Type:</strong>",
  #           str_to_title(ems$NAICSDESCR)),
  #     htmltools::HTML
  #   )
  #   
  #   leaflet(options = leafletOptions(minZoom = 10)) %>%
  #     addProviderTiles(providers$CartoDB.Positron) %>%
  #     addCircles(data = residential,
  #                fillColor = colors[5],
  #                fillOpacity = .5,
  #                stroke = FALSE,
  #                group = "Residential Properties") %>%
  #     addPolygons(data = ems_iso_8_1,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "8 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_8_2,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "8 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_8_3,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "8 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_8_4,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "8 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_8_5,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "8 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_8_6,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "8 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_8_7,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "8 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_8_8,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "8 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_8_9,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "8 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_10_1,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "10 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_10_2,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "10 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_10_3,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "10 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_10_4,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "10 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_10_5,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "10 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_10_6,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "10 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_10_7,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "10 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_10_8,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "10 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_10_9,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "10 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_12_1,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "12 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_12_2,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "12 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_12_3,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "12 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_12_4,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "12 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_12_5,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "12 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_12_6,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "12 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_12_7,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "12 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_12_8,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "12 Minute Isochrones") %>%
  #     addPolygons(data = ems_iso_12_9,
  #                 fillColor = colors[1],
  #                 fillOpacity = .5,
  #                 stroke = FALSE,
  #                 group = "12 Minute Isochrones") %>%
  #     addMarkers(data = ems,
  #                group = "EMS Locations",
  #                label = labels,
  #                labelOptions = labelOptions(direction = "bottom",
  #                                            style = list(
  #                                              "font-size" = "12px",
  #                                              "border-color" = "rgba(0,0,0,0.5)",
  #                                              direction = "auto"))) %>%
  #     addLayersControl(
  #       position = "topright",
  #       baseGroups = c("8 Minute Isochrones",
  #                      "10 Minute Isochrones",
  #                      "12 Minute Isochrones"),
  #       overlayGroups = c("EMS Locations",
  #                         "Residential Properties"),
  #       options = layersControlOptions(collapsed = FALSE))
  # })
  # 
  # output$allemstable <- renderTable({
  #   table <- read.csv("data/isochrones/tables/ems_iso_table.csv")
  #   table$Coverage <- paste0(round(table$Coverage, 2), " %")
  #   table
  # }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  # 
}


# Run the App-------------

shinyApp(ui = ui, server = server)

