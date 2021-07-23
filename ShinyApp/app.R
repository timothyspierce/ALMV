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

prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 7)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

# data -----------------------------------------------------------
socdem_block <- readRDS("data/socdem_block.Rds")
socdem_block <- st_transform(socdem_block, '+proj=longlat +datum=WGS84')

socdem_tract <- readRDS("data/socdem_tract.Rds")
socdem_tract <- st_transform(socdem_tract, '+proj=longlat +datum=WGS84')

connectivity <- readRDS("data/connectivity.Rds")
connectivity <- st_transform(connectivity, '+proj=longlat +datum=WGS84')

ems <- readRDS("data/ems.Rds")
ems <- st_transform(ems, '+proj=longlat +datum=WGS84')

groceries <- readRDS("data/groceries.Rds")
groceries <- st_as_sf(groceries, coords = c("longitude", "latitude"))
st_crs(groceries) <- "+proj=longlat +datum=WGS84"
groceries <- st_transform(groceries, '+proj=longlat +datum=WGS84')
groceries <- subset(groceries, type == "farmers market" | type == "supermarket")
groceries_latlong <- readRDS("data/groceries.Rds")
groceries_latlong <- subset(groceries_latlong, type == "farmers market" | type == "supermarket")

otherfood <- readRDS("data/otherfood.Rds")
otherfood <- st_as_sf(otherfood, coords = c("longitude", "latitude"))
st_crs(otherfood) <- "+proj=longlat +datum=WGS84"
otherfood <- st_transform(otherfood, "+proj=longlat +datum=WGS84")

usda <- readRDS("data/usda.Rds")
usda <- st_transform(usda, '+proj=longlat +datum=WGS84')

wifi <- readRDS("data/wifi.Rds")
wifi <- st_as_sf(wifi, coords = c("longitude", "latitude"))
st_crs(wifi) <- "+proj=longlat +datum=WGS84"
wifi <- st_transform(wifi, '+proj=longlat +datum=WGS84')
wifi_latlong <- readRDS("data/wifi.Rds")

olderadults <- readRDS("data/olderadults.Rds")
olderadults <- st_transform(olderadults, '+proj=longlat +datum=WGS84')

residential <- readRDS("data/residential.Rds")
residential <- st_as_sf(residential, coords = c("longitude", "latitude"))
st_crs(residential) <- "+proj=longlat +datum=WGS84"
residential <- st_transform(residential, '+proj=longlat +datum=WGS84')

measures_table <- read_excel("data/Measures.xlsx")

patrickborder <- readRDS("data/patrickborder.Rds")
patrickborder <- st_transform(patrickborder, '+proj=longlat +datum=WGS84')

grc_iso_10_1 <- readRDS("data/isochrones/grocery/grc_iso_10_1.RDS")
grc_iso_10_2 <- readRDS("data/isochrones/grocery/grc_iso_10_2.RDS")
grc_iso_10_3 <- readRDS("data/isochrones/grocery/grc_iso_10_3.RDS")
grc_iso_10_4 <- readRDS("data/isochrones/grocery/grc_iso_10_4.RDS")
grc_iso_10_5 <- readRDS("data/isochrones/grocery/grc_iso_10_5.RDS")
grc_iso_10_6 <- readRDS("data/isochrones/grocery/grc_iso_10_6.RDS")
grc_iso_10_7 <- readRDS("data/isochrones/grocery/grc_iso_10_7.RDS")

grc_iso_15_1 <- readRDS("data/isochrones/grocery/grc_iso_15_1.RDS")
grc_iso_15_2 <- readRDS("data/isochrones/grocery/grc_iso_15_2.RDS")
grc_iso_15_3 <- readRDS("data/isochrones/grocery/grc_iso_15_3.RDS")
grc_iso_15_4 <- readRDS("data/isochrones/grocery/grc_iso_15_4.RDS")
grc_iso_15_5 <- readRDS("data/isochrones/grocery/grc_iso_15_5.RDS")
grc_iso_15_6 <- readRDS("data/isochrones/grocery/grc_iso_15_6.RDS")
grc_iso_15_7 <- readRDS("data/isochrones/grocery/grc_iso_15_7.RDS")

wifi_iso_10_1 <- readRDS("data/isochrones/wifi/wifi_iso_10_1.RDS")
wifi_iso_10_2 <- readRDS("data/isochrones/wifi/wifi_iso_10_2.RDS")
wifi_iso_10_3 <- readRDS("data/isochrones/wifi/wifi_iso_10_3.RDS")
wifi_iso_10_4 <- readRDS("data/isochrones/wifi/wifi_iso_10_4.RDS")
wifi_iso_10_5 <- readRDS("data/isochrones/wifi/wifi_iso_10_5.RDS")
wifi_iso_10_6 <- readRDS("data/isochrones/wifi/wifi_iso_10_6.RDS")
wifi_iso_10_7 <- readRDS("data/isochrones/wifi/wifi_iso_10_7.RDS")
wifi_iso_10_8 <- readRDS("data/isochrones/wifi/wifi_iso_10_8.RDS")
wifi_iso_10_9 <- readRDS("data/isochrones/wifi/wifi_iso_10_9.RDS")
wifi_iso_10_10 <- readRDS("data/isochrones/wifi/wifi_iso_10_10.RDS")

wifi_iso_15_1 <- readRDS("data/isochrones/wifi/wifi_iso_15_1.RDS")
wifi_iso_15_2 <- readRDS("data/isochrones/wifi/wifi_iso_15_2.RDS")
wifi_iso_15_3 <- readRDS("data/isochrones/wifi/wifi_iso_15_3.RDS")
wifi_iso_15_4 <- readRDS("data/isochrones/wifi/wifi_iso_15_4.RDS")
wifi_iso_15_5 <- readRDS("data/isochrones/wifi/wifi_iso_15_5.RDS")
wifi_iso_15_6 <- readRDS("data/isochrones/wifi/wifi_iso_15_6.RDS")
wifi_iso_15_7 <- readRDS("data/isochrones/wifi/wifi_iso_15_7.RDS")
wifi_iso_15_8 <- readRDS("data/isochrones/wifi/wifi_iso_15_8.RDS")
wifi_iso_15_9 <- readRDS("data/isochrones/wifi/wifi_iso_15_9.RDS")
wifi_iso_15_10 <- readRDS("data/isochrones/wifi/wifi_iso_15_10.RDS")

ems_iso_8_1 <- readRDS("data/isochrones/ems/ems_iso_8_1.RDS")
ems_iso_8_2 <- readRDS("data/isochrones/ems/ems_iso_8_2.RDS")
ems_iso_8_3 <- readRDS("data/isochrones/ems/ems_iso_8_3.RDS")
ems_iso_8_4 <- readRDS("data/isochrones/ems/ems_iso_8_4.RDS")
ems_iso_8_5 <- readRDS("data/isochrones/ems/ems_iso_8_5.RDS")
ems_iso_8_6 <- readRDS("data/isochrones/ems/ems_iso_8_6.RDS")
ems_iso_8_7 <- readRDS("data/isochrones/ems/ems_iso_8_7.RDS")
ems_iso_8_8 <- readRDS("data/isochrones/ems/ems_iso_8_8.RDS")
ems_iso_8_9 <- readRDS("data/isochrones/ems/ems_iso_8_9.RDS")

ems_iso_10_1 <- readRDS("data/isochrones/ems/ems_iso_10_1.RDS")
ems_iso_10_2 <- readRDS("data/isochrones/ems/ems_iso_10_2.RDS")
ems_iso_10_3 <- readRDS("data/isochrones/ems/ems_iso_10_3.RDS")
ems_iso_10_4 <- readRDS("data/isochrones/ems/ems_iso_10_4.RDS")
ems_iso_10_5 <- readRDS("data/isochrones/ems/ems_iso_10_5.RDS")
ems_iso_10_6 <- readRDS("data/isochrones/ems/ems_iso_10_6.RDS")
ems_iso_10_7 <- readRDS("data/isochrones/ems/ems_iso_10_7.RDS")
ems_iso_10_8 <- readRDS("data/isochrones/ems/ems_iso_10_8.RDS")
ems_iso_10_9 <- readRDS("data/isochrones/ems/ems_iso_10_9.RDS")

ems_iso_12_1 <- readRDS("data/isochrones/ems/ems_iso_12_1.RDS")
ems_iso_12_2 <- readRDS("data/isochrones/ems/ems_iso_12_2.RDS")
ems_iso_12_3 <- readRDS("data/isochrones/ems/ems_iso_12_3.RDS")
ems_iso_12_4 <- readRDS("data/isochrones/ems/ems_iso_12_4.RDS")
ems_iso_12_5 <- readRDS("data/isochrones/ems/ems_iso_12_5.RDS")
ems_iso_12_6 <- readRDS("data/isochrones/ems/ems_iso_12_6.RDS")
ems_iso_12_7 <- readRDS("data/isochrones/ems/ems_iso_12_7.RDS")
ems_iso_12_8 <- readRDS("data/isochrones/ems/ems_iso_12_8.RDS")
ems_iso_12_9 <- readRDS("data/isochrones/ems/ems_iso_12_9.RDS")

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

# user -------------------------------------------------------------
ui <- navbarPage(title = "I'm a title!",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),
                 # main -----------------------------------------------------------
                 # tabPanel("Home", value = "home",
                 #          fluidRow(style = "margin: 6px;",
                 #                   align = "center",
                 #                   br("", style = "padding-top:10px;"),
                 #                   img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                 #                   br(""),
                 #                   h2(strong("Addressing Barriers to Health in Patrick County, Virginia"),
                 #                   br(""),
                 #                   h4("Data Science for the Public Good Program"),
                 #                   h4("University of Virginia"),
                 #                   h4("Biocomplexity Insititute"),
                 #                   br(),
                 #                   br(),
                 #                   br(),
                 #                   br(),
                 #                   br(),
                 #                   p(tags$small(em('Last updated: August 2020')))
                 #                   )
                 #          )
                 # ),

                 # main -----------------------------------------------------------
                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   # br("", style = "padding-top:2px;"),
                                   # img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                   br(""),
                                   h1(strong("Addressing Barriers to Health in Patrick County, Virginia"),
                                      br(""),
                                      h4("Data Science for the Public Good Program"),
                                      h4("University of Virginia"),
                                      h4("Biocomplexity Insititute"),
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
                                   p(tags$small(em('Last updated: August 2020'))))
                 ),

                 # socio -----------------------------------------------------------
                 tabPanel("Sociodemographics", value = "socio",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Patrick County Residents' Sociodemographic Characteristics"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(4,
                                          h4(strong("Who does Patrick County Serve?")),
                                          p("We examined Patrick County population sociodemographic and socioeconomic characteristics to better understand the
                                            residents that the county serves."),
                                          p("We retrieved American Community Survey (ACS) data to calculate this information at census block group and census
                                            tract levels. ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households to compile 1-year and 5-year datasets. We used
                                            the most recently available 5-year estimates from 2014/18 to compute percent Patrick County residents in a given block group or tract by age, race, ethnicity,
                                            employment, health insurance coverage, and other relevant characteristics."),
                                          p("Our interactive plots visualize census block-group level sociodemographic characteristics of Patrick County residents.")),
                                   column(8,
                                          h4(strong("Map of Resident Socioeconomic Characteristics by Census Tract or Block Group")),
                                          selectInput("sociodrop", "Select Variable:", width = "100%", choices = c(
                                            "Percent Population Age 65 and Older" = "age65",
                                            "Percent Population Age 18 and Younger" = "under18",
                                            "Percent Population Black" = "black",
                                            "Percent Population Hispanic" = "hispanic",
                                            "Percent Population Without Bachelor's Degree" = "noba",
                                            "Percent Population In Labor Force Unemployed" = "unempl",
                                            "Percent Population Without Health Insurance" = "nohealthins2",
                                            "Percent Population With Private Health Insurance" = "privateins",
                                            "Percent Population With Public Health Insurance" = "publicins",
                                            "Percent Population in Poverty" = "inpov",
                                            "Percent Population Receiving SNAP Benefits or Public Assistance" = "snap",
                                            "Total Population by Census Block Group" = "totalpop_bgrp",
                                            "Total Population by Census Tract" = "totalpop_trct")
                                          ),
                                          withSpinner(leafletOutput("socioplot")),
                                          p(tags$small("Data Source: American Community Survey 2014/18 5-Year Estimates."))
                                   ))
                                          ),

                 # older -----------------------------------------------------------
                 tabPanel("Older Adults", value = "older",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Older Adults in Patrick County"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(4,
                                          h4(strong("Who are the Patrick County Older Adults?")),
                                          p("The US population is aging, and in Patrick County, over 30% of residents are older adults aged 65 years and over. This represents more than 5,000
                                           individuals with varying health conditions that may benefit from locally accessible health care and social services resources. However, access to
                                           health care resources is limited in rural areas, particularly for older adults in need of assistance with activities of daily life."),
                                          p("To help Patrick County better understand their older adult population, we used American Community Survey (ACS) data and obtained census tract
                                           level information for the age group. ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households to compile
                                           1-year and 5-year estimates of population sociodemographic and socioeconomic characteristics. We used the most recently available 5-year data
                                           from 2014/18 to calculate the percentage of the Patrick County older adults with different types of disability, as well as provided information
                                           on their living arrangements and socioeconomic status. We provided all information at census tract level and by gender."),
                                          p("These insights on the health and socioeconomic status of older adults in Patrick County can assist the county in identifying areas of high need
                                          for health care resources.")
                                   ),
                                   column(8,
                                          h4(strong("Map of Older Adult Characteristics by Census Tract")),
                                          tabsetPanel(
                                            tabPanel("Older Adult Characteristics",
                                                     p(""),
                                                     column(6,
                                                            selectInput("olddrop", "1. Select Variable:", width = "100%", choices = c(
                                                              "Percent with Vision Difficulty" = "visdiff",
                                                              "Percent with Ambulatory Difficulty" = "ambdiff",
                                                              "Percent with Self-Care Difficulty" = "carediff",
                                                              "Percent with Cognitive Difficulty" = "cogdiff",
                                                              "Percent with Independent Living Difficulty" = "ildiff",
                                                              "Percent with Any Disability" = "disab",
                                                              "Percent in Poverty" = "inpov",
                                                              "Percent in Labor Force" = "labfor")
                                                            )),
                                                     column(6,
                                                            selectInput("oldspecdrop", "2. Select Group:", width = "100%", choices = c(
                                                              "Total",
                                                              "Female" = "_f",
                                                              "Male" = "_m")
                                                            )),
                                                     withSpinner(leafletOutput("oldplot")),
                                                     p(tags$small("Data Source: American Community Survey 2014/18 5-Year Estimates."))
                                            ),
                                            tabPanel("Older Adult Household Characteristics",
                                                     p(""),
                                                     selectInput("hhdrop", "Select Variable:", width = "100%", choices = c(
                                                       "Percent Married Couple Households with one or more 60+ Member" = "hhsixty_married",
                                                       "Percent Households with one or more 60+ Members" = "hhsixty_total",
                                                       "Percent Single (no partner present) Households with one or more 60+ Member" = "hhsixty_nonfam",
                                                       "Percent Households with one or more Male 60+ Members" = "hhsixty_mhh",
                                                       "Households with one or more Female 60+ Members" = "hhsixty_fhh")),
                                                     withSpinner(leafletOutput("householdplot")),
                                                     p(tags$small("Data Source: American Community Survey 2014/18 5-Year Estimates."))
                                            )
                                          )
                                   )
                          )
                 ),

                 # wifi-----------------------------------------------------------
                 tabPanel("Connectivity", value = "connectivity",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Digital Connectivity in Patrick County"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(6,
                                          h4(strong("Computing Device Ownership and Internet Access Type")),
                                          p("Internet connection and computing devices are key for access to health information and participation in online health-related services like
                                             telemedicine. Rural areas frequently lack broadband access, experience low internet speeds, and have fewer internet providers available
                                             than urban areas. It is crucial to consider digital connectivity in improving health care access. We examined digital connectivity in Patrick County in two ways to
                                             provide the county with insights on where increasing connectivity would facilitate communicating health information and improve online health service access."),
                                          p("We first examined access to computing devices and internet connection types in Patrick County. We used American Community Survey (ACS) data to
                                            obtain this information at census block group level. ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households
                                            to compile 1-year and 5-year estimates of population sociodemographic and socioeconomic characteristics. We used the most
                                            recently available 5-year data from 2014/18 to calculate the percentage of the Patrick County residents with access to devices
                                            and internet by census block group."),
                                          br(),
                                          selectInput("devicedrop", "Select Variable:", width = "100%", choices = c(
                                            "Percent Households with No Computer" = "nocomputer",
                                            "Percent Households with Laptop" = "laptop",
                                            "Percent Households with Smartphone" = "smartphone",
                                            "Percent Households with Tablet" = "tablet",
                                            "Percent Households without Internet" = "nointernet",
                                            "Percent Households with Satellite Internet" = "satellite",
                                            "Percent Households with Cellular Internet" = "cellular",
                                            "Percent Households with Broadband Internet" = "broadband")
                                          ),
                                          p(strong("Map of Access by Census Block Group")),
                                          withSpinner(leafletOutput("deviceplot")),
                                          p(tags$small("Data Source: American Community Survey 2014/18 5-Year Estimates."))),
                                   column(6,
                                          h4(strong("Free WiFi Hotspot Access")),
                                          p("To understand internet access at a more granular level, we examined access to free wi-fi hotspots in the county."),
                                          p("We obtained wifi hotspot locations using the Virginia Tech and CommonwealthConnect hotspot map. CommonwealthConnect identifies where people can connect to
                                            the internet for free, decreasing constraints placed on families that do not have internet access at home. We retrieved free internet locations in Patrick
                                            County from the data. We extracted locations of Patrick County residential properties from 2019 CoreLogic, a proprietary dataset for US real estate that
                                            includes information on building characteristics. Finally, we used the TravelTime Application Programming Interface (API) to calculate 10- and 15-minute
                                            car travel time isochrones—areas of equal travel time given a departure time and mode of transportation—from wifi hotspots. TravelTime API aggregates data
                                            from Open Street Maps, transport timetables and speed profiles to generate isochrones. Isochrones allowed us to identify wifi gaps, or clusters of
                                            residential properties that cannot reach a free internet location within a selected travel time range."),
                                          p("This information equips extension agents with knowledge on how best to reach their constituents, as well as identifies internet gaps that suggest where
                                            new wi-fi hotspots could be optimally placed to provide internet access to more residents."),
                                          br(),
                                          tabsetPanel(
                                            tabPanel("Explore Hotspot Coverage",
                                                     p(""),
                                                     selectInput("wifidrop", "Select Free Wifi Location:", width = "100%", choices = c(
                                                       "Meadows of Dan Elementary School",
                                                       "Woolwine Elementary School",
                                                       "Patrick Springs Primary School",
                                                       "Blue Ridge Elementary School",
                                                       "Patrick County High School",
                                                       "Stuart Elementary School",
                                                       "Patrick County Branch Library",
                                                       "Hardin Reynolds Memorial School",
                                                       "Stuart Baptist Church",
                                                       "Patrick Henry Community College Stuart Campus")),
                                                     p(strong("Percent Residential Properties Covered")),
                                                     withSpinner(tableOutput("wifitable")),
                                                     p(strong("Map of Coverage")),
                                                     withSpinner(leafletOutput("wifiplot")),
                                                     p(tags$small("Data Sources: CommonwealthConnect, 2020; CoreLogic, 2019; TravelTime API."))
                                            ),
                                            tabPanel("Explore 'Deserts'",
                                                     p(""),
                                                     p(strong("Percent Residential Properties Covered")),
                                                     withSpinner(tableOutput("allwifitable")),
                                                     p(strong("Map of Free Wi-Fi Deserts")),
                                                     withSpinner(leafletOutput("allwifi")),
                                                     p(tags$small("Data Sources: CommonwealthConnect, 2020; CoreLogic, 2019; TravelTime API."))
                                            )
                                          )
                                   )
                          )
                 ),

                 # ems -----------------------------------------------------------
                 tabPanel("Health Care Access", value = "ems",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Health Care Access in Patrick County"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(4,
                                          h4(strong("Accessing Emergency Medical Service Stations")),
                                          p("Access to health care services in rural areas is limited by a lack of transportation and a shortage of healthcare professionals. Compared to urban
                                            counterparts, rural residents must travel farther to obtain both preventive and specialty care. Patrick County’s general practitioner, dentist, and mental health
                                            provider-to-patient ratios fall below state averages, and the county recently experienced the closure of its only hospital. Its residents often rely on emergency
                                            medical services (EMS) stations to obtain care and transportation to other health care facilities."),
                                          p("To better understand health service access limitations in the county, we examined residents’ access to EMS stations. We obtained EMS locations using Homeland
                                            Infrastructure Foundation-Level Data (HIFLD) collected by the Department of Homeland Security. HIFLD is a public source dataset with information on a range of
                                            facilities; we used the data to retrieve EMS station latitude and longitude. We extracted locations of Patrick County residential
                                            properties from 2019 CoreLogic, a proprietary dataset for US real estate that includes information on building characteristics. Finally, we used the TravelTime
                                            Application Programming Interface (API) to calculate 8-, 10-, and 12- minute drive time isochrones—areas of equal travel time given a departure time and
                                            mode of transportation—from EMS stations. TravelTime API aggregates data from Open Street Maps, transport timetables and speed profiles to generate isochrones.
                                            Isochrones allowed us to identify EMS coverage gaps, or clusters of residential properties that cannot be reached from an EMS location within a selected travel
                                            time range. We selected 8-, 10-, and 12-minute thresholds as EMS are expected to reach distressed individuals within 8 minutes. However, this threshold is
                                            frequently exceeded by 20% to 40% in rural areas.")
                                   ),
                                   column(8,
                                          tabsetPanel(
                                            tabPanel("Explore Coverage",
                                                     p(""),
                                                     selectInput("emsdrop", "Select EMS Location:", width = "100%", choices = c(
                                                       "Stuart Volunteer Fire Department" = "STUART VOLUNTEER FIRE DEPARTMENT",
                                                       "Moorefield Store Volunteer Fire Department" = "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT",
                                                       "Blue Ridge Volunteer Rescue Squad" = "BLUE RIDGE VOLUNTEER RESCUE SQUAD",
                                                       "Vesta Rescue Squad" = "VESTA RESCUE SQUAD",
                                                       "Ararat Rescue Squad" = "ARARAT RESCUE SQUAD",
                                                       "Five Forks Volunteer Fire and Rescue Station 1" = "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS",
                                                       "Five Forks Volunteer Fire and Rescue Station 2"= "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2",
                                                       "Jeb Stuart Rescue Squad" = "JEB STUART RESCUE SQUAD",
                                                       "Smith River Rescue Squad" = "SMITH RIVER RESCUE SQUAD"
                                                     )),
                                                     p(strong("Percent Residents Covered")),
                                                     withSpinner(tableOutput("emstable")),
                                                     p(strong("Map of Coverage")),
                                                     withSpinner(leafletOutput("emsplot")),
                                                     p(tags$small("Data Sources: Homeland Infrastructure Foundation-Level Data, 2010; CoreLogic, 2019; TravelTime API."))
                                            ),
                                            tabPanel("Explore 'Deserts'",
                                                     p(""),
                                                     p(strong("Percent Residents Covered")),
                                                     withSpinner(tableOutput("allemstable")),
                                                     p(strong("Map of Coverage Deserts")),
                                                     withSpinner(leafletOutput("allems")),
                                                     p(tags$small("Data Sources: Homeland Infrastructure Foundation-Level Data, 2010; CoreLogic, 2019; TravelTime API.")))
                                          )
                                   )
                          )
                 ),

                 # food -----------------------------------------------------------
                 tabPanel("Food Access", value =  "food",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Food Access in Patrick County"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(5,
                                          h4(strong("Food Access in Rural Areas")),
                                          p("Social determinants of health shape food access, a key factor in negative health outcomes. Rural area residents frequently face difficulties in accessing
                                             healthy and nutritious food, and experience high rates of chronic illnesses like heart disease and diabetes, resulting in higher mortality rates and lower
                                             life expectancy compared to urban areas. Facilitating  access to nutritious and high-quality foods can lead to decreases in chronic disease prevalence.
                                             Many Patrick County residents suffer from conditions like diabetes and obesity, and providing healthy food may support disease prevention."),
                                          p("We analyzed multiple data sources to give Patrick County actionable information on their residents’ food access that can inform county efforts ensuring equitable food access for all."),
                                          p("First, we examined", strong("food access at multiple distance thresholds by age and socioeconomic status."), "We used the 2017 United States Department of
                                             Agriculture (USDA) Food Access Research Atlas, a central database created by the Economic Research Service that provides access indicators for different social groups.
                                             We created census tract-level maps that identify Patrick County areas where residents may have difficulty accessing nutritious foods, and highlight geographies
                                             where this is the case for particularly vulnerable groups like low-income individuals and older adults."),
                                          p("Second, to better understand how residents must travel to obtain food, we constructed isochrones—shapes covering places within reach in the
                                             same time frame given a start location and a mode of transportation—from Patrick County", strong("residential properties to locations of grocery stores
                                             and farmers’ markets."), "We used Google Maps to identify these locations' latitude and longitude. We extracted locations of
                                             Patrick County residential properties from 2019 CoreLogic, a proprietary dataset for US real estate with information on building characteristics.
                                             Finally, we used the TravelTime Application Programming Interface (API) to calculate 10- and 15-minute car travel times from grocery locations. TravelTime
                                             API aggregates data from Open Street Maps, transport timetables and speed profiles to generate isochrones. This allowed us to identify food deserts, or clusters
                                             of properties that cannot reach a location with healthy food within a selected travel time range. These areas in the county could benefit from programs
                                             facilitating access to produce."),
                                          p("Finally, Patrick County offers", strong("access to free food"), "at multiple locations. For community members that struggle with food security, these locations can
                                             offer temporary assistance. We used GoogleMaps to locate food banks, food pantries, and community meal sites, geocoded their addresses, and mapped
                                             these resources along with notes on their target audiences.")
                                   ),
                                   column(7,
                                          tabsetPanel(
                                            tabPanel("Food Access",
                                                     p(""),
                                                     selectInput("usdadrop", "Select Variable:", width = "100%", choices = c(
                                                       "Percent Population with Low Food Access at 1 Mile" = "lapop1share",
                                                       "Percent Population with Low Food Access at 10 Miles" = "lapop10share",
                                                       "Percent Children with Low Food Access at 1 Mile" = "lakids1share",
                                                       "Percent Children with Low Food Access at 10 Miles" = "lakids10share",
                                                       "Percent Low Income Population with Low Food Access at 1 Mile" = "lalowi1share",
                                                       "Percent Low Income Population with Low Food Access at 10 Miles" = "lalowi10share",
                                                       "Percent Older Adults with Low Food Access at 1 Mile" = "laseniors1share",
                                                       "Percent Older Adults with Low Food Access at 10 Miles" = "laseniors10share")
                                                     ),
                                                     p(strong("Map of Access at Census Tract Level")),
                                                     withSpinner(leafletOutput("usdaplot")),
                                                     p(tags$small("Data Source: USDA Food Access Research Atlas, 2017"))
                                            ),
                                            tabPanel("Grocery and Farmers' Market Coverage",
                                                     p(""),
                                                     selectInput("grocdrop", "Select Location:", width = "100%", choices = c(
                                                       "Mountain Meadow Farm and Craft Market",
                                                       "Lowes Foods of Stuart",
                                                       "Patrick County Local Farmers Market",
                                                       "Stuart Farmers Market",
                                                       "W & W Produce",
                                                       "Walmart Supercenter",
                                                       "Poor Farmers Farm")),
                                                     p(strong("Percent Households Covered")),
                                                     withSpinner(tableOutput("groctable")),
                                                     p(strong("Map of Coverage")),
                                                     withSpinner(leafletOutput("grocplot")),
                                                     p(tags$small("Data Source: Google Maps; TravelTime API; CoreLogic, 2019."))
                                            ),
                                            tabPanel("Food Deserts",
                                                     p(""),
                                                     p(strong("Percent Households Covered")),
                                                     withSpinner(tableOutput("allgrctable")),
                                                     p(strong("Map of Food Deserts")),
                                                     withSpinner(leafletOutput("allgroc")),
                                                     p(tags$small("Data Source: Google Maps; TravelTime API; CoreLogic, 2019."))
                                            ),
                                            tabPanel("Food Security Resources",
                                                     p(""),
                                                     p(strong("Map of Food Security Resources")),
                                                     withSpinner(leafletOutput("othermap")),
                                                     p(tags$small("Data Source: Google Maps."))
                                            )
                                          )
                                   )
                          )
                 ),
                 # data -----------------------------------------------------------
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
                                     selectInput("topic", "Select Topic:", width = "100%", choices = c(
                                       "All Measures",
                                       "Sociodemographic Measures",
                                       "Older Adult Population Measures",
                                       "Connectivity Measures",
                                       "Food Access Measures",
                                       "Health Care Access Measures")
                                     ),
                                     withSpinner(DTOutput("datatable"))
                            )
                          )
                 ),

                 # contact -----------------------------------------------------------
                 tabPanel("Contact", value = "contact",
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                            h1(strong("Contact"), align = "center"),
                            br(),
                            h4(strong("UVA Data Science for the Public Good")),
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

  # socio plots: done -----------------------------------------------------

  var <- reactive({
    input$sociodrop
  })
  #age65
  output$socioplot <- renderLeaflet({
    if(var() == "age65") {

      pal <- colorQuantile("Blues", domain = socdem_block$age65, probs = seq(0, 1, length = 6), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population age 65 or over:</strong>",
              round(socdem_block$age65, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$age65),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$age65),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
      #under18
    }else if(var() == "under18"){
      pal <- colorQuantile("Blues", domain = socdem_block$under18, probs = seq(0, 1, length = 6), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population age 18 or under: </strong>",
              round(socdem_block$under18, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$under18),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$under18),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
      #population-tract
    }else if(var() == "totalpop_trct"){
      pal <- colorQuantile("Blues", domain = socdem_tract$totalpop_trct, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>Total population: </strong>",
              formatC(socdem_tract$totalpop_trct, format = "f", big.mark =",", digits = 0)),
        htmltools::HTML
      )

      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$totalpop_trct),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$totalpop_trct),
                  title = "Total Population<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 0), " &ndash; ", round(cuts[-1], 0), ")")
                  })
      #population-block group
    }else if(var() == "totalpop_bgrp"){
      pal <- colorQuantile("Blues", domain = socdem_block$totalpop_bgrp, probs = seq(0, 1, length = 6), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>Total population: </strong>",
              formatC(socdem_block$totalpop_bgrp, format = "f", big.mark =",", digits = 0)),
        htmltools::HTML
      )

      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$totalpop_bgrp),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$totalpop_bgrp),
                  title = "Total Population<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 0), " &ndash; ", round(cuts[-1], 0), ")")
                  })
    }else if(var() == "black"){
      pal <- colorQuantile("Blues", domain = socdem_block$black, probs = seq(0, 1, length = 6), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population Black: </strong>",
              round(socdem_block$black, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$black),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$black),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "noba"){
      pal <- colorQuantile("Blues", domain = socdem_block$noba, probs = seq(0, 1, length = 6), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population without BA degree: </strong>",
              round(socdem_block$noba, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$noba),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$noba),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "unempl"){
      pal <- colorQuantile("Blues", domain = socdem_block$unempl, probs = seq(0, 1, length = 6), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population in labor force unemployed: </strong>",
              round(socdem_block$unempl, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$unempl),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$unempl),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "nohealthins2"){
      pal <- colorQuantile("Blues", domain = socdem_block$nohealthins2, probs = seq(0, 1, length = 6), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population without health insurance: </strong>",
              round(socdem_block$nohealthins2, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$nohealthins2),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$nohealthins2),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "snap"){
      pal <- colorQuantile("Blues", domain = socdem_block$snap, probs = seq(0, 1, length = 6), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population receiving public assistance or SNAP benefits: </strong>",
              round(socdem_block$snap, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$snap),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$snap),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "inpov"){
      pal <- colorQuantile("Blues", domain = socdem_tract$inpov, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% Population in poverty: </strong>",
              round(socdem_tract$inpov, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$inpov),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$inpov),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "hispanic"){
      pal <- colorQuantile("Blues", domain = socdem_tract$hispanic, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% Population Hispanic or Latino: </strong>",
              round(socdem_tract$hispanic, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$hispanic),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$hispanic),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "privateins"){
      pal <- colorQuantile("Blues", domain = socdem_tract$privateins, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% Population with private health insurance: </strong>",
              round(socdem_tract$privateins, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$privateins),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$privateins),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else{
      pal <- colorQuantile("Blues", domain = socdem_tract$publicins, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% Population with public health insurance: </strong>",
              round(socdem_tract$publicins, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$publicins),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$publicins),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })


  # old plots - snap -----------------------------------------------
  var_old <- reactive({
    input$olddrop
  })
  var_hh <- reactive({
    input$hhdrop
  })
  output$oldplot <- renderLeaflet({
    # healthins wasn't coded properly so it's just all zeroes
    if(var_old() == "visdiff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$visdiff,
                     "_f" = olderadults$visdiff_f,
                     "_m" = olderadults$visdiff_m)

      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with vision difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "ambdiff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$ambdiff,
                     "_f" = olderadults$ambdiff_f,
                     "_m" = olderadults$ambdiff_m)

      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with ambulatory difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "cogdiff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$cogdiff,
                     "_f" = olderadults$cogdiff_f,
                     "_m" = olderadults$cogdiff_m)

      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with cognitive difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "carediff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$carediff,
                     "_f" = olderadults$carediff_f,
                     "_m" = olderadults$carediff_m)

      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with self-care difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "ildiff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$ildiff,
                     "_f" = olderadults$ildiff_f,
                     "_m" = olderadults$ildiff_m)

      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with independent living difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "disab") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$disab,
                     "_f" = olderadults$disab_f,
                     "_m" = olderadults$disab_m)

      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with any disability: </strong>",
              round(data, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "inpov") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$inpov,
                     "_f" = olderadults$inpov_f,
                     "_m" = olderadults$inpov_m)

      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults in poverty: </strong>",
              round(data, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else
      # if(var_old() == "labfor")
    {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$labfor,
                     "_f" = olderadults$labfor_f,
                     "_m" = olderadults$labfor_m)

      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults in the labor force: </strong>",
              round(data, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  output$householdplot <- renderLeaflet({
    if(var_hh() == "hhsixty_total") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_total,
                     "_f" = olderadults$hhsixty_total,
                     "_m" = olderadults$hhsixty_total)

      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_total, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Housholds with a 60+ member: </strong>",
              round(olderadults$hhsixty_total, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_total),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_total),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_hh() == "hhsixty_fhh") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_fhh,
                     "_f" = olderadults$hhsixty_fhh,
                     "_m" = olderadults$hhsixty_fhh)

      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_fhh, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Housholds with a female 60+ member:</strong>",
              round(olderadults$hhsixty_fhh, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_fhh),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_fhh),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_hh() == "hhsixty_mhh") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_mhh,
                     "_f" = olderadults$hhsixty_mhh,
                     "_m" = olderadults$hhsixty_mhh)

      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_mhh, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Housholds with a male 60+ member: </strong>",
              round(olderadults$hhsixty_mhh, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_mhh),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_mhh),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_hh() == "hhsixty_nonfam") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_nonfam,
                     "_f" = olderadults$hhsixty_nonfam,
                     "_m" = olderadults$hhsixty_nonfam)

      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_nonfam, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Single housholds with a 60+ member: </strong>",
              round(olderadults$hhsixty_nonfam, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_nonfam),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_nonfam),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else{
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_marr,
                     "_f" = olderadults$hhsixty_marr,
                     "_m" = olderadults$hhsixty_marr)

      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_marr, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Married households with a 60+ member: </strong>",
              round(olderadults$hhsixty_marr, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_marr),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_marr),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })


  # data and measures table: done ----------------------------------------
  var_topic <- reactive({
    input$topic
  })
  output$datatable <- renderDataTable({
    if(var_topic() == "All Measures"){
      table <- as.data.frame(measures_table)
     datatable(table, rownames = FALSE, options = list(pageLength = 15)) %>% formatStyle(0, target = 'row', lineHeight = '80%')
    }
    else{
      data <- switch(input$topic,
                     "Connectivity Measures" = "connectivity",
                     "Sociodemographic Measures" = "demographics",
                     "Food Access Measures" = "food access",
                     "Health Care Access Measures" = "health",
                     "Older Adult Population Measures" = "older adults")
      table <- subset(measures_table, Topic == data)
      table <- as.data.frame(table)
      datatable(table, rownames = FALSE, options = list(pageLength = 15)) %>% formatStyle(0, target = 'row', lineHeight = '80%')
    }
  })

  # device: done ---------------------------------------------------------

  output$deviceplot <- renderLeaflet({
      data <- switch(input$devicedrop,
                     "nocomputer" = connectivity$nocomputer,
                     "laptop" = connectivity$laptop,
                     "smartphone" = connectivity$smartphone,
                     "tablet" = connectivity$tablet,
                     "nointernet" = connectivity$nointernet,
                     "satellite" = connectivity$satellite,
                     "cellular" = connectivity$cellular,
                     "broadband" = connectivity$broadband)

      device_spec <- switch(input$devicedrop,
                            "nocomputer" = "no computer",
                            "laptop" = "laptop",
                            "smartphone" = "smartphone",
                            "tablet" = "tablet",
                            "nointernet" = "no internet access",
                            "satellite" = "satellite internet",
                            "cellular" = "cellular internet",
                            "broadband" = "broadband internet")

      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 6), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              connectivity$NAME.y,
              "<br />",
              "<strong>% Households with",
              device_spec,
              "access: </strong>",
              round(data, 2)),
        htmltools::HTML
      )

      leaflet(data = connectivity, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
  })


  # wifi: done -----------------------------------------------------------

  # Iso selector
  output$wifiplot <- renderLeaflet({
      colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

      wifi_iso10 <- switch(input$wifidrop,
                     "Meadows of Dan Elementary School" = wifi_iso_10_1,
                     "Woolwine Elementary School" = wifi_iso_10_2,
                     "Patrick Springs Primary School" = wifi_iso_10_3,
                     "Blue Ridge Elementary School" = wifi_iso_10_4,
                     "Patrick County High School" = wifi_iso_10_5,
                     "Stuart Elementary School" = wifi_iso_10_6,
                     "Patrick County Branch Library" = wifi_iso_10_7,
                     "Hardin Reynolds Memorial School" = wifi_iso_10_8,
                     "Stuart Baptist Church" = wifi_iso_10_9,
                     "Patrick Henry Community College Stuart Campus" = wifi_iso_10_10)

      wifi_iso15 <- switch(input$wifidrop,
                           "Meadows of Dan Elementary School" = wifi_iso_15_1,
                           "Woolwine Elementary School" = wifi_iso_15_2,
                           "Patrick Springs Primary School" = wifi_iso_15_3,
                           "Blue Ridge Elementary School" = wifi_iso_15_4,
                           "Patrick County High School" = wifi_iso_15_5,
                           "Stuart Elementary School" = wifi_iso_15_6,
                           "Patrick County Branch Library" = wifi_iso_15_7,
                           "Hardin Reynolds Memorial School" = wifi_iso_15_8,
                           "Stuart Baptist Church" = wifi_iso_15_9,
                           "Patrick Henry Community College Stuart Campus" = wifi_iso_15_10)

       data <- switch(input$wifidrop,
                           "Meadows of Dan Elementary School" = 1,
                           "Woolwine Elementary School" = 2,
                           "Patrick Springs Primary School" = 3,
                           "Blue Ridge Elementary School" = 4,
                           "Patrick County High School" = 5,
                           "Stuart Elementary School" = 6,
                           "Patrick County Branch Library" = 7,
                           "Hardin Reynolds Memorial School" = 8,
                           "Stuart Baptist Church" = 9,
                           "Patrick Henry Community College Stuart Campus" = 10)

       labels <- lapply(
         paste("<strong>Name: </strong>",
               wifi_latlong[data, ]$name,
               "<br />",
               "<strong>Address:</strong>",
               wifi_latlong[data, ]$fulladdress,
               "<br />",
               "<strong>Notes:</strong>",
               wifi_latlong[data, ]$notes),
         htmltools::HTML
       )

      m1 <- leaflet(options = leafletOptions(minZoom = 10)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircles(data = residential,
                   fillColor = colors[5],
                   fillOpacity = .8,
                   stroke = FALSE,
                   group = "Residential Properties") %>%
        addPolygons(data = wifi_iso10,
                    fillColor = colors[1],
                    fillOpacity = .8,
                    stroke = FALSE,
                    group = "10 Minute Isochrone") %>%
        addPolygons(data = wifi_iso15,
                    fillColor = colors[2],
                    fillOpacity = .8,
                    stroke = FALSE,
                    group = "15 Minute Isochrone") %>%
        addMarkers(data = wifi_latlong, ~longitude[data], ~latitude[data],
                   label = labels,
                   labelOptions = labelOptions(direction = "bottom",
                                               style = list(
                                                 "font-size" = "12px",
                                                 "border-color" = "rgba(0,0,0,0.5)",
                                                 direction = "auto")))  %>%
        addLayersControl(
          position = "topright",
          overlayGroups = c("10 Minute Isochrone",
                            "15 Minute Isochrone",
                            "Residential Properties"),
          options = layersControlOptions(collapsed = FALSE))
      m1
  })

  # Coverage table
  output$wifitable <- renderTable({
    data <- switch(input$wifidrop,
                         "Meadows of Dan Elementary School" = 1,
                         "Woolwine Elementary School" = 2,
                         "Patrick Springs Primary School" = 3,
                         "Blue Ridge Elementary School" = 4,
                         "Patrick County High School" = 5,
                         "Stuart Elementary School" = 6,
                         "Patrick County Branch Library" = 7,
                         "Hardin Reynolds Memorial School" = 8,
                         "Stuart Baptist Church" = 9,
                         "Patrick Henry Community College Stuart Campus" = 10)

    table <- read.csv(paste0("data/isochrones/tables/wifi_iso_table_",data,".csv"))
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)

  # Wifi deserts
  output$allwifi <- renderLeaflet({

    labels <- lapply(
      paste("<strong>Name: </strong>",
            wifi_latlong$name,
            "<br />",
            "<strong>Address:</strong>",
            wifi_latlong$fulladdress,
            "<br />",
            "<strong>Notes:</strong>",
            wifi_latlong$notes),
      htmltools::HTML
    )

    leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential,
                 fillColor = colors[5],
                 fillOpacity = .5,
                 stroke = FALSE,
                 group = "Residential Properties") %>%
      addPolygons(data = wifi_iso_10_1,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_2,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_3,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_4,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_5,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_6,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_7,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_8,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_9,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_1,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_2,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_3,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_4,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_5,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_6,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_7,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_8,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_9,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_10,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addMarkers(data = wifi_latlong,
                 group = "Free Wi-Fi Locations",
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto")))  %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("Free Wi-Fi Locations",
                          "Residential Properties"),
        baseGroups = c("10 Minute Isochrones",
                       "15 Minute Isochrones"),
        options = layersControlOptions(collapsed = FALSE))
  })

  output$allwifitable <- renderTable({
    table <- read.csv("data/isochrones/tables/wifi_iso_table.csv")
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)

  # ems: done ------------------------------------------------------------

  output$emsplot <- renderLeaflet({
      colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

      ems_iso8 <- switch(input$emsdrop,
                     "STUART VOLUNTEER FIRE DEPARTMENT" = ems_iso_8_1,
                     "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = ems_iso_8_2,
                     "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = ems_iso_8_3,
                     "VESTA RESCUE SQUAD" = ems_iso_8_4,
                     "ARARAT RESCUE SQUAD" = ems_iso_8_5,
                     "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = ems_iso_8_6,
                     "JEB STUART RESCUE SQUAD" = ems_iso_8_7,
                     "SMITH RIVER RESCUE SQUAD" = ems_iso_8_8,
                     "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = ems_iso_8_9)

      ems_iso10 <- switch(input$emsdrop,
                         "STUART VOLUNTEER FIRE DEPARTMENT" = ems_iso_10_1,
                         "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = ems_iso_10_2,
                         "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = ems_iso_10_3,
                         "VESTA RESCUE SQUAD" = ems_iso_10_4,
                         "ARARAT RESCUE SQUAD" = ems_iso_10_5,
                         "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = ems_iso_10_6,
                         "JEB STUART RESCUE SQUAD" = ems_iso_10_7,
                         "SMITH RIVER RESCUE SQUAD" = ems_iso_10_8,
                         "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = ems_iso_10_9)

      ems_iso12 <- switch(input$emsdrop,
                          "STUART VOLUNTEER FIRE DEPARTMENT" = ems_iso_12_1,
                          "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = ems_iso_12_2,
                          "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = ems_iso_12_3,
                          "VESTA RESCUE SQUAD" = ems_iso_12_4,
                          "ARARAT RESCUE SQUAD" = ems_iso_12_5,
                          "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = ems_iso_12_6,
                          "JEB STUART RESCUE SQUAD" = ems_iso_12_7,
                          "SMITH RIVER RESCUE SQUAD" = ems_iso_12_8,
                          "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = ems_iso_12_9)

      data <- switch(input$emsdrop,
                         "STUART VOLUNTEER FIRE DEPARTMENT" = 1,
                         "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = 2,
                         "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = 3,
                         "VESTA RESCUE SQUAD" = 4,
                         "ARARAT RESCUE SQUAD" = 5,
                         "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = 6,
                         "JEB STUART RESCUE SQUAD" = 7,
                         "SMITH RIVER RESCUE SQUAD" = 8,
                         "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = 9)

      labels <- lapply(
        paste("<strong>Name: </strong>",
              str_to_title(ems[data, ]$NAME),
              "<br />",
              "<strong>Address:</strong>",
              str_to_title(ems[data, ]$ADDRESS), ",", str_to_title(ems[data, ]$CITY), ", VA", ems[data, ]$ZIP,
              "<br />",
              "<strong>Type:</strong>",
              str_to_title(ems[data, ]$NAICSDESCR)),
        htmltools::HTML
      )

      m1 <- leaflet(options = leafletOptions(minZoom = 10)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircles(data = residential,
                   fillColor = colors[5],
                   fillOpacity = .8,
                   stroke = FALSE,
                   group = "Residential Properties") %>%
        addPolygons(data = ems_iso8,
                    fillColor = colors[1],
                    fillOpacity = .8,
                    stroke = FALSE,
                    group = "8 Minute Isochrone") %>%
        addPolygons(data = ems_iso10,
                    fillColor = colors[2],
                    fillOpacity = .8,
                    stroke = FALSE,
                    group = "10 Minute Isochrone") %>%
        addPolygons(data = ems_iso12,
                    fillColor = colors[2],
                    fillOpacity = .8,
                    stroke = FALSE,
                    group = "12 Minute Isochrone") %>%
        addMarkers(data = ems, ~LONGITUDE[data], ~LATITUDE[data],
                   group = "EMS Locations",
                   label = labels,
                   labelOptions = labelOptions(direction = "bottom",
                                               style = list(
                                                 "font-size" = "12px",
                                                 "border-color" = "rgba(0,0,0,0.5)",
                                                 direction = "auto"))) %>%
        addLayersControl(
          position = "topright",
          overlayGroups = c("8 Minute Isochrone",
                         "10 Minute Isochrone",
                         "12 Minute Isochrone",
                         "Residential Properties"),
          options = layersControlOptions(collapsed = FALSE))
      m1
  })

  output$emstable <- renderTable({
    data <- switch(input$emsdrop,
                       "STUART VOLUNTEER FIRE DEPARTMENT" = 1,
                       "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = 2,
                       "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = 3,
                       "VESTA RESCUE SQUAD" = 4,
                       "ARARAT RESCUE SQUAD" = 5,
                       "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = 6,
                       "JEB STUART RESCUE SQUAD" = 7,
                       "SMITH RIVER RESCUE SQUAD" = 8,
                       "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = 9)


    table <- read.csv(paste0("data/isochrones/tables/ems_iso_table_",data,".csv"))
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)

  # EMS deserts
  output$allems <- renderLeaflet({

    labels <- lapply(
      paste("<strong>Name: </strong>",
            str_to_title(ems$NAME),
            "<br />",
            "<strong>Address:</strong>",
            paste0(str_to_title(ems$ADDRESS), ", ", str_to_title(ems$CITY), ", VA ", ems$ZIP),
            "<br />",
            "<strong>Type:</strong>",
            str_to_title(ems$NAICSDESCR)),
      htmltools::HTML
    )

    leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential,
                 fillColor = colors[5],
                 fillOpacity = .5,
                 stroke = FALSE,
                 group = "Residential Properties") %>%
      addPolygons(data = ems_iso_8_1,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_2,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_3,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_4,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_5,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_6,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_7,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_8,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_9,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_1,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_2,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_3,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_4,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_5,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_6,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_7,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_8,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_9,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_1,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_2,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_3,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_4,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_5,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_6,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_7,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_8,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_9,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "12 Minute Isochrones") %>%
      addMarkers(data = ems,
                 group = "EMS Locations",
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto"))) %>%
      addLayersControl(
        position = "topright",
        baseGroups = c("8 Minute Isochrones",
                       "10 Minute Isochrones",
                       "12 Minute Isochrones"),
        overlayGroups = c("EMS Locations",
                       "Residential Properties"),
        options = layersControlOptions(collapsed = FALSE))
  })

  output$allemstable <- renderTable({
    table <- read.csv("data/isochrones/tables/ems_iso_table.csv")
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)


  # usda - lahunv10share  -----------------------------------------------------------
  var_usda <- reactive({
    input$usdadrop
  })
  output$usdaplot <- renderLeaflet({
      data <- switch(input$usdadrop,
                     "lakids1share" = usda$lakids1share,
                     "lakids10share" = usda$lakids10share,
                     "lalowi1share" = usda$lalowi1share,
                     "lalowi10share" = usda$lalowi10share,
                     "lapop1share" = usda$lapop1share,
                     "lapop10share" = usda$lapop10share,
                     "laseniors1share" = usda$laseniors1share,
                     "laseniors10share" = usda$laseniors10share)

      usda_spec <- switch(input$usdadrop,
                          "lakids1share" = "low food access for children at 1 mile",
                          "lakids10share" = "low food access for children at 10 miles",
                          "lalowi1share" = "low food access for low income population at 1 mile",
                          "lalowi10share" = "low food access for low income population at 10 miles",
                          "lapop1share" = "low food access at 1 mile",
                          "lapop10share" = "low food access at 10 miles",
                          "laseniors1share" = "low food access for seniors at 1 mile",
                          "laseniors10share" = "low food access for seniors at 10 miles")

      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              usda$NAME.y,
              "<br />",
              "<strong>% Population with",
              usda_spec,
              round(data, 2)),
        htmltools::HTML
      )

      leaflet(data = usda, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
  })

  # grocery --------------------------------------------------------

  # Iso selector
  output$grocplot <- renderLeaflet({
      colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

      groc_iso10 <- switch(input$grocdrop,
                     "Mountain Meadow Farm and Craft Market" = grc_iso_10_1,
                     "Lowes Foods of Stuart" = grc_iso_10_2,
                     "Patrick County Local Farmers Market" = grc_iso_10_3,
                     "Stuart Farmers Market" = grc_iso_10_4,
                     "W & W Produce" = grc_iso_10_5,
                     "Walmart Supercenter" = grc_iso_10_6,
                     "Poor Farmers Farm" = grc_iso_10_7)

      groc_iso15 <- switch(input$grocdrop,
                           "Mountain Meadow Farm and Craft Market" = grc_iso_15_1,
                           "Lowes Foods of Stuart" = grc_iso_15_2,
                           "Patrick County Local Farmers Market" = grc_iso_15_3,
                           "Stuart Farmers Market" = grc_iso_15_4,
                           "W & W Produce" = grc_iso_15_5,
                           "Walmart Supercenter" = grc_iso_15_6,
                           "Poor Farmers Farm" = grc_iso_15_7)

      data <- switch(input$grocdrop,
                           "Mountain Meadow Farm and Craft Market" = 1,
                           "Lowes Foods of Stuart" = 2,
                           "Patrick County Local Farmers Market" = 3,
                           "Stuart Farmers Market" = 4,
                           "W & W Produce" = 5,
                           "Walmart Supercenter" = 6,
                           "Poor Farmers Farm" = 7)

      labels <- lapply(
        paste("<strong>Name: </strong>",
              groceries_latlong[data, ]$name,
              "<br />",
              "<strong>Address:</strong>",
              groceries_latlong[data, ]$fulladdress,
              "<br />",
              "<strong>Type:</strong>",
              groceries_latlong[data, ]$type),
        htmltools::HTML
      )

      m1 <- leaflet(options = leafletOptions(minZoom = 10)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircles(data = residential,
                   fillColor = colors[5],
                   fillOpacity = .8,
                   stroke = FALSE,
                   group = "Residential Properties") %>%
        addPolygons(data = groc_iso10,
                    fillColor = colors[1],
                    fillOpacity = .8,
                    stroke = FALSE,
                    group = "10 Minute Isochrone") %>%
        addPolygons(data = groc_iso15,
                    fillColor = colors[2],
                    fillOpacity = .8,
                    stroke = FALSE,
                    group = "15 Minute Isochrone") %>%
        addMarkers(data = groceries_latlong, ~longitude[data], ~latitude[data],
                   group = "Fresh Food Location",
                   label = labels,
                   labelOptions = labelOptions(direction = "bottom",
                                               style = list(
                                                 "font-size" = "12px",
                                                 "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"))) %>%
        addLayersControl(
          position = "topright",
          overlayGroups = c("15 Minute Isochrone",
                            "10 Minute Isochrone",
                            "Residential Properties",
                            "Fresh Food Location"),
          options = layersControlOptions(collapsed = FALSE))
      m1
  })

  # Grocery table
  output$groctable <- renderTable({
    data <- switch(input$grocdrop,
                         "Mountain Meadow Farm and Craft Market" = 1,
                         "Lowes Foods of Stuart" = 2,
                         "Patrick County Local Farmers Market" = 3,
                         "Stuart Farmers Market" = 4,
                         "W & W Produce" = 5,
                         "Walmart Supercenter" = 6,
                         "Poor Farmers Farm" = 7)

    table <- read.csv(paste0("data/isochrones/tables/grc_iso_table_",data,".csv"))
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)

  # Food deserts
  output$allgroc <- renderLeaflet({

    labels <- lapply(
      paste("<strong>Name: </strong>",
            groceries_latlong$name,
            "<br />",
            "<strong>Address:</strong>",
            groceries_latlong$fulladdress,
            "<br />",
            "<strong>Type:</strong>",
            groceries_latlong$type),
      htmltools::HTML
    )

    leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential,
                 fillColor = colors[5],
                 fillOpacity = .5,
                 stroke = FALSE,
                 group = "Residential Properties") %>%
      addPolygons(data = grc_iso_10_1,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_2,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_3,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_4,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_5,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_6,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_7,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_1,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_2,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_3,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_4,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_5,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_6,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_7,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addMarkers(data = groceries_latlong,
                 group = "Fresh Food Locations",
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto")))  %>%
      addLayersControl(
        position = "topright",
        baseGroups = c("10 Minute Isochrones",
                       "15 Minute Isochrones"),
        overlayGroups = c("Residential Properties",
                          "Fresh Food Locations"),
        options = layersControlOptions(collapsed = FALSE))
  })

   # Other food resources
  output$othermap <- renderLeaflet({

    pal <- colorFactor(c("#0E879C", "#D9E12B", "#E6A01D"), domain = otherfood$type)

    labels <- lapply(
      paste("<strong>Name: </strong>",
            otherfood$name,
            "<br />",
            "<strong>Address:</strong>",
            otherfood$fulladdress,
            "<br />",
            "<strong>Type:</strong>",
            otherfood$type,
            "<br />",
            "<strong>Open to:</strong>",
            otherfood$audience,
            "<br />",
            "<strong>Notes:</strong>",
            otherfood$notes),
      htmltools::HTML
    )

    leaflet(data = otherfood,
            options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = patrickborder, stroke = T, weight = 2, color = "grey", fillOpacity = 0) %>%
      addCircleMarkers(data = otherfood,
                       stroke = FALSE,
                       fillOpacity = 1,
                       color = ~pal(type),
                       radius = 7,
                       opacity = 1,
                       label = labels,
                       labelOptions = labelOptions(direction = "bottom",
                                                   style = list(
                                                     "font-size" = "12px",
                                                     "border-color" = "rgba(0,0,0,0.5)",
                                                     direction = "auto"))) %>%
      addLegend("bottomleft",
                pal = pal,
                values =  ~type,
                title = "Type",
                opacity = 0.9)
  })

  output$allgrctable <- renderTable({
    table <- read.csv("data/isochrones/tables/grc_iso_table.csv")
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)

}

shinyApp(ui = ui, server = server)

