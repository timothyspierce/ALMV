# team tab -----------------------------------------------------------
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
                         img(src = "team-pierce.jpeg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                         img(src = "team-jacobs.jpeg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                         img(src = "team-burcham.jpeg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                         p(a(href = 'www.linkedin.com/in/timothyspierce', 'Timothy Pierce', target = '_blank'), "(Virginia Tech, Agricultural and Applied Economics);",
                           a(href = 'https://www.linkedin.com/in/ryan-jacobs-bb5727174/', 'Ryan Jacobs', target = '_blank'), "(Virginia Tech, Environmental Economics);",
                           a(href = 'https://www.linkedin.com/in/austin-burcham-9b32a81ab/', 'Austin Burcham', target = '_blank'), "(Virginia Tech, Computational Modeling and Data Analytics)."),
                         p("", style = "padding-top:10px;")
                  ),
                  column(6, align = "center",
                         h4(strong("VT AAEC Team Members")),
                         img(src = "faculty-chen.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                         img(src = "faculty-gupta.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                         img(src = "fellow-yang.jpeg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                         p(a(href = "https://aaec.vt.edu/people/faculty/chen-susan.html", 'Dr. Susan Chen', target = '_blank'), "(Faculty Lead);",
                           a(href = "https://aaec.vt.edu/people/faculty/gupta-anubhab.html", 'Dr. Anubhab Gupta', target = '_blank'), "(Faculty Lead);",
                           a(href = 'https://www.linkedin.com/in/yang-cheng-200118191/', 'Yang Cheng', target = '_blank'), "(Research Associate)."),
                         p("", style = "padding-top:10px;")
                  )
         ),
)