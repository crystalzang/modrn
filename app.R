if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(rsconnect)) install.packages("rsconnect", repos = "http://cran.us.r-project.org")
if(!require(shinycssloaders)) install.packages("shinycssloaders", repos = "http://cran.us.r-project.org")
if(!require(shinyjs)) install.packages("shinyjs", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")

if(!require(sjmisc)) install.packages("sjmisc", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")

if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(metafor)) install.packages("metafor", repos = "http://cran.us.r-project.org")

# Data
dd <- read.csv("data/Aim2_UDS_logistic_model_Either_UDS_v2.csv")

source("01_aggregate.R")

# user -------------------------------------------------------------
ui <- navbarPage(title = "The Medicaid Outcomes Distributed Research Network (MODRN)",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),
                 
                 # main -----------------------------------------------------------
                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   br(""),
                                   h1(strong("Title One"),
                                      br(""),
                                      h4("Title Two"),
                                      h4("Title Three"),
                                      h4("Title Four"),
                                      br()
                                   )
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: June 2022'))))
                 ),
                 
                 # data -----------------------------------------------------------
                 tabPanel("Data",
                        
                 ),
                 
                 # Modeling -----------------------------------------------------------
                 tabPanel("Tab 1", value = "data",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Modeling"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   tabsetPanel(
                                    
                                     tabPanel("Tab 1",
                                              h3(strong(""), align = "center"),
                                              fluidRow(style='margin: 6px;',
                                                       column(5,
                                                              h3(strong("Heading 1")),
                                                              # Select type of trend to plot
                                                              selectInput(inputId = "odds", label = strong("Odds/Odds Ratio"),
                                                                          choices = c("odds", "log_odds"),
                                                                          selected = "odds"),
                                                              h3(strong("Heading 2")),
                                                              p(""),
                                                             ),
                                                       column(7, h3(strong("Figures")),
                                                                      plotOutput(outputId = "plot", height = "600px")
                                                              )
                                                      )
                                              ),
                                     tabPanel("Tab 2",  
                                              h3(strong(""), align = "center")         
                                                    )
                                            )
                 )),
                 
                 
                 # contact -----------------------------------------------------------
                 tabPanel("Contact", value = "contact",
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
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
                          )
                ))


# server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  #runjs(jscode)

  
  ### ggplot 
  output$plot <- renderPlotly({
    p <-  ### ggplot the log OR
      ggplot_data_long%>%
      filter(odds == input$odds)%>%
      select(-c(order,odds))%>%
      spread(est, value)%>%
      ggplot(aes(x = (estimate), y = variables, group = 1, variables=variables)) +
      geom_errorbarh(height = 0.0, size = 1.8, aes(xmin = (pcilb), xmax = (pciub)), colour="grey88", alpha = 0.5) +
      geom_errorbarh(height = 0.0, size = 0.8, aes(xmin = (cilb), xmax = (ciub)), colour="grey22", alpha = 0.5) +
      geom_point(colour = "black", size = 1.8, alpha = 1) +
      labs(y = NULL, 
          # x = "Adjusted Log OR", ### plotting the LOG OR here
           title = NULL ) + 
      geom_vline(xintercept = 0, linetype = "dashed", color = "blue", alpha = 0.5) +
      theme_bw() +  
      theme(axis.text.y = element_text(angle = 0, hjust = 1, size = rel(1.2)),
            axis.title.y = element_text(size = rel(1.2)),
            strip.text.y = element_text(size = rel(0.6)),
            axis.text.x = element_text(size = rel(1.1)),
            axis.title.x = element_text(size = rel(1.2), hjust = 0.5),
            plot.title = element_text(size = rel(1.2)),
            strip.background = element_rect(fill="gray95"),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      ) +
      facet_grid(variablesgroup ~ ., drop = TRUE, scales = "free", space = "free")
    ggplotly(p,tooltip =  "variables")
    # ply <- ggplotly(p, tooltip = c("variables"))%>%
    #   layout(legend = list(
    #     orientation = "h"
    #     )
    #   )
    # ply
    
  })
}

shinyApp(ui = ui, server = server)
