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
## TODO: Since we are hosting data on github, please change the directory of the data.
dd <- read.csv("../meta_analysis_20220203_example_code/input/Aim2_UDS_logistic_model_Either_UDS_v2.csv")

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
                 tabPanel("Model Output", value = "data",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Modeling"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   tabsetPanel(
                                    #start tab 1
                                     tabPanel("Global Model",
                                              h3(strong(""), align = "center"),
                                              fluidRow(style='margin: 6px;',
                                                       column(4,
                                                              h3(strong("Heading 1")),
                                                             
                                                              h3(strong("Heading 2")),
                                                              p(""),
                                                             ),
                                                       column(8, 
                                                              h3(strong("Figures")),
                                                              # Select type of trend to plot
                                                              selectInput(inputId = "odds", label = strong("Relative Risk/Odds Ratio"),
                                                                          choices = c( "Relative Risk" = "RR", "Odds Ratio"= "OR"),
                                                                          selected = "RR"),
                                                              plotlyOutput(outputId = "regressionPlot", height = 600)
                                                              )
                                                       
                                                      ),
                                           
                                              
                                              ),
                                     #start tab 2
                                     tabPanel("Variable Plot",  
                                              h3(strong(""), align = "center") ,
                                              fluidRow(style='margin: 6px;',
                                                       column(4,
                                                              h3(strong("Heading 1")),
                                                              
                                                              h3(strong("Heading 2")),
                                                              p(""),
                                                       ),
                                                       column(8, 
                                                              h3(strong("Figures")),
                                                              # Select type of trend to plot
                                                              selectInput(inputId = "var", label = strong("Variable"),
                                                                          choices = variable,
                                                                          selected = "Age Index"),
                                                              plotOutput(outputId = "plot_by_variable", height = 600)
                                                            )
                                                       
                                                    )
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
server <- function(input, output, session){
  # Run JavaScript Code
  #runjs(jscode)

  
  ### ggplot 
  output$regressionPlot <- renderPlotly({
    
    if (input$odds == "RR"){   #log odds ratio 
      xintercept = 0
    }else if (input$odds == "OR"){   #odds ratio
      xintercept = 1
    }else{
      xintercept = NA
    }
  
    ### ggplot the log OR
    p <- ggplot_data_long%>%
      #filter(odds == "RR")%>%
      filter(odds == input$odds)%>% 
      select(-c(order,odds))%>%
      spread(est, value)%>%
      ggplot(aes(x = (estimate), y = variables, group = 1, variables=variables, description = description)) +
      geom_errorbarh(height = 0.0, size = 1.8, aes(xmin = pcilb, xmax = pciub), colour="grey88", alpha = 1) + #grey88
      geom_errorbarh(height = 0.0, size = 0.8, aes(xmin = cilb, xmax = ciub), colour="grey22", alpha = 0.5) + #grey22
      geom_point(colour = "black", size = 1.8, alpha = 1) +
      labs(y = NULL, title = NULL ) + 
      #geom_vline(xintercept = 0, linetype = "dashed", color = "blue", alpha = 0.5) +
      geom_vline(xintercept = xintercept, linetype = "dashed", color = "blue", alpha = 0.5)+
      theme_bw() +
      theme(axis.text.y = element_text(angle = 0, hjust = 1, size = rel(1.2)),
            axis.title.y = element_text(size = rel(1.2)),
            strip.text.y = element_text(size = rel(0.6)),
            axis.text.x = element_text(size = rel(1.1)),
            axis.title.x = element_text(size = rel(1.2), hjust = 0.5),
            plot.title = element_text(size = rel(1.2)),
            strip.background = element_rect(fill="gray95"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      ) +
      facet_grid(variablesgroup ~ ., drop = TRUE, scales = "free", space = "free")
    
    
    if(input$odds == "RR"){
      p <- p +
        scale_x_continuous(
          breaks = seq(-2, 2, by = 0.5)
        )
    }else if (input$odds == "OR"){

       p <- p + coord_trans(x = "log2") +
         scale_x_continuous(limits = c(0.5, 8), # make the x range to be wider on the left side
                           breaks = seq(0.5, 8, by = 1)
                           #,
                          # label = c("0.1", "0.2", "0.5", "1", "2", "4", "6")
                          )

    }
    
    ply <- ggplotly(p,tooltip =  c("description"))%>%
      layout(legend = list(
        orientation = "h"
      ))
    ply

    
  })
  
  output$plot_by_variable <- renderPlot({
  #  var = "Age Index"
   # metahksj <- metahksj_ls[[var]]
   # dat <- dat_ls[[var]]
     metahksj <- metahksj_ls[[input$var]]
     dat <- dat_ls[[input$var]]
    
      forest(metahksj, 
           addfit = FALSE, # set this to false to suppress global, will manually add later
           addcred = FALSE, # set this to false to suppress global, will manually add later
           slab = dat$state, # study label
           ylim = c(0, metahksj$k+3),
           rows = c((metahksj$k+1):2), # can be adjusted, height location of display [leave room for global at bottom]
           mlab = "Summary:", 
           
           xlab = input$var, # x-axis label
           psize = 0.8, # dot size
           level = 95, # CI level
           refline = 0, # vertical reference line
           pch = 19, # dot shape/type
           # transf = exp, # whether transformation of scale should be done
           showweights = FALSE, 
           header = c("State", "Log OR [95% CI]"), # CHECK LABEL TO BE Log OR
           top = 2) # Plots 95% CI and 95% PI
        addpoly(metahksj, row = 0.5, cex = 0.65, mlab = "Global", addcred = TRUE, 
            # transf = exp, # whether transformation of scale should be done
            level = 0.9, annotate = TRUE) # in this way, the CI will be 95%, the PI will be 90% [this is a work around]
    abline(h = 1)
  })
}

shinyApp(ui = ui, server = server)
