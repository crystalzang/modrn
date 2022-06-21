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

# example upload data
site <- c("A", "B", "C", "D", "A", "A")
outcome <- c(1, 0,0,0,1,1)
var1 <- c(100, 89, 120, 91, 111, 90)
var2 <- c("F", "M", "F", "F", "F", "M")
var3 <- c("Level3", "Level1", "Level2", "Level2","Level1", "Level3")
dt <- as.data.frame(cbind(site, outcome, var1,var2, var3))


source("01_aggregate.R")

source("helper_figures.R")

source("helper_data_prep.R")







# user -------------------------------------------------------------
ui <- navbarPage(title = "The Medicaid Outcomes Distributed Research Network (MODRN)",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),
                 
                 # Overview -----------------------------------------------------------
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
                 
                 # Data -----------------------------------------------------------
                 tabPanel("Data",
                          # fluidRow(style = "margin: 6px",
                          #          h1(strong("Data"), align = "center"),
                          #          p("", style = "padding-top: 10px;")
                          #         ),
                          fluidRow(style = "margin: 6px",
                                   column(4, h3(strong("Example Data"), align = ""),
                                          tableOutput('example_table')
                                          ),
                                   column(8,
                                          h3(strong("Upload Your Data"), align = ""),
                                          fileInput("upload", "Upload a file", multiple = T, accept = ".csv"),
                                          numericInput("n", "Rows", value = 5, min = 1, step = 1),
                                          tableOutput("uploaded_table"))
                                     
                                   )
                          ),
                 
                 # Modeling -----------------------------------------------------------
                 tabPanel("Model Output", value = "data",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Modeling"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   tabsetPanel(
                                    ## Global Model Plot
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
                                                              selectInput(inputId = "odds", label = strong("Plotting Scale"),
                                                                          choices = c( "Relative Risk" = "RR", "Odds Ratio"= "OR"),
                                                                          selected = "RR"),
                                                              plotlyOutput(outputId = "regressionPlot", height = 600)
                                                              )
                                                       
                                                      ),
                                           
                                              
                                              ),
                                     ## Individual Plot
                                     tabPanel("Variable Plot",  
                                              h3(strong(""), align = "center") ,
                                              fluidRow(style='margin: 6px;',
                                                       column(4,
                                                              # Select type of trend to plot
                                                              selectInput(inputId = "var", label = strong("Variable"),
                                                                          choices = variable,
                                                                          selected = "Male"),
                                                              sliderInput(inputId = "cl",  #confidence level of estimator
                                                                          label = "Confidence Level",
                                                                          min = 0.8, max = 0.975, value = 0.95, width = '300px'
                                                              ),
                                                              sliderInput(inputId = "pcl", #prediction confidence level
                                                                          label = "Prediction Confidence Level",
                                                                          min = 0.8, max = 0.95, value = 0.90, width = '300px'
                                                              )
                                                          ),
                                                       column(8, 
                                                              h3(strong("Figures")) ,
                                                              plotOutput(outputId = "plot_by_variable", height = 600)
                                                            )
                                                       
                                                    )
                                            )
                                            
                                    )
                 )),
                 
                 
                 # Contact -----------------------------------------------------------
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
  # Exampl data
  output$example_table <- renderTable(dt)
  
  # User-uploaded data
  data <- reactive({
    req(input$upload)
    
    # Validate file formate by checking its extension
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           validate("Invalid file; Please upload a .csv file")
    )
  })
  
  # User can select how many rows they want to see
  output$uploaded_table <- renderTable({
    head(data(), input$n) ### TODO: data() Generates error message
  })
  
  
  ## ggplot 
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
    
    plot_individual(ggplotdata,input$var, input$cl)
    
  })
}

shinyApp(ui = ui, server = server)
