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
if(!require(vroom)) install.packages("vroom", repos = "http://cran.us.r-project.org")


# Data
dd <- read_rds("data/data.rds")

# example upload data
site <- c("A", "A", "A", "B", "B", "B")
Parameter <- c("Var1", "Var2", "Var3", "Var1", "Var2", "Var3")
Estimate <- c(1, 0,0,0,1,1)
StdErr <- c(0.1, 1.2, -2.2, 0.02, -0.43, 0.03)
dt <- as.data.frame(cbind(site, Parameter, Estimate, StdErr))

source("helper_figures.R")

source("helper_data_prep.R")


variable <- get_variable_names(dd)




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
                                          p("In your uploaded data, please the following format:"),
                                          tags$ul(
                                            tags$li("site: indicator for the center/state where the data was originally collected."), 
                                            tags$li("Parameter: predictors for the outcome"), 
                                            tags$li("Estimate: estimate for the outcome in log odds ratio"),
                                            tags$li("StdErr: standard error of the estimate")
                                          ),
                                          tableOutput('example_table')
                                          ),
                                   column(8,
                                          h3(strong("Upload Your Data"), align = ""),
                                          fileInput("upload", "Upload a file", multiple = F, accept = ".csv"),
                                          numericInput("n", "Rows", value = 5, min = 1, step = 1),
                                          tableOutput("data_upload"))
                                     
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
                                                              sliderInput(inputId = "cl",  #confidence level of estimator
                                                                          label = "Confidence Level",
                                                                          min = 0.8, max = 0.975, value = 0.95, width = '300px'
                                                              ),
                                                              sliderInput(inputId = "pcl", #prediction confidence level
                                                                          label = "Prediction Confidence Level",
                                                                          min = 0.8, max = 0.975, value = 0.90, width = '300px'
                                                              )
                                                             ),
                                                       column(8, 
                                                              h3(strong("Figures")),
                                                      
                                                              # Select type of trend to plot
                                                              #selectInput(inputId = "odds", label = strong("Plotting Scale"),
                                                              #            choices = c( "Relative Risk" = "RR", "Odds Ratio"= "OR"),
                                                              #            selected = "RR"),
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
                                                                          selected = "param_1"),
                                                              sliderInput(inputId = "cl",  #confidence level of estimator
                                                                          label = "Confidence Level",
                                                                          min = 0.8, max = 0.975, value = 0.95, width = '300px'
                                                              )
                                                          ),
                                                       column(8, 
                                                              h3(strong("Figures")) ,
                                                              plotOutput(outputId = "plot_by_variable", height = 600)
                                                            )
                                                       
                                                    )
                                            ),
                                     ## Export
                                     tabPanel("Export",  
                                              h3(strong(""), align = "center") ,
                                              fluidRow(style='margin: 6px;',
                                                       column(4,
                                                              # Select type of trend to plot
                                                              selectInput(inputId = "var", label = strong("Variable"),
                                                                          choices = variable,
                                                                          selected = "param_1"),
                                                              sliderInput(inputId = "cl",  #confidence level of estimator
                                                                          label = "Confidence Level",
                                                                          min = 0.8, max = 0.975, value = 0.95, width = '300px'
                                                              )
                                                       ),
                                                       column(8, 
                                                              h3(strong("Export Model Output")),
                                                             # selectInput("dataset", "Select a dataoutput",ls("package:datasets")),
                                                             dataTableOutput("data_output")
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
  output$data_upload <- renderTable({
    head(data(), input$n) ### TODO: data() Generates error message
  })
  
  
  ## ggplot 
  output$regressionPlot <- renderPlotly({
    plot_global(dd, input$cl, input$pcl)    
    
  })
  
  output$plot_by_variable <- renderPlot({
    plot_individual(dd, input$var, input$cl)
    
  })
  
  output$data_output <- renderDataTable(
    #output global results
    
    generate_global_estimates(dd, input$cl, input$pcl)%>%
      datatable(
      extensions="Buttons", options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print")
      )
    )
  )
  
}

shinyApp(ui = ui, server = server)
