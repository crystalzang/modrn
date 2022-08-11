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
#dd <- read_csv("data/data.csv")

# example upload data
site <- c("A", "A", "A", "B", "B", "B")
Parameter <- c("Var1", "Var2", "Var3", "Var1", "Var2", "Var3")
Estimate <- c(1, 0,0,0,1,1)
StdErr <- c(0.1, 1.2, -2.2, 0.02, -0.43, 0.03)
dt <- as.data.frame(cbind(site, Parameter, Estimate, StdErr))

source("helper_figures.R")

source("helper_data_prep.R")






# user -------------------------------------------------------------
ui <- 
  navbarPage(
   # theme = shinythemes::shinytheme("journal"),
    title = "Online Random Effect Meta-Analysis Calculator",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),
                 
                 # Overview -----------------------------------------------------------
                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   br(""),
                                   h1(strong("Online Random Effect Meta-Analysis Calculator"),
                                      br(""),
                                      h2("Background"),
                                      p("This R Shiny online calculator performs a random effects meta-analysis using 
                                      the Hartung-Knapp-Sidik-Jonkman method and provide convenient visualization and 
                                      downloadable output summaries. Estimates from individual sites are combined to 
                                      produce pooled estimates. The required inputs are the site-specific estimates 
                                      for a single or multiple parameters, their standard errors, and columns that 
                                      identifies the site and the parameter. A sample input is provided under the 
                                      XXX tab. Important outputs include the pooled estimates along with confidence
                                      intervals (CI). In addition, it generates prediction intervals (PI) around the 
                                      pooled estimates, which represent the interval we would expect to contain the 
                                      estimates if the analysis had been conducted with a different sample of states. 
                                      The confidence level of the CI and PI can be separately specified and modified 
                                      via slider controls. "),
                                      


                                      h2("Title Three"),
                                      p("The calculation is based on the R package metafor (Viechtbauer, 2010). Plots are produced in R using ggplot2. "),

                                      h2("Legal Disclaimer"),
                                      p("The online meta-analysis calculator is strictly a research tool.
                                        Our team has made every attempt to ensure the accuracy and reliability
                                        of the information provided by this software. However, the information 
                                        is provided 'as is' without warranty of any kind. Neither Pitt nor the 
                                        investigators accept any responsibility or liability for the accuracy, 
                                        content, completeness, legality, or reliability of the information provided 
                                        by this software. No warranties, promises and/or representations of any 
                                        kind, expressed or implied, are given as to the nature, standard, accuracy 
                                        or otherwise of the information provided by the software nor to the suitability
                                        or otherwise of the information to your particular circumstances."),
                                      br()
                                   )
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: Aug 2022'))))
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
                                          fileInput(inputId="upload", label="Upload a file", multiple = F, accept = ".csv"), #other tabs can access this data using inputId
                                           dataTableOutput("data_upload")
                                         )
                                     
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
                                                              ),
                                                              # Select type of trend to plot
                                                              selectInput(inputId = "scale", label = strong("Plotting Scale"),
                                                                          choices = c( "Odds Ratio" = "OR", "Log Odds Ratio"= "logOR"),
                                                                          selected = "logOR")
                                                             ),
                                                       column(8, 
                                                              h3(strong("Figures")),
                                                              plotlyOutput(outputId = "globalPlot", height = 600)
                                                              )
                                                       
                                                      ),
                                           
                                              
                                              ),
                                     ## Individual Plot
                                     tabPanel("Variable Plot",  
                                              h3(strong(""), align = "center") ,
                                              fluidRow(style='margin: 6px;',
                                                       column(4,
                                                              # Select type of trend to plot
                                                              # selectInput(inputId = "var", label = strong("Variable"),
                                                              #           # choices = get_variable_names(input$upload),
                                                              #             choices = XXX, #todo
                                                              #             selected = "param_1"),
                                                              uiOutput("param_selection"), #widget for parameter selection
                                                              sliderInput(inputId = "cl_2",  #confidence level of estimator
                                                                          label = "Confidence Level",
                                                                          min = 0.8, max = 0.975, value = 0.95, width = '300px'
                                                              ),
                                                              sliderInput(inputId = "pcl_2", #prediction confidence level
                                                                          label = "Prediction Confidence Level",
                                                                          min = 0.8, max = 0.975, value = 0.90, width = '300px'
                                                              )
                                                          ),
                                                       column(8, 
                                                              h3(strong("Figures")) ,
                                                              plotOutput(outputId = "plot_by_variable", height = 600),
                                                              h3(strong("Export")) ,
                                                              downloadButton( "plot_by_variable_export", "Export All Plots")
                                                            )
                                                       
                                                    )
                                            ),
                                     ## Export
                                     tabPanel("Export",  
                                              h3(strong(""), align = "center") ,
                                              fluidRow(style='margin: 6px;',
                                                       column(4,
                                                              numericInput("cl_3", "Confidence Level:", 0.95, min = 0.8, max = 0.99),
                                                              #verbatimTextOutput("value_cl"),
                                                              numericInput("pcl_3", "Prediction Confidence Level:", 0.95, min = 0.8, max = 0.99)
                                                              #verbatimTextOutput("value_pcl")
                                                       ),
                                                       column(8, 
                                                              h3(strong("Export Model Output")),
                                                              p("Use Shift/Ctrl + Click for selecting/deselect multiple columns"),
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
                                   h4(strong("University of Pittsburgh School of Public Health")),
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
  # Example data
  output$example_table <- renderTable(dt)
  
  # User-uploaded data
  data <- reactive({
    #make sure dataset is uploaded
    req(input$upload)
    
    # Validate file formate by checking its extension
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           validate("Invalid file; Please upload a .csv file")
    )
  })

  output$value_cl <- renderText({ input$cl_3})
  output$value_pcl <- renderText({ input$pcl_3})
  
  # preview uploaded dataset
  output$data_upload <- renderDataTable(
    data()
  )
  

  # 1. global plot 
  output$globalPlot <- renderPlotly({
    plot_global(get(input$upload), input$cl, input$pcl, input$scale)    
  })
  
  # 2.individual plot depend on the selected parameter
  
  # variable selection from the uploaded dataset
  # in the rest of server, var() can be used as a function that returns a vector of parameter names 
  var <- reactive({
    data <- get(input$upload)
    # get_variable_names(mydata)
    get_variable_names(data)
  })
  
  output$param_selection <- renderUI({
    selectInput("var_selected", "Select parameter", choices = var())
  })
  
  output$plot_by_variable <- renderPlot({
    plot_individual(get(input$upload()), get(input$var_selected), cl = input$cl_2, pcl= input$pcl_2)
  })
  
  output$data_output <- renderDataTable(
    #output global results
    generate_global_estimates(get(input$upload), input$cl_3, input$pcl_3)%>%
      datatable(
      extensions=c("Select", "Buttons"), options = list(
        select = list(style = "os", items = "row"),
        dom = "Bfrtip",
        buttons = c("selectAll","selectNone", "selectColumns","copy", "csv", "excel", "pdf", "print")
      )
    )
  )
  
  output$plot_by_variable_export <- downloadHandler(
    plot_individual_export(get(input$upload), input$cl_2),
    
    filename = "MA_Forest_Plots.pdf",
    content = function(file) {
      file.copy("www/MA_Forest_Plots.pdf", file)
    }
  )
  
}

shinyApp(ui = ui, server = server)
