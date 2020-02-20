# if (!require('pacman')) install.packages('pacman')
# pacman::p_load(shiny, DT, data.table, h2o, DT, magrittr, plotly, here)
library(shiny); library(data.table); library(h2o); library(DT); library(magrittr); library(plotly); library(here)


h2o.init()
h2o.no_progress()

ui <- fluidPage(
    sidebarPanel(
        fileInput("dataset", "Choose a dataset"),
        selectInput("response", "Pick a response variable", NULL),
        selectInput("independent", "Pick independent variables", NULL, multiple = T),
        shiny::numericInput("seconds", "Pick number of seconds to create models", 60, 10, 300),
        shiny::actionButton("automl", "Run h2o automl", icon("cloud"))
    ),
    mainPanel(
        verbatimTextOutput("leaderboard"),
        DTOutput("datatable")
    )
)

server <- function(input, output, session) {
    data <- reactive({
        req(input$dataset)
        fread(input$dataset$datapath, stringsAsFactors = T)
    })
    
    output$datatable <- renderDT({
        data()
    })
    
    observe({
        shiny::updateSelectInput(session, "response", choices = names(data()))
        shiny::updateSelectInput(session, "independent", choices = names(data()))
    })
    
    h2o_data <- reactive(as.h2o(data()))
    
    leaderboard <- eventReactive(input$automl, {
        withProgress(
            message = "Running AutoML",
            h2o.automl(input$independent, input$response, h2o_data(), max_runtime_secs = input$seconds)
        )
    })
    
    output$leaderboard <- renderPrint({
        leaderboard()@leaderboard
    })
}

# Run the application 
shinyApp(ui = ui, server = server)