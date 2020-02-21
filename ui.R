fluidPage(
  tags$head(tags$title('Gun violence modelling')),
  sidebarPanel(
    fileInput("dataset", "Choose a dataset"),
    selectInput("response", "Pick a response variable", NULL),
    selectInput("independent", "Pick independent variables", NULL, multiple = T),
    shiny::numericInput("seconds", "Pick number of seconds to create models", 60, 10, 300),
    shiny::actionButton("automl", "Run h2o automl", icon("cloud"))
  ),
  mainPanel(
    verbatimTextOutput("leaderboard"),
    DTOutput("datatable"),
    plotlyOutput("NAplot")
  )
)