fluidPage(
  tags$head(tags$title('AutoML Application')),
  sidebarPanel(
    fileInput("dataset", "Choose a dataset"),
    selectInput("response", "Pick a response variable", NULL),
    selectInput("independent", "Pick independent variables", NULL, multiple = T),
    shiny::numericInput("seconds", "Pick number of seconds to create models", 60, 10, 300),
    shiny::actionButton("automl", "Run h2o automl", icon("cloud"))
  ),
  mainPanel(tabsetPanel(
    tabPanel("Table", DTOutput("datatable")),
    tabPanel("NA's", plotlyOutput("plotNA")),
    tabPanel("Leaderboard", verbatimTextOutput("leaderboard"))
  ))
)