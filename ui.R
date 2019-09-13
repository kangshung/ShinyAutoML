fluidPage(
  tags$head(tags$title('Gun violence modelling')),
  plotlyOutput('no_of_NA_plot_id'),
  selectInput('gv_variables_id', 'Pick variables', names(gv), multiple = T),
  dataTableOutput('gv_table_id'),
  selectInput('gv_cleaned_variables_id', 'Pick variables', names(gv_cleaned), multiple = T),
  dataTableOutput('gv_cleaned_table_id')
)