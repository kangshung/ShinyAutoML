function(input, output, session) {
  output$no_of_NA_plot_id <- renderPlotly({
    no_of_NA_plot
  })
  
  gv_variables <- reactive({
    gv_variables_chosen <- input$gv_variables_id
    gv[, ..gv_variables_chosen]
  })
  
  output$gv_table_id <- renderDataTable({
    gv_variables()
  })
  
  gv_cleaned_variables <- reactive({
    gv_cleaned_variables_chosen <- input$gv_cleaned_variables_id
    gv_cleaned[, ..gv_cleaned_variables_chosen]
  })
  
  output$gv_cleaned_table_id <- renderDataTable({
    gv_cleaned_variables()
  })
}