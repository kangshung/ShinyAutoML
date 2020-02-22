function(input, output, session) {
  data <- reactive({
    req(input$dataset)
    fread(input$dataset$datapath, stringsAsFactors = T)
  })

  data_NA <- reactive({
    as.data.table(unlist(lapply(data(), function(x) sum(is.na(x)))), T)[, V1 := reorder(V1, V2)][]
  })

  output$plotNA <- renderPlotly({
    req(nrow(data_NA()[V2 > 0]) != 0)

    plot_ly(data_NA()) %>%
      add_bars(x = ~ V2, y = ~ V1) %>%
      layout(
        xaxis = list(title = 'Number of observations'),
        yaxis = list(title = ''),
        title = "Number of NA's"
      )
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
      h2o.automl(
        input$independent,
        input$response,
        h2o_data(),
        max_runtime_secs = input$seconds
      )
    )
  })

  output$leaderboard <- renderPrint({
    leaderboard()@leaderboard
  })
}