if (!require('pacman')) install.packages("pacman")
pacman::p_load(data.table,shiny,dplyr,magrittr,stringr,ggplot2,plotly,DT,pROC,leaflet,randomForest,rpart,ipred,rpart.plot,lubridate,tidyr,zoo,reshape2,rgdal,tmap,spData,rvest,caret,rpart,rpart.plot,naivebayes)


ui <- fluidPage(
  navbarPage('Criminals!',
             tabPanel('Dataset overview',
                      tabsetPanel(
                        tabPanel('Dataset table', br(),
                                 sidebarPanel(width = 2,
                                              sliderInput('slider', 'Choose the sample size:', 5, 25, 10),
                                              actionButton('refresh', 'Refresh sample'), br(), br(),
                                              selectInput('variabT', 'Choose variable(s) to show:', 'NA', multiple = T),
                                              radioButtons('typeofdataset', 'Choose the dataset:', c('Original', 'Fixed'))
                                 ),
                                 mainPanel(width = 10,
                                           div(dataTableOutput('dataT'), style = 'font-size: 80%')
                                 )
                        ),
                        tabPanel('Variables\' distributions', br(),
                                 radioButtons('typeofplot', 'Choose the plot:', c('Variable distribution', 'Time series')),
                                 conditionalPanel('input.typeofplot == "Variable distribution"',
                                                  sidebarPanel(width = 2,
                                                               selectInput('variabD', 'Choose a variable to show:', c('injured' = 'n_injured', 'killed' = 'n_killed')),
                                                               radioButtons('outliers', 'Remove outliers?', c('Yes', 'No'), 'No'),
                                                               radioButtons('log10scale', 'Use log10 y-scale?', c('Yes', 'No'), 'No')),
                                                  mainPanel(width = 10,
                                                            plotlyOutput('plotD', height = '500px'))
                                 ),
                                 conditionalPanel('input.typeofplot == "Time series"',
                                                  sidebarPanel(width = 2,
                                                               selectInput('variabTS', 'Choose a variable to show:', c('injured' = 'injured_total', 'killed' = 'killed_total'))),
                                                  mainPanel(width = 10,
                                                            plotlyOutput('plotTS', height = '500px'))
                                 )
                        )
                      )
             ),
             tabPanel('NA\'s overview',
                      verticalLayout(
                        radioButtons('choose', 'The type of output:', c('Summary of original variables' = 's1',
                                                                    'Summary of fixed variables' = 's2',
                                                                    'Standalone plots' = 'p1',
                                                                    'A comparison plot' = 'p2'), inline = T),
                        conditionalPanel('input.choose == "p1"',
                                         plotlyOutput('separate')),
                        conditionalPanel('input.choose == "p2"',
                                         plotlyOutput('together')),
                        conditionalPanel('input.choose == "s1"',
                                         h2('Original dataset'), h4('with both NA\'s and \'\' (empty strings)'),
                                         verbatimTextOutput('original')),
                        conditionalPanel('input.choose == "s2"',
                                         h2('Fixed dataset'), h4('only with NA\'s'),
                                         verbatimTextOutput('fixed'))
                      )
             ),
             tabPanel('Correlations',
                      sidebarPanel(width = 2,
                                   radioButtons('corchoice', 'Choose the correlation type:', c('pearson', 'spearman'))),
                      mainPanel(width = 10,
                                plotlyOutput('cor', height = '500px'))),
             tabPanel('Map',
                      numericInput('mapsample', 'Choose the sample size:', 50, 10, 5000),
                      actionButton('clickmapupdate', 'Update'), br(),
                      leafletOutput('map')
             )
             # navbarMenu('Modelling',
             #   # tabPanel('Logistic regression',
             #   #          sidebarPanel(width = 3,
             #   #                       selectInput('y', 'Choose the dependent variable:', 'NA', selected = NULL),
             #   #                       selectInput('x', 'Choose the independent variable(s):', 'NA', multiple = T, selected = NULL)
             #   #          ),
             #   #          mainPanel(width = 9,
             #   #                    tabsetPanel(
             #   #                      tabPanel('Model',
             #   #                        mainPanel(
             #   #                          div(verbatimTextOutput('summary'), style = "font-size:150px")
             #   #                        )
             #   #                      ),
             #   #                      tabPanel('Confusion matrix',
             #   #                        mainPanel(
             #   #                          div(verbatimTextOutput('confusionMatrix'), style = "font-size:150px")
             #   #                        )
             #   #                      ),
             #   #                      tabPanel('ROC curve',
             #   #                        mainPanel(
             #   #                          plotOutput('ROC', width = '1000px')
             #   #                        )
             #   #                      )
             #   #                    )
             #   #          )
             #   # ),
             #   tabPanel('Decision tree',
             #            sidebarPanel(width = 3,
             #                         selectInput('ytree', 'Choose the dependent variable:', 'NA', selected = NULL),
             #                         selectInput('xtree', 'Choose the independent variable(s):', 'NA', multiple = T, selected = NULL)
             #            ),
             #            mainPanel(width = 9,
             #                      tabsetPanel(
             #                        tabPanel('Model',
             #                                 mainPanel(
             #                                   div(verbatimTextOutput('summarytree'), style = "font-size:150px")
             #                                 )
             #                        ),
             #                        tabPanel('Confusion matrix',
             #                                 mainPanel(
             #                                   div(verbatimTextOutput('confusionMatrixtree'), style = "font-size:150px")
             #                                 )
             #                        ),
             #                        tabPanel('ROC curve',
             #                                 mainPanel(
             #                                   plotOutput('ROCtree', width = '1000px')
             #                                 )
             #                        ),
             #                        tabPanel('Decision tree',
             #                                 mainPanel(
             #                                   plotOutput('dectree')
             #                                 )
             #                        )
             #                      )
             #            )
             #   ),
             #   tabPanel('Naive bayes',
             #            sidebarPanel(width = 3,
             #                         selectInput('ynb', 'Choose the dependent variable:', 'NA', selected = NULL),
             #                         selectInput('xnb', 'Choose the independent variable(s):', 'NA', multiple = T, selected = NULL)
             #            ),
             #            mainPanel(width = 9,
             #                      tabsetPanel(
             #                        tabPanel('Model',
             #                                 mainPanel(
             #                                   div(verbatimTextOutput('summarynb'), style = "font-size:150px")
             #                                 )
             #                        ),
             #                        tabPanel('Confusion matrix',
             #                                 mainPanel(
             #                                   div(verbatimTextOutput('confusionMatrixnb'), style = "font-size:150px")
             #                                 )
             #                        ),
             #                        tabPanel('ROC curve',
             #                                 mainPanel(
             #                                   plotOutput('ROCnb', width = '1000px')
             #                                 )
             #                        )
             #                      )
             #            )
             #   ),
             #   tabPanel('Bagging',
             #            sidebarPanel(width = 3,
             #                         selectInput('yb', 'Choose the dependent variable:', 'NA', selected = NULL),
             #                         selectInput('xb', 'Choose the independent variable(s):', 'NA', multiple = T, selected = NULL)
             #            ),
             #            mainPanel(width = 9,
             #                      tabsetPanel(
             #                        tabPanel('Model',
             #                                 mainPanel(
             #                                   div(verbatimTextOutput('summaryb'), style = "font-size:150px")
             #                                 )
             #                        ),
             #                        tabPanel('Confusion matrix',
             #                                 mainPanel(
             #                                   div(verbatimTextOutput('confusionMatrixb'), style = "font-size:150px")
             #                                 )
             #                        ),
             #                        tabPanel('ROC curve',
             #                                 mainPanel(
             #                                   plotOutput('ROCb', width = '1000px')
             #                                 )
             #                        )
             #                      )
             #            )
             #   ),
             #   tabPanel('RandomForest',
             #            sidebarPanel(width = 3,
             #                         selectInput('yrf', 'Choose the dependent variable:', 'NA', selected = NULL),
             #                         selectInput('xrf', 'Choose the independent variable(s):', 'NA', multiple = T, selected = NULL)
             #            ),
             #            mainPanel(width = 9,
             #                      tabsetPanel(
             #                        tabPanel('Model',
             #                                 mainPanel(
             #                                   div(verbatimTextOutput('summaryrf'), style = "font-size:150px")
             #                                 )
             #                        ),
             #                        tabPanel('Confusion matrix',
             #                                 mainPanel(
             #                                   div(verbatimTextOutput('confusionMatrixrf'), style = "font-size:150px")
             #                                 )
             #                        ),
             #                        tabPanel('ROC curve',
             #                                 mainPanel(
             #                                   plotOutput('ROCrf', width = '1000px')
             #                                 )
             #                        )
             #                      )
             #            )
             #   )
             # )
             # # tabPanel('Models summary and comparison',
             # #          DTOutput('comparison'),
             # #          selectInput('whattoplot', 'What to plot?', NULL),
             # #          div(plotlyOutput('comparisonplot', width = '700px'), align = 'center')
             # #          )
  )
)

server <- function(input, output, session) {
  
  readRDS('gv') -> data; data_original = data
  for (i in 1:ncol(data)) data[,i] = replace(data[,i], which(data[,i] == ''), NA)
  
  data %>% mutate(date = as.Date(date)) %>% filter(date >= '2014-01-01') %>% 
    mutate(year = lubridate::year(date), month = lubridate::month(date)) %>% 
    select(date, n_injured, n_killed, year, month) %>% group_by(month, year) %>% 
    summarise(injured_total = sum(n_injured), killed_total = sum(n_killed)) %>% 
    unite('date', year, month, sep = '-') %>% mutate(date = as.yearmon(date)) %>% arrange(date) -> timeseries
  
  data %>% select(longitude, latitude, state, city_or_county) -> leaflet_data
  
  readRDS('gv2') -> gun_violence
  picked_vars = c('n_victims','n_unharmed','n_arrested','n_injured','average_age')
  
  observe({
    updateSelectInput(session, 'variabT', choices = colnames(data))
    updateSelectInput(session, 'y', choices = names(gun_violence)[str_detect(names(gun_violence), 'is_')])
    updateSelectInput(session, 'x', choices = names(gun_violence)[!names(gun_violence) == input$y])
    updateSelectInput(session, 'ytree', choices = names(gun_violence)[str_detect(names(gun_violence), 'is_')])
    updateSelectInput(session, 'xtree', choices = names(gun_violence)[!names(gun_violence) == input$y])
    updateSelectInput(session, 'ynb', choices = names(gun_violence)[str_detect(names(gun_violence), 'is_')])
    updateSelectInput(session, 'xnb', choices = names(gun_violence)[!names(gun_violence) == input$y])
    updateSelectInput(session, 'yb', choices = names(gun_violence)[str_detect(names(gun_violence), 'is_')])
    updateSelectInput(session, 'xb', choices = names(gun_violence)[!names(gun_violence) == input$y])
    updateSelectInput(session, 'yrf', choices = names(gun_violence)[str_detect(names(gun_violence), 'is_')])
    updateSelectInput(session, 'xrf', choices = names(gun_violence)[!names(gun_violence) == input$y])
    updateSelectInput(session, 'whattoplot', choices = names(tableend()))
  })
  
  select_variabs = reactive({
    req(input$x)
    gun_violence %>% select(input$y, input$x) %>% mutate_at(input$y, factor)
  })
  
  select_variabsnb = reactive({
    req(input$xnb)
    gun_violence %>% select(input$ynb, input$xnb) %>% mutate_at(input$ynb, factor)
  })
  
  select_variabstree = reactive({
    req(input$xtree)
    gun_violence %>% select(input$ytree, input$xtree) %>% mutate_at(input$ytree, factor)
  })
  
  select_variabsb = reactive({
    req(input$xb)
    gun_violence %>% select(input$yb, input$xb) %>% mutate_at(input$yb, factor)
  })
  
  select_variabsrf = reactive({
    req(input$xrf)
    gun_violence %>% select(input$yrf, input$xrf) %>% mutate_at(input$yrf, factor)
  })
  
  
  set.seed(2137)
  bootstrap = sample.int(nrow(gun_violence), nrow(gun_violence), T) %>% unique
  
  train = reactive({
    select_variabs()[bootstrap,]
  })
  
  trainnb = reactive({
    select_variabsnb()[bootstrap,]
  })
  
  traintree = reactive({
    select_variabstree()[bootstrap,]
  })
  
  trainb = reactive({
    select_variabsb()[bootstrap,]
  })
  
  trainrf = reactive({
    select_variabsrf()[bootstrap,]
  })
  
  
  test = reactive({
    select_variabs()[-bootstrap,]
  })
  
  testnb = reactive({
    select_variabsnb()[-bootstrap,]
  })
  
  testtree = reactive({
    select_variabstree()[-bootstrap,]
  })
  
  testb = reactive({
    select_variabsb()[-bootstrap,]
  })
  
  testrf = reactive({
    select_variabsrf()[-bootstrap,]
  })
  
  
  model.glm = reactive({
    req(input$x)
    glm(as.formula(paste(input$y, paste(input$x, collapse = ' + '), sep = ' ~ ')), binomial, train())
  })
  
  model.nb = reactive({
    req(input$xnb)
    naive_bayes(as.formula(paste(input$ynb, paste(input$xnb, collapse = ' + '), sep = ' ~ ')), trainnb())
  })
  
  model.tree = reactive({
    req(input$xtree)
    rpart(as.formula(paste(input$ytree, paste(input$xtree, collapse = ' + '), sep = ' ~ ')), traintree(), method = 'class')
  })
  
  model.b = reactive({
    req(input$xb)
    bagging(as.formula(paste(input$yb, paste(input$xb, collapse = ' + '), sep = ' ~ ')), trainb())
  })
  
  model.rf = reactive({
    req(input$xb)
    randomForest(as.formula(paste(input$yrf, paste(input$xrf, collapse = ' + '), sep = ' ~ ')), trainrf())
  })
  
  
  output$summary = renderPrint({
    summary(model.glm())
  })
  
  output$summarynb = renderPrint({
    summary(model.nb())
  })
  
  output$summarytree = renderPrint({
    summary(model.tree())
  })
  
  output$summaryb = renderPrint({
    model.b()
  })
  
  output$summaryrf = renderPrint({
    summary(model.rf())
  })
  
  conmatrix = reactive({
    confusionMatrix(predict(model.glm(), test(), type = 'response') %>% round %>% factor, test()[[input$y]])
  })
  conmatrixnb = reactive({
    confusionMatrix(predict(model.nb(), testnb()), testnb()[[input$ynb]])
  })
  conmatrixtree = reactive({
    confusionMatrix(predict(model.tree(), testtree(), type = 'class'), testtree()[[input$ytree]])
  })
  conmatrixb = reactive({
    confusionMatrix(predict(model.b(), testb()), testb()[[input$yb]])
  })
  conmatrixrf = reactive({
    confusionMatrix(predict(model.rf(), testrf()), testrf()[[input$yrf]])
  })
  
  output$confusionMatrix = renderPrint({
    conmatrix()
  })
  
  output$confusionMatrixnb = renderPrint({
    conmatrixnb()
  })
  
  output$confusionMatrixtree = renderPrint({
    conmatrixtree()
  })
  
  output$confusionMatrixb = renderPrint({
    conmatrixb()
  })
  
  output$confusionMatrixrf = renderPrint({
    conmatrixrf()
  })
  
  
  output$ROC = renderPlot({
    plot(roc(test()[[input$y]], predict(model.glm(), test(), type = 'response')))
  })
  
  output$ROCnb = renderPlot({
    plot(roc(testnb()[[input$ynb]], as.numeric(predict(model.nb(), testnb()))))
  })
  
  output$ROCtree = renderPlot({
    plot(roc(testtree()[[input$ytree]], as.numeric(predict(model.tree(), testtree(), type = 'class'))))
  })
  
  output$ROCb = renderPlot({
    plot(roc(testb()[[input$yb]], as.numeric(predict(model.b(), testb()))))
  })
  
  output$ROCrf = renderPlot({
    plot(roc(testrf()[[input$yrf]], as.numeric(predict(model.rf(), testrf()))))
  })
  
  output$dectree = renderPlot({
    rpart.plot(model.tree())
  })
  
  tableend = reactive({
    rbind(round(c(conmatrixnb()$overall[-(6:7)], conmatrixnb()$byClass[c(1,2,5)]), 3),
          round(c(conmatrixtree()$overall[-(6:7)], conmatrixtree()$byClass[c(1,2,5)]), 3),
          round(c(conmatrixb()$overall[-(6:7)], conmatrixb()$byClass[c(1,2,5)]), 3),
          round(c(conmatrixrf()$overall[-(6:7)], conmatrixrf()$byClass[c(1,2,5)]), 3)) %>% as_tibble %>% 
      `rownames<-`(c('Naive Bayes', 'Decision tree', 'Bagging of trees', 'Random forest'))
  })
  
  output$comparison = renderDataTable({
    tableend()
  })
  
  output$comparisonplot = renderPlotly({
    req(input$whattoplot)
    ggplotly(ggplot(tableend() %>% mutate(Name = rownames(.)), aes(x = Name, fill = rownames(tableend()))) + geom_col(aes_string(y = input$whattoplot)) +
               theme_void() + ggtitle(paste(input$whattoplot, 'in different models')) +
               scale_fill_manual(name = 'Models:', labels = rownames(tableend()), values = rainbow(5)),
             tooltip = c('x','y'))
  })
  
  output$original = renderPrint({
    glimpse(data_original)
  })
  
  output$fixed = renderPrint({
    glimpse(data)
  })
  
  dataset = reactive({
    if (input$typeofdataset == 'Original') data_original
    else data
  })
  
  sample = reactive({
    input$refresh
    
    isolate(sample_n(dataset(), input$slider))
  })
  
  output$dataT = renderDataTable({
    req(input$variabT)
    
    sample() %>% select(input$variabT) %>% 
      datatable(class = 'cell-border stripe', options = list(scrollX = T, pageLength = 25), rownames = F)
  })
  
  output$plotD = renderPlotly({
    if (input$variabD == 'n_killed') threshold = 12
    else if (input$variabD == 'n_injured') threshold = 21
    
    if (input$outliers == 'Yes') data_check = data[data[input$variabD] < threshold,]
    else data_check = data
    
    ggplot(data_check, aes_string(input$variabD)) + stat_count() + theme_minimal() +
      labs(title = paste('Distribution of variable', input$variabD)) -> ggg
    
    if (input$log10scale == 'Yes') {
      ggg = ggg + scale_y_log10()
    }
    
    ggplotly(ggg)
  })
  
  output$plotTS = renderPlotly({
    ggplot(timeseries, aes_string('date', input$variabTS)) + geom_line() + theme_minimal() +
      labs(title = paste('Number of people', input$variabTS, 'over time'), x = 'Time', y = paste('Number of people', input$variabTS)) +
      theme(plot.title = element_text(hjust = .5)) + scale_x_continuous() -> ggobj
    
    ggplotly(ggobj, tooltip = c('date', input$variabTS))
  })
  
  output$separate = renderPlotly({
    ggplot(NULL, aes(colnames(data_original), colSums(is.na.data.frame(data_original)))) + geom_col(fill = 'red') +
      coord_flip() + theme_minimal() + ylim(0, nrow(data)) -> gg1
    ggplot(NULL, aes(colnames(data), colSums(is.na.data.frame(data)))) + geom_col(fill = 'blue') +
      coord_flip() + theme_minimal() + ylim(0, nrow(data)) + ggtitle('Original vs fixed dataset NA\'s') +
      theme(plot.title = element_text(hjust = .5)) -> gg2
    
    subplot(ggplotly(gg1, tooltip = 'y'), ggplotly(gg2, tooltip = 'y'))
  })
  
  output$together = renderPlotly({
    ggplot(NULL, aes(colnames(data))) +
      geom_col(aes(y = colSums(is.na.data.frame(data_original))), fill = 'red', alpha = .5) +
      geom_col(aes(y = colSums(is.na.data.frame(data))), fill = 'blue', alpha = .5) +
      coord_flip() + theme_minimal() + ylim(0, nrow(data)) + ggtitle('Comparison of NA\'s distribution') +
      theme(axis.title = element_blank(), plot.title = element_text(hjust = .5)) -> gg3
    
    ggplotly(gg3, tooltip = 'y')
  })
  
  cor_data = reactive({
    data %>% select_if(is.numeric) %>% cor(use = 'complete.obs', method = input$corchoice) %>% round(2) %>% melt
  })
  
  output$cor = renderPlotly({
    ggplot(cor_data(), aes(Var1, Var2, fill = value)) + geom_tile() + theme_minimal() + geom_text(aes(label = value), size = 2) +
      scale_fill_gradient2(low = 'red', mid = 'white', high = 'green', limit = c(-1, 1), name = 'Correlation\nstrength:') +
      theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45), plot.title = element_text(hjust = .5)) +
      ggtitle(input$corchoice) -> gg
    
    ggplotly(gg)
  })
  
  generate = reactive({
    input$clickmapupdate
    
    isolate({
      leaflet_data %>% sample_n(input$mapsample)
    })
  })
  
  output$map = renderLeaflet({
    leaflet() %>% addTiles() %>% addMarkers(generate()$longitude, generate()$latitude, label = paste0(generate()$city_or_county, ', ', generate()$state))
  })
}

shinyApp(ui, server)
