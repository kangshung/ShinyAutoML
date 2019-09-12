if (!require('pacman')) install.packages('pacman')
pacman::p_load(shiny, qs, magrittr, data.table, plotly, forcats)

gv <- qread('gun_violence.qs')
gv_cleaned <- qread('gun_violence_cleaned.qs')

no_of_NA <- as.data.table(unlist(lapply(gv, function(x) sum(is.na(x)))), T)
no_of_NA[, V1 := fct_reorder(V1, V2)]

no_of_NA_plot <- plot_ly(no_of_NA) %>% 
  add_bars(x = ~V2, y = ~V1) %>% 
  layout(xaxis = list(title = 'Number of observations'),
         yaxis = list(title = ''), title = 'Number of empty fields in variables')