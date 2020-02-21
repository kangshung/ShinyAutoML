# if (!require('pacman')) install.packages('pacman')
# pacman::p_load(shiny, DT, data.table, h2o, DT, magrittr, plotly, here)
library(shiny); library(data.table); library(h2o); library(DT); library(magrittr); library(plotly); library(here)

h2o.init()
h2o.no_progress()