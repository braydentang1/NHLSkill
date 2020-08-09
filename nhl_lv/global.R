library(tidyverse)
library(plotly)
library(shiny)

all_forwards_gte <- read_rds("results/models/gte/forwards.rds")
all_defenceman_gte <- read_rds("results/models/gte/defenceman.rds")
