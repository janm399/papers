library(parcoords)
library(dplyr)
asa <- read.csv("~/Desktop/x.tsv", sep = "\t", row.names = NULL)

asa %>% 
  select(score, performance, steps_to_deploy, logging) %>%
  parcoords(brushMode = "1d-axes-multi"
            ,reorderable = TRUE
            ,alphaOnBrushed = 0.15
            ,color = list(colorBy = "score", colorScale = htmlwidgets::JS("d3.scale.category10()")))
