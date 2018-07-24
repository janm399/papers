library(parcoords)
library(dplyr)
library(ggvis)
library(htmlwidgets)
asa <- read.csv("~/Desktop/asa/x.tsv", sep = "\t", row.names = "name")

asa %>%
  parcoords(brushMode = "1d-axes-multi"
            ,reorderable = TRUE
            ,alphaOnBrushed = 0.15
            ,color = list(colorBy = "score", colorScale = htmlwidgets::JS("d3.scale.category10()")))

asa %>% ggvis(x = ~score, y = ~steps_to_deploy,
              size = ~logging, fill = ~performance) %>%
  layer_points() %>%
  add_legend(c("size", "fill"))
