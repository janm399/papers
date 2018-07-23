library(parcoords)
library(dplyr)
library(ggvis)
library(htmlwidgets)
asa <- read.csv("~/Desktop/x.tsv", sep = "\t", row.names = "name")
sc <- read.csv("../sum/target/out.csv", row.names = NULL)

scChart <- sc %>%
  parcoords(brushMode = "1d-axes-multi"
            ,reorderable = TRUE
            ,rownames = FALSE
            ,alphaOnBrushed = 0.15
            ,color = list(colorBy = "project", colorScale = htmlwidgets::JS("d3.scale.category10()")))

saveWidget(scChart, file="~/Tmp/asa.html")
scChart

all <- asa %>% 
  select(score, performance, steps_to_deploy, logging)

all %>%
  parcoords(brushMode = "1d-axes-multi"
            ,reorderable = TRUE
            ,alphaOnBrushed = 0.15
            ,color = list(colorBy = "score", colorScale = htmlwidgets::JS("d3.scale.category10()")))

all %>% ggvis(x = ~score, y = ~steps_to_deploy,
              size = ~logging, fill = ~performance) %>%
  layer_points() %>%
  add_legend(c("size", "fill"))
