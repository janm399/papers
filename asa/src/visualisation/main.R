library(parcoords)
library(dplyr)
library(ggvis)
library(htmlwidgets)
asa <- read.csv("~/Desktop/asa/x.tsv", sep = "\t", row.names = "name")
sc <- read.csv("../sum/target/out.csv", row.names = NULL) %>% select(-file)

x <- sc %>% 
  group_by(project) %>%
  summarise_all(mean)

scChart <- sc %>%  parcoords(brushMode = "1d-axes-multi"
            ,reorderable = TRUE
            ,rownames = FALSE
            ,alphaOnBrushed = 0.15
            ,autoresize = TRUE
            ,width = NULL
            ,height = NULL
            ,color = list(colorBy = "project", colorScale = htmlwidgets::JS("d3.scale.category10()")))

saveWidget(scChart, file="~/Tmp/asa.html")
scChart

all <- asa

all %>%
  parcoords(brushMode = "1d-axes-multi"
            ,reorderable = TRUE
            ,alphaOnBrushed = 0.15
            ,color = list(colorBy = "score", colorScale = htmlwidgets::JS("d3.scale.category10()")))

all %>% ggvis(x = ~score, y = ~steps_to_deploy,
              size = ~logging, fill = ~performance) %>%
  layer_points() %>%
  add_legend(c("size", "fill"))
