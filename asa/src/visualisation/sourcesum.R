library(parcoords)
library(dplyr)
library(tidyverse)

so <- read.csv("~/Downloads/QueryResults.csv")
x <- so %>% filter(str_detect(QTitle, "akka")) %>% arrange(desc(CreationDate))
