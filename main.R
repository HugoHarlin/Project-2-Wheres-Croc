rm(list = ls())
#setwd("~/GitHub/Project-2-Wheres-Croc/")
setwd("~/Teknisk Fysik/AI/Project-2-Wheres-Croc")
library(WheresCroc)
source("moveRanger.R")
source("shortestPath.R")
source("appendSorted.R")

#runWheresCroc(makeMoves = moveRanger, showCroc = T)
testWC(myFunction =  moveRanger)






