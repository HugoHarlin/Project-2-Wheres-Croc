rm(list = ls())
#setwd("~/GitHub/Project-2-Wheres-Croc/")
setwd("~/Teknisk Fysik/AI/Project-2-Wheres-Croc")
library(WheresCroc)
source("myFunction.R")
source("shortestPath.R")
source("appendSorted.R")

#runWheresCroc(makeMoves = myFunction, showCroc = T)

testWC(myFunction =  myFunction)








