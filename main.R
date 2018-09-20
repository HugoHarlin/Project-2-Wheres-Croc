rm(list = ls())
#setwd("~/GitHub/Project-1-Delivery-Man/")
setwd("~/Teknisk Fysik/AI/Project-2-Wheres-Croc")
library(WheresCroc)
source("moveRanger.R")
source("shortestPath.R")

runWheresCroc(makeMoves = moveRanger, showCroc = T)
#testWC(makeMoves = manualWC)