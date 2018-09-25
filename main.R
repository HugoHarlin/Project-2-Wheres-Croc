rm(list = ls())
#setwd("~/GitHub/Project-2-Wheres-Croc/")
setwd("~/Teknisk Fysik/AI/Project-2-Wheres-Croc")
library(WheresCroc)
source("myFunction.R")
source("shortestPath.R")
source("appendSorted.R")

#runWheresCroc(makeMoves = myFunction, showCroc = T)

mean = 0
num = 3
for (i in 1:num) {
  set.seed(i)
  show("i")
  show(i)
  mean = mean + (1/num)*testWC(myFunction =  myFunction)
}

show(mean)








