testWC = function (myFunction, verbose = 0, returnVec = FALSE, seed = 21, 
          timeLimit = 300) 
{
  set.seed(21)
  seeds = sample(1:25000, 500)
  startTime = Sys.time()
  mem = NA
  hmm = c()
  for (s in seeds) {
    midTime = Sys.time()
    if (as.numeric(midTime) - as.numeric(startTime) > timeLimit) {
      cat("\nRun terminated due to slowness.")
      return(NA)
    }
    set.seed(s)
    if (verbose == 2) 
      cat("\nNew game, seed", s)
    res = runWheresCroc(myFunction, doPlot = F, pause = 0, 
                        verbose = verbose == 2, returnMem = T, mem = mem)
    mem = res$mem
    hmm = c(hmm, res$move)
  }
  if (verbose >= 1) {
    endTime = Sys.time()
    cat("\nMean moves:", mean(hmm))
    cat("\nSD moves:", sd(hmm))
    cat("\nTime taken:", as.numeric(endTime) - as.numeric(startTime), 
        "seconds.")
  }
  if (returnVec) 
    return(hmm)
  else return(mean(hmm))
}