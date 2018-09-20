WhereCroc_Code = function (makeMoves, doPlot = T, showCroc = F, pause = 1, verbose = T, 
          returnMem = F, mem = NA) 
{
  positions = sample(1:40, 4)
  points = getPoints()
  edges = getEdges()
  probs = getProbs()
  move = 0
  moveInfo = list(moves = c(), mem = list(status = 0))
  if (!all(is.na(mem))) 
    moveInfo$mem = mem
  first = T
  while (!is.na(positions[1])) {
    move = move + 1
    if (!first) {
      positions[1] = sample(getOptions(positions[1], edges), 
                            1)
      if (!is.na(positions[2]) && positions[2] > 0) {
        positions[2] = sample(getOptions(positions[2], 
                                         edges), 1)
      }
      else if (!is.na(positions[2]) && positions[2] < 0) {
        positions[2] = NA
      }
      if (!is.na(positions[3]) && positions[3] > 0) {
        positions[3] = sample(getOptions(positions[3], 
                                         edges), 1)
      }
      else if (!is.na(positions[3]) && positions[3] < 0) {
        positions[3] = NA
      }
      if (!is.na(positions[2]) && positions[2] == positions[1]) {
        positions[2] = -positions[2]
      }
      if (!is.na(positions[3]) && positions[3] == positions[1]) {
        positions[3] = -positions[3]
      }
    }
    else first = F
    if (doPlot) 
      plotGameboard(points, edges, move, positions, showCroc)
    Sys.sleep(pause)
    readings = getReadings(positions[1], probs)
    moveInfo = makeMoves(moveInfo, readings, positions[2:4], 
                         edges, probs)
    if (length(moveInfo$moves) != 2) {
      stop("Error! Passed makeMoves function should return a vector of two elements.")
    }
    for (m in moveInfo$moves) {
      if (m == 0) {
        if (positions[1] == positions[4]) {
          if (verbose) 
            cat("\nCongratualations - You got croc at move ", 
                move)
          if (returnMem) {
            mem = moveInfo$mem
            mem$status = 1
            return(list(move = move, mem = mem))
          }
          return(move)
        }
      }
      else {
        if (m %in% getOptions(positions[4], edges)) {
          positions[4] = m
        }
        else {
          warning("Invalid move.")
        }
      }
    }
  }
}