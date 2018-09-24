myFunction = function (moveInfo, readings, positions, edges, probs){
  # calculating the probability of the crocs position based on readings
  
  
  # the following only happens for the very first run of the simulation
  if(moveInfo$mem$status == 0){
    
    # transition vector
    transition = rep(1,40)
    
    for (i in 1:length(edges[,1])) {
      transition[edges[i,1]] = transition[edges[i,1]] +1 
      transition[edges[i,2]] = transition[edges[i,2]] +1
    }
    
    moveMatrix = array(0,c(40,40,2))
    for (i in 1:40) {
      
      if(i+1 < 41){
        for (j in (i+1):40) {
          #show("i")
          #show(i)
          #show("j")
          #show(j)
          shortPath = shortestPath(i,j,edges)
          #show("shortPath")
          #show(shortPath)
          
          if(length(shortPath) >2){
            moveMatrix[i,j,1] = shortPath[2]
            moveMatrix[i,j,2] = shortPath[3]
            moveMatrix[j,i,1] = shortPath[length(shortPath)-1]
            moveMatrix[j,i,2] = shortPath[length(shortPath)-2]
          }else if(length(shortPath) == 2){
            moveMatrix[i,j,1] = shortPath[2]
            moveMatrix[j,i,1] = shortPath[length(shortPath)-1]
          }
          #show("moveMatrix[i,j,]")
          #show(moveMatrix[i,j,])
          #show("moveMatrix[j,i,]")
          #show(moveMatrix[j,i,])
          #readline(prompt="Press [enter] to continue")
        }
      }
    }
    moveInfo$mem$transfer = transition
    moveInfo$mem$moveMatrix = moveMatrix
  }else{
    transition = moveInfo$mem$transfer
    moveMatrix = moveInfo$mem$moveMatrix
  }
  
  # should only happen first time moveRanger is called for each simulation
  if(is.null(moveInfo$moves)){
    # P(S0 = i) for i = 1...40
    moveInfo$mem$probNodes = rep(0,40)
    # we suspect that the crodocile is never placed on the same node as a hicker at T=0
    for(i in 1:40){
      if(positions[1] != i & positions[2] != i){
    moveInfo$mem$probNodes[i] = 1/38
      }
    }
  }
  
  probability = rep(0,40)
  
  # checks if the first hicker is dead
  if(!is.na(positions[1]) && positions[1] < 0){
    probability[-1*positions[1]] = 1
  }
  # checks if the second hicker is dead
  else if(!is.na(positions[2]) && positions[2] < 0){
    probability[-1*positions[2]] = 1
  }
  else{
    # If both hikers are alive the crockodile cant be in those waterholes
    moveInfo$mem$probNodes[positions[1]] = 0
    moveInfo$mem$probNodes[positions[2]] = 0
    
    # smoothing of probabilites test
    #moveInfo$mem$probNodes = moveInfo$mem$probNodes + 0.05
    #moveInfo$mem$probNodes = moveInfo$mem$probNodes/sum(moveInfo$mem$probNodes)
    
    #markov chain.
    for (i in 1:length(edges[,1])) {
      probability[edges[i,1]] = probability[edges[i,1]] + moveInfo$mem$probNodes[edges[i,2]]*(1/transition[edges[i,2]])
      probability[edges[i,2]] = probability[edges[i,2]] + moveInfo$mem$probNodes[edges[i,1]]*(1/transition[edges[i,1]])
    }
    
    for (i in 1:length(probability)) {
      probReadings = dnorm(readings[1],probs$salinity[i,1],probs$salinity[i,2]) +
        dnorm(readings[2],probs$phosphate[i,1],probs$phosphate[i,2]) +
        dnorm(readings[3],probs$nitrogen[i,1],probs$nitrogen[i,2])
      
      probability[i] =(1/3)*probReadings*(probability[i] + moveInfo$mem$probNodes[i]*(1/transition[i]))
    }
    
    #normalizing
    probability = probability/sum(probability)
    
  }
  
  moveInfo$mem$probNodes = probability
  
  
  probSorted = sort(probability, decreasing = T)
  #show("probabilities")
  #show(probSorted[1:5])
  index = rep(0,40)
  for (i in 1:40) {
    index[i] = match(probSorted[i],probability)
  }
  #show("top five nodes:")
  #show(index[1:5])
  #show("positions[3]")
  #show(positions[3])
  moveInfo$moves = moveMatrix[positions[3],index[1],]
  
  #if the next most probable waterhole is within one move, 
  #the ranger goes there and searches.
  holeVar = 0
  for(i in 2:40){
    if(  moveInfo$moves[2] != 0 & (probSorted[1]-probSorted[i])/(probSorted[1]+probSorted[i])< 0.30 & moveMatrix[positions[3],index[i],2] == 0){
      moveInfo$moves = moveMatrix[positions[3],index[i],]
      holeVar = i
      break
    }
    
  }
  #show("moveMatrix[positions[3],index[2],1]")
  #show(moveMatrix[positions[3],index[2],1])
  
  #if the ranger checks a hole and the crockodile isn't there, the probability is set to 0
  if(  moveInfo$moves[1] == 0){
    moveInfo$mem$probNodes[positions[3]] = 0
    if(holeVar != 0 ){
    moveInfo$moves[2] = moveMatrix[positions[3],index[1],1]
    }else{
      moveMatrix[positions[3],index[2],1]
    }
  }
  else if(moveInfo$moves[2] == 0){
    moveInfo$mem$probNodes[moveInfo$moves[1]] = 0;
  }
  
  #show(" moveInfo$moves")
  #show(moveInfo$moves)
  #readline(prompt="Press [enter] to continue")
  return(moveInfo)
}