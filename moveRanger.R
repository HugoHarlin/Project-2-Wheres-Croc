moveRanger = function (moveInfo, readings, positions, edges, probs){
  # calculating the probability of the crocs position based on readings
  
  
  
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
    moveInfo$mem$probNodes = rep(1/40,40)
  }
  
  probability = rep(0,40)
  
  if(!is.na(positions[1]) && positions[1] < 0){
    probability[-1*positions[1]] = 1
  }
  else if(!is.na(positions[2]) && positions[2] < 0){
    probability[-1*positions[2]] = 1
  }
  else{
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
    
    probability = probability/sum(probability)
    
  }
  
  moveInfo$mem$probNodes = probability
  
  #show("probabilities")
  #show(max(probability))
  index = match(max(probability),probability)
  #show("index")
  #show(index)
  #show("positions[3]")
  #show(positions[3])
  
  moveInfo$moves = moveMatrix[positions[3],index,]
  #show(" moveInfo$moves")
  #show(moveInfo$moves)
  #readline(prompt="Press [enter] to continue")
  return(moveInfo)
}