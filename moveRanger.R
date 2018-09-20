moveRanger = function (moveInfo, readings, positions, edges, probs){
  # calculating the probability of the crocs position based on readings
  show("moveInfo")
  show(moveInfo)
  
  readline(prompt="Press [enter] to continue")
  
  if(moveInfo$mem$status == 0){
    
    # transition vector
    transition = rep(1,40)
    
    for (i in 1:length(edges[,1])) {
      transition[edges[i,1]] = transition[edges[i,1]] +1 
      transition[edges[i,2]] = transition[edges[i,2]] +1
    }
  }
  
  # should only happen first time moveRanger is called for each simulation
  if(is.null(moveInfo$moves)){
    # P(S0 = i) for i = 1...40
    moveInfo$mem$probNodes = rep(1/40,40)
  }
  
  probability = rep(0,40)
  
  for (i in 1:length(edges[,1])) {
    probability[edges[i,1]] = probability[edges[i,1]] + moveInfo$mem$probNodes[edges[i,2]]*(1/transition[edges[i,2]])
    probability[edges[i,2]] = probability[edges[i,2]] + moveInfo$mem$probNodes[edges[i,1]]*(1/transition[edges[i,1]])
  }
  
  for (i in length(probability)) {
    probReadings = dnorm(readings[1],probs$salinity[i,1],probs$salinity[i,2]) +
                   dnorm(readings[2],probs$phosphate[i,1],probs$phosphate[i,2]) +
                   dnorm(readings[3],probs$nitrogen[i,1],probs$nitrogen[i,2])

probability[i] =(1/3)*probReadings*(probability[i] + moveInfo$mem$probNodes[i]*(1/transition[i]))
  }
  
  sumPRob = sum(probability)
  probability = probability/sum
  moveInfo$mem$probNodes = probability
  
  show("probabolities")
  show(probability)
  
  
  moveInfo$moves = c(0,0)
  return(moveInfo)
}