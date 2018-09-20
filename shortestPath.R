shortestPath = function(start,goal,edges){
  start_node = list(pos=start, cost=0, path=start);
  frontier = list(start_node)
  expanded = NA
  
  flag = 1;
  counter = 1
  while (flag) {
    #show("counter")
    #show(counter)
    #show("start")
    #show(start)
    #show("goal")
    #show(goal)
    
    expanded = frontier[[1]]
    frontier = frontier[-1]
 
    #show(frontier)
    #show("expanded")
    #show(expanded)
    
    if(expanded$pos == goal ){
      flag = 0
      return(expanded$path)
     
    }
    
    for (i in 1:length(edges[,1])) {
      if(edges[i,1] == expanded$pos){
        newNode = list(pos= edges[i,2], cost=expanded$cost +1, path=expanded$path)
        newNode$path[length(newNode$path)+1] = edges[i,2]
        frontier = appendSorted(newNode,frontier)
      }
      else if(edges[i,2] == expanded$pos){
        newNode = list(pos= edges[i,1], cost=expanded$cost +1, path=expanded$path)
        newNode$path[length(newNode$path)+1] = edges[i,1]
        frontier = appendSorted(newNode,frontier)
      }
    }
    #readline(prompt="Press [enter] to continue")
  }
  
}