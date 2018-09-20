shortestPath = function(start,goal,edges){
  start_node = list(pos=start, cost=0, path=list());
  frontier = list(start_node)
  expanded = NA
  
  flag = T
  while (flag) {
    expanded = frontier[[1]];
    frontier[[-1]]
    
    for (i in 1:length(edges[,1])) {
      if(edges[i,1] == expanded$pos){
        newNode = list(pos= edges[i,2], cost=expanded$cost +1, path=append(expanded$path,edges[i,2]))
        frontier = appendSorted(frontier, edges[i,2])
      }
      else if(edges[i,2] == expanded$pos){
        
      }
    }
    
  }
  
}