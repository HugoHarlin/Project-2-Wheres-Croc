#  for(i in 1:length(edges[,1])){
#    if(!is.na(positions[1]) &  positions[1] > 0){
#      if(edges[i,1] == positions[1] ){
#        transition[edges[i,2]] = transition[edges[i,2]] -1
#      }
#      if(edges[i,2] == positions[1] ){
#        transition[edges[i,1]] = transition[edges[i,1]] -1
#      }
#    }
#    
#    if(!is.na(positions[2]) &  positions[2] > 0){
#      if(edges[i,1] == positions[2] ){
#        transition[edges[i,2]] = transition[edges[i,2]] -1
#      }
#      if(edges[i,2] == positions[2] ){
#        transition[edges[i,1]] = transition[edges[i,1]] -1
#      }
#    }
#}