get.enp <-
function(seats=NULL, votes=NULL, total=NA, method="Laakso/Taagepera"){
  if(method=="Golosov"){
      if(!is.null(seats)){
        round(sum((seats)/((seats)+((seats[1])^2)-((seats)^2))),2) -> Golosov
      } else{
        # round(sum((votes)/((votes)+((votes[1])^2)-((votes)^2))),2) -> Golosov 
      }
      return(Golosov)
    } 
  if(method=="LSq"){
    if(!is.null(seats)){
      round(sum((seats)/((seats)+((seats[1])^2)-((seats)^2))),2) -> LSq
    } else{
      
      # round(sum((votes)/((votes)+((votes[1])^2)-((votes)^2))),2) ->LSq 
    }
  return(LSq) 
  
  }else {
      if(!is.null(seats)){
        round(1/sum(table(seats)*(seats/sum(seats))^2),2) -> invHHIs
      }else{
        # round(sum(table(votes))^2/sum(table(votes)^2),2) -> invHHIs
        }
    return(invHHIs)
    }
}
