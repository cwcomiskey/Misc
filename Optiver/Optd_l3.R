Optd_l3 <- function(winner, p1, p2, p3){
  if(!("I1" %in% winner)){
    d <- c((p1 + p2)/2, (p2 + p3)/2)
    
  } else if(!("I2" %in% winner)){
    d <- (p2 + p3)/2
    
  } else if(!("I3" %in% winner)){
    d <- c(p1 + p2)/2
    
  } else if(!("I4" %in% winner)){
    d <- c((p1 + p2)/2, (p2 + p3)/2)
    
  }  else{
    stop("Something went wrong for Optd_l3")
  }
  return(d)
}