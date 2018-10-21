Optd_l2 <- function(winner, p1, p2, p3){
  if("I1" %in% winner & "I2" %in% winner){
    d <- (p1 + p2)/2
    
  } else if("I1" %in% winner & "I3" %in% winner){
    d <- (p2 + p3)/2
    
  } else if("I1" %in% winner & "I4" %in% winner){
    d <- c(p1 - .Machine$double.eps, p3 + .Machine$double.eps)
    
  } else if("I2" %in% winner & "I3" %in% winner){
    d <- c((p1 + p2)/2, (p2 + p3)/2)
    
  } else if("I2" %in% winner & "I4" %in% winner){
    d <- (p1 + p2)/2
    
  } else if("I3" %in% winner & "I4" %in% winner){
    d <- (p2 + p3)/2
    
  }  else{
    stop("Something went wrong for Optd_l2")
  }
  return(d)
}