Optd_l1 <- function(winner, p1, p2, p3){
  if(winner == "I1"){
    d <- p1 - .Machine$double.eps
  } else if(winner == "I2"){
    d <- (p1 + p2)/2
  } else if(winner == "I3"){
    d <- (p2 + p3)/2
  } else if(winner == "I4"){
    d <- p3 + .Machine$double.eps
  }  else{
    stop("Something went wrong for Optd_l1")
  }
  return(d)
}