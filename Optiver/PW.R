PW <- function(w, x, y, z){
  if(w == x | w == y | w == z | x == y | x == z | y == z) {
    stop("Ties aren't allowed")
  }
  others <- c(x, y, z)
  i <- sort(others)
  if(w < i[1]){
    pwin <- (w - 0) + 0.5*(i[1] - w)
  } else if(w < i[2]){
    pwin <- 0.5*(i[2] - i[1])
  } else if(w < i[3]){
    pwin <- 0.5*(i[3] - i[2])
  } else {
    pwin <- (1 - w) + 0.5*(w - i[3])
  }
  return(pwin)
} # P(W = w wins|x, y, z)
