source("Optd_l1.R")
source("Optd_l2.R")
source("Optd_l3.R")
source("Optd_l4.R")

Optd <- function(a, b, c){
  if(a == b | a == c | b == c){
    stop("No shared choices allowed; e.g. a = b")
  }
  
  X <- data.frame(Int = c("I1", "I2", "I3", "I4"), PW = 0) 
  
  x <- sort(c(a, b, c)) 
  p1 <- x[1] 
  p2 <- x[2]
  p3 <- x[3]
  
  # P(Winning|Interval-optimal)
  X[1, 2] <- p1 - 0
  X[2, 2] <- 0.5*(p2 - p1) # avg optimal
  X[3, 2] <- 0.5*(p3 - p2) # avg optimal
  X[4, 2] <- 1 - p3
  
  winner <- X %>% filter(PW == max(PW)) %>% .$Int
  l = length(winner)
  
  if(l == 1){
    d <- Optd_l1(winner, p1, p2, p3)
    
  } else if(l == 2){
    d <- Optd_l2(winner, p1, p2, p3)
  
  } else if(l == 3){
    d <- Optd_l3(winner, p1, p2, p3)

  } else if(l == 4){
    d <- Optd_l4(winner, p1, p2, p3)
    
  } else{
    Stop("Optd: there should be 1, 2, 3, or 4 best 'intervals'... but there's not???")
  }
  
  return(d)
}  # optimal d 
