library(dplyr)

container <- data.frame(a = NA, b = NA, c = NA, d = NA, 
                PA = NA, PB = NA, PC = NA, PD = NA)

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

Optd <- function(a, b, c){
  X <- data.frame(Interval = c("1", "2", "3", "4"), Score = 0) 
  x <- sort(c(a, b, c))
  X["1", "Score"] <- x[1] - 0
  X["2", "Score"] <- 0.5*(x[2] - x[1])
  X["3", "Score"] <- 0.5*(x[3] - x[2])
  X["4", "Score"] <- 1 - x[3]
  winner <- filter(X, Score == max(Score))$Interval
  
  if(length(winner) == 1){
    if(winner == "4"){
      d <- x[3] + .Machine$double.eps
    } else if(winner == "3"){
      d <- 0.5*(x[2] + x[3])
    } else if(winner == "2"){
      d <- 0.5*(x[1] + x[2])
    } else{
        d <- x[1] - .Machine$double.eps
    }
    return(d)
    }
  if(length(winner) > 1){
    l <- length(winner)
    d <- 0
    for(index in 1:l){
        if(winner[index] == "4"){
          d[index] <- x[3] + .Machine$double.eps
        } else if(winner[index] == "3"){
          d[index] <- 0.5*(x[2] + x[3])
        } else if(winner[index] == "2"){
          d[index] <- 0.5*(x[1] + x[2])
        } else{
          d[index] <- x[1] - .Machine$double.eps
        }
    }
    return(d)
  }
    } # optimal d(s) given a, b, c

l <- 100 # length
I05 <- seq(0, 0.5, length = l) # [0, ... , 0.5]
I01 <- seq(0, 1, length = l)   # [0, ... , 1]

for(ai in I05){
  print(ai)
  
  for(bj in I01){
    if(bj == ai) next
    
    for(ck in I01){
      if(ck == ai | ck == bj) next
      
      d_opt <- Optd(a = ai, b = bj, c = ck)
      if(length(d_opt) == 1){
        PA <- PW(w = ai, bj, ck, d_opt)
        PB <- PW(w = bj, ai, ck, d_opt)
        PC <- PW(w = ck, ai, bj, d_opt)
        PD <- PW(w = d_opt, ai, bj, ck)
        
        result_ijk <- data.frame(a = ai, b = bj, c = ck, d = d_opt,
                                 PA = PA, PB = PB, PC = PC, PD = PD)
        container <- rbind.data.frame(container, result_ijk)
      } else if(length(d_opt) > 1){
        len <- length(d_opt)
        for(index in 1:len){
          PA <- PW(w = ai, bj, ck, d_opt[index])
          PB <- PW(w = bj, ai, ck, d_opt[index])
          PC <- PW(w = ck, ai, bj, d_opt[index])
          PD <- PW(w = d_opt[index], ai, bj, ck)
          
          tie_ijk <- data.frame(a = ai, b = bj, c = ck, d = d_opt[index],
                                   PA = PA, PB = PB, PC = PC, PD = PD)
          container <- rbind.data.frame(container, tie_ijk)
        }
      }

    }
  }
}
