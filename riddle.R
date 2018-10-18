library(dplyr)

X <- data.frame(Player = c("A", "B", "C", "D"), choice = runif(4)) %>%
  arrange(choice)

# Prob function
PX <- function(df){
  df$p <- 0
  df[1,"p"] <- (df[1,"choice"] - 0) + 0.5*(df[2, "choice"] - df[1,"choice"])
  df[2,"p"] <- 0.5*(df[3, "choice"] - df[1, "choice"]) 
  df[3,"p"] <- 0.5*(df[4, "choice"] - df[2, "choice"])
  df[4,"p"] <- (1 - df[4, "choice"]) + 0.5*(df[4, "choice"] - df[3, "choice"])
  return(df)
} # P(X) column

# D's choice function
Optd <- function(a, b, c){
  X <- data.frame(Interval = c("1", "2", "3", "4"), Score = 0) 
  x <- sort(c(a, b, c))
  X["1", "Score"] <- x[1] - 0
  X["2", "Score"] <- 0.5*(x[2] - x[1])
  X["3", "Score"] <- 0.5*(x[3] - x[2])
  X["4", "Score"] <- 1 - x[3]
  winner <- filter(X, Score == max(Score))$Interval
  if(length(winner) != 1) stop("Tie!")
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

