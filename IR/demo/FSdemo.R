# fastscore.schema.0: input-schema
# fastscore.schema.1: output-schema

begin <- function(){
  
  library(LTRCtrees)
  library(survival)
  library(rpart)
  library(rpart.plot) 
  library(partykit)
    # Loading required package: grid
    # Loading required package: libcoin
    # Loading required package: mvtnorm
  
  LTRCIT.fit <- load("demo/LTRCIT.fit.rda", envir = .GlobalEnv)  # model

}

action <- function(dat){t
  
  # predicted median survival time
  medians <- predict(LTRCIT.fit, newdata = dat, type = "response")
  
  emit(medians)
  
}
