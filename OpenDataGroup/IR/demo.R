library(ir)
library(LTRCtrees)
library(survival)
library(rpart)
library(rpart.plot) 
library(partykit)
  # Loading required package: grid
  # Loading required package: libcoin
  # Loading required package: mvtnorm

data(TK_AG)

LTRCIT.fit <- LTRCIT(
  Surv(time1, time2, event) ~ max_return_z1 + min_return_z1 + max_ambient + min_ambient, 
  data = TK_AG[1:51300,]
  ) 

plot(LTRCIT.fit)

LTRCIT.fit

# type = “prob”: list of predicted KM curves
LTRCIT.pred <- predict(LTRCIT.fit, newdata = TK_AG[51301:51327,], type = "prob")

# type = "response": predicted median survival time
predict(LTRCIT.fit, newdata = TK_AG[51301:51327,], type = "response")

# See: 
# (1) https://arxiv.org/pdf/1606.03033.pdf for details of modeling technique
# (2) https://cran.r-project.org/web/packages/LTRCtrees/vignettes/LTRCtrees.html for implementation details
