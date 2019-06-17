# LTRCtrees vignette

# Example 1 ========
data(package = "survival")
?flchain

library(survival)
set.seed(0)

DATA <- flchain %>%  
  filter(!is.na(creatinine)) %>%
  mutate(End = age + futime/365) %>%
  filter(End > age) 

names(DATA)[6] <- "FLC"

Train = DATA[1:500,]
Test = DATA[1000:1020,]

library(LTRCtrees) 
LTRCART.obj <- LTRCART(Surv(age, End, death) ~ sex + FLC + creatinine, Train)
LTRCIT.obj <- LTRCIT(Surv(age, End, death) ~ sex + FLC + creatinine, Train)

library(rpart.plot) 
rpart.plot.version1(LTRCART.obj)
plot(LTRCIT.obj)

library(partykit)
LTRCART.obj.party <- as.party(LTRCART.obj)
LTRCART.obj.party$fitted[["(response)"]] <- Surv(Train$age, Train$End, Train$death)
plot(LTRCART.obj.party)

LTRCIT.pred <- predict(LTRCIT.obj, newdata=Test, type = "response")
head(LTRCIT.pred)

LTRCIT.pred <- predict(LTRCIT.obj, newdata=Test, type = "prob")
head(LTRCIT.pred,2)

# Example 2: time-varying covariates =======
set.seed(0)
library(survival)

## Create the start-stop-event triplet needed for coxph and LTRC trees
first <- with(pbcseq, c(TRUE, diff(id) != 0)) # first id for each subject
last <- c(first[-1], TRUE) # last id

pbcseq <- pbcseq %>%
  mutate(
    time1 = ifelse(first, 0, day),
    time2 = ifelse(last, futime, c(day[-1], 0)),
    event = as.numeric(ifelse(last, status, 0) == 2)
  ); rm(first, last)

## Fit the Cox model and LTRC trees with time-varying covariates
fit.cox <- coxph(Surv(time1, time2, event) ~ age + sex + log(bili), pbcseq)
LTRCIT.fit <- LTRCIT(Surv(time1, time2, event) ~ age + sex + log(bili), pbcseq)
LTRCART.fit <- LTRCART(Surv(time1, time2, event) ~ age + sex + log(bili), pbcseq)

## Result of the Cox model with time-varying covariates
fit.cox

## plots of fitted survival trees with time-varying covariates
rpart.plot.version1(LTRCART.fit,type=0)
plot(LTRCIT.fit)                       
