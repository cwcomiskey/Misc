# rpart::rpart()

# method ...method can be a list of functions named init, split and eval. Examples are given in the file ‘tests/usersplits.R’ in the sources, and in the vignettes ‘User Written Split Functions’.

library(dplyr)
library(lubridate)
library(ggplot2)
library(ir)
library(LTRCtrees)
library(survival)
library(rpart.plot) 
library(partykit)
library(tidyr)

data(TK_AG)

# LTRCART: operative function is rpart::rpart(...)

# Punt on NA problem
TK_AG.2 <- TK_AG %>%
  select(-electric.starts.per.electric.hour,
         -tkis_model,
         -mfg_date,
         -months_in_field,
         -serial,
         -first_serial,
         -last,
         -censor,
         -temp_st) %>%
  drop_na() # 44K left

x <- Sys.time()
LTRCART.fit2 <- LTRCART(
  Surv(time1, time2, event) ~ ., 
  data = TK_AG.2, 
  control = rpart::rpart.control(cp = 0.005)
  ) 
Sys.time() - x

# rpart.plot.version1(LTRCART.fit, type = 0)
rpart.plot.version1(LTRCART.fit2, type = 0)

find_na <- function(x){ sum(is.na(x))}
sapply(TK_AG, find_na) # LOTS of NAs

                          