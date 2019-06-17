packages <- function(){
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(ir)
  library(LTRCtrees)
  library(survival)
  library(rpart.plot) 
  library(partykit)
  library(tidyverse)
  }

# data(TK_AG)

# LTRCIT: operative function --> partykit::ctree(...)
#  ---> this is the SLOWER ONE

TK_AG.2 <- TK_AG %>%
  select(-electric.starts.per.electric.hour,
         -tkis_model,
         -mfg_date,
         -months_in_field,
         -serial,
         -first_serial,
         -last,
         -censor,
         -temp_st,
         -max_months_in_field) %>%
  drop_na() # 44K left

x <- Sys.time()
LTRCIT.fit <- LTRCIT(
  Surv(time1, time2, event) ~ 
    max_return_z1 + min_return_z1 + 
    max_ambient + min_ambient +
    max_discharge_z1 + min_discharge_z1 + 
    max_engine_temp + avg_engine_temp, 
  Control = partykit::ctree_control(alpha = 0.001, maxdepth = 3),
  data = TK_AG) # 
y <- Sys.time(); y - x 
# 4 covs, a = 0.001, maxdepth = 3 ---> 1.633964 mins
# 6 covs, 1.6 mins
# 7 covs, 1.6 mins
# 8 covs, fail -- NOT HAPPENING:
# Error in .LinStatExpCov1d(X = X, Y = Y, weights = weights, subset = subset,  : 
#   'Calloc' could not allocate memory (18446744072864686080 of 8 bytes)



plot(LTRCIT.fit)
