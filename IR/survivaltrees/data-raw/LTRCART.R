# LTRCART: operative function is rpart::rpart(...)

packages <- function(){
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(ir)
  library(LTRCtrees)
  library(survival)
  library(rpart.plot) 
  library(partykit)
  library(tidyr)
  }

# data(TK_AG)

# find_na <- function(x){sum(is.na(x))}
# sapply(TK_AG, find_na) # LOTS of NAs

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
         -temp_st,
         -max_months_in_field) %>%
  drop_na() # 44K left

x <- Sys.time()
LTRCART.fit <- LTRCART(
  Surv(time1, time2, event) ~ ., 
  data = TK_AG.2, 
  control = rpart::rpart.control(cp = 0.003) # smaller = more complex
  ) 
Sys.time() - x

rpart.plot.version1(LTRCART.fit, type = 0)


                          