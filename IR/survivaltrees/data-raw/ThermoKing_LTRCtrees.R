library(dplyr)
library(lubridate)
library(ggplot2)
library(ir) # MINE!!
library(LTRCtrees)
library(survival)
library(rpart.plot)
library(partykit)

# Load, format =====
# TK <- read.csv("~/Desktop/ODG/IngersolRand/NA_DTF_wPREDICTORS_3.csv")
# names(TK)

# devtools::use_data(TK_AG, pkg = "ir")

# TK <- TK %>%
#   mutate(mfg_date = ymd_hms(mfg_date),
#          serial = as.character(serial)) %>%
#   arrange(serial, months_in_field) %>%
#   filter(months_in_field > 0)

# Format to Anderson-Gill style ======

# Add 'first' and 'last' indicator columns to TK
# i.e. indicator first/last obs. of that serial #
for(i in 2:dim(TK)[1]){
  if(i == 2) first_serial <- "TRUE"
  first_serial[i] <- !(TK$serial[i] == TK$serial[i-1])
  if(i == dim(TK)[1]){
    TK$first_serial <- first_serial
    TK$last <- c(first_serial[-1], TRUE)
    rm(i, first_serial)
  }
}

# TK <- TK %>%
#   mutate(
#     time1 = ifelse(first_serial, 0, lag(months_in_field)),
#     time2 = months_in_field
#     )

# ir::TK_AG -- properly Anderson-Gill formatted data

# fit LTRC model ======
data(TK_AG)

fit.cox <- coxph(
  Surv(time1, time2, event) ~ max_return_z1 + min_return_z1,
  data = TK_AG)

# Fast One ===== #
LTRCART.fit <- LTRCART(
  Surv(time1, time2, event) ~
    max_return_z1 + min_return_z1 + max_ambient + min_ambient,
  data = TK_AG) # operative function--> rpart::rpart(...)

# Slow one ===== #
x <- Sys.time()
LTRCIT.fit <- LTRCIT(
  Surv(time1, time2, event) ~
    max_return_z1 + min_return_z1 + max_ambient + min_ambient,
  data = TK_AG) # # operative function--> partykit::ctree(...)
Sys.time() - x

fit.cox
rpart.plot.version1(LTRCART.fit, type = 0)
plot(LTRCIT.fit)

# type = “prob”: list of predicted KM curves
# KM_curves <- predict(LTRCIT.fit, newdata = , type = "prob")

# type = "response": predicted median survival time
# predict(LTRCART.fit, newdata = , type = "response")
