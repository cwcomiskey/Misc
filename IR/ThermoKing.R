  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(ir)

  
# TK <- read.csv("~/Desktop/ODG/IngersolRand/NA_DTF_wPREDICTORS_3.csv")
# names(TK)

# devtools::use_data(tk7, pkg = "ir")

data("TK")
TK <- TK %>% mutate(mfg_date = ymd_hms(mfg_date),
                    serial = as.character(serial)) %>% 
  arrange(serial, months_in_field) %>%
  filter(months_in_field > 0)

# Anderson-Gill style ======

# Add 'first' and 'last' columns to tk7
# i.e. logical first/last obs. of that serial #
for(i in 2:dim(TK)[1]){
  if(i == 2) first_serial <- "TRUE"
  first_serial[i] <- !(TK$serial[i] == TK$serial[i-1]) 
  if(i == dim(TK)[1]){
    TK$first_serial <- first_serial
    TK$last <- c(first_serial[-1], TRUE)
    rm(i, first_serial)
  }
} 

TK <- TK %>%
  mutate(
    time1 = ifelse(first_serial, 0, lag(months_in_field)),
    time2 = months_in_field
    )

fit.cox <- coxph(Surv(time1, time2, event) ~ max_return_z1 + min_return_z1 + 
                   max_return_z1 + min_return_z1, data = TK)
LTRCART.fit <- LTRCIT(Surv(time1, time2, event) ~ max_return_z1 + min_return_z1 + 
                        max_return_z1 + min_return_z1, data = TK)
LTRCIT.fit <- LTRCART(Surv(time1, time2, event) ~ max_return_z1 + min_return_z1 + 
                        max_return_z1 + min_return_z1, data = TK)

fit.cox
rpart.plot.version1(LTRCART.fit, type = 0)
