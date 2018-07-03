# ELIs: walmart API loop

# Automate =====
macro::depends() # load packages
library(scrapr) # data

# dat = package data
# datr = environment data


# Reading and writing ======

# (1) load
walmart_dat <- data.table::fread("walmart_dat.csv") %>%
  mutate("today()" = lubridate::ymd(`today()`))

# (2) collect
datr <- scrapr::collect_byID()

# (3) combine
walmart_dat <- rbind.data.frame(walmart_dat, datr)

# (4) re-save
data.table::fwrite(walmart_dat, "walmart_dat.csv")
