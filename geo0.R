# Geo Research - Traffic Generation and Analytics

# geos: non-overlapping geographic regions, that are randomly, or systematically, assigned to a control or treatment condition.

# 'Control + G' to jump to row in Excel

library(devtools)
# install_github("google/GeoexperimentsResearch")
library(GeoexperimentsResearch)
library(tidyr)
library(dplyr)

# Netherlands ========== ========== ========== ========== ==========

dat0 <- read_excel("Geosplit NL Paid Search Tactics NEW.xlsx", sheet = "GMA Geo Split NL", 
             col_types = c("skip", "skip", "date", "date", "skip", "skip", "skip", "skip",
                           "skip", "skip", "skip", "text", "numeric",  "numeric", "numeric",
                           "numeric", "numeric", "text"), range = cell_rows(1:194332)) 

dat <- dat0 %>%
  rename(date = To, geo = `City ID`, geo.group = `Geo Group`) %>%
  drop_na() %>%
  filter(geo.group != 'excluded') %>%
  mutate(date = as.Date(date), geo.group = ifelse(geo.group == 'control', 1, 2)) %>%
  distinct(geo, date, .keep_all = TRUE); rm(dat0)

geo_ts <- GeoTimeseries(dat, metrics = c('Cost', 'Revenue'))

phases <- ExperimentPeriods(c("2019-03-06", "2019-05-10", "2019-05-15"))

geo_assign <- dat %>%
  select(geo, geo.group) %>%
  distinct() %>%
  GeoAssignment()
  
geo_obj <- GeoExperimentData(geo.timeseries = geo_ts, periods = phases, geo.assignment = geo_assign)

aggregate(geo_obj, by=c('geo.group', 'period'))

# Time-based Regression - cost ========== ========== #
tbr_cost <- DoTBRAnalysis(geo_obj, 
                         response = 'Cost',
                         model = 'tbr1',
                         pretest.period = 0,
                         intervention.period = 1,
                         control.group = 1,
                         treatment.group = 2)

summary(tbr_cost, level = 0.95, interval.type = "two-sided")
plot(tbr_cost) 

m = -3810; st_dev = 1429.594
x <- seq(m - 4*st_dev, m + 4*st_dev, length=100)
hx <- dnorm(x, mean = m, sd = st_dev)
ggplot2::ggplot() + geom_line(aes(x = x, y = hx))

# "We refer to this confidence interval half-width by precision"

# Time-based Regression - revenue ========== ========== #
tbr_rev <- DoTBRAnalysis(geo_obj, 
                          response = 'Revenue',
                          model = 'tbr1',
                          pretest.period = 0,
                          intervention.period = 1,
                          control.group = 1,
                          treatment.group = 2)

summary(tbr_rev, level = 0.95, interval.type = "two-sided")
plot(tbr_rev) 

# Time-based Regression - Return On Ad Spend ========== ========== #
# ROAS == iROAS
tbr_roas <- DoTBRROASAnalysis(geo_obj, 
                              response='Revenue', 
                              cost='Cost',
                              model='tbr1',
                              pretest.period=0,
                              intervention.period=1,
                              control.group=1,
                              treatment.group=2)

summary(tbr_roas, level=0.95, interval.type="two-sided")
plot(tbr_roas) 

# Spain  ========== ========== ========== ========== 
spain0 <- read_excel(
  "Geosplit ES PLA.xlsx", sheet = "GMA Split Spain",
  col_types = c("skip", "skip", "date", "date", "skip", "skip", "skip", 
                "skip", "skip", "skip", "skip", "text", "numeric", "numeric", 
                "numeric", "numeric", "numeric", "text"))

dim(drop_na(spain0)) # 1167 -- all `Geo Group` column

spain <- spain0 %>% 
  rename(date = To, geo = `City ID`, geo.group = `Geo Group`) %>%
  drop_na() %>%
  mutate(date = as.Date(date), geo.group = ifelse(geo.group == 'control', 1, 2))
  
spain_ts <- GeoTimeseries(spain, metrics = c('Cost', 'Revenue'))

spain_phases <- ExperimentPeriods(c("2019-03-06", "2019-05-10", "2019-05-15"))

spain_assign <- spain %>%
  select(geo, geo.group) %>%
  distinct() %>%
  GeoAssignment()

spain_obj <- GeoExperimentData(geo.timeseries = spain_ts, 
                             periods = spain_phases, 
                             geo.assignment = spain_assign)

aggregate(spain_obj, by=c('geo.group', 'period'))

# Time-Based Regression - revenue  ========== ========== #
spain_rev <- DoTBRAnalysis(spain_obj, 
                         response = 'Revenue',
                         model = 'tbr1',
                         pretest.period = 0,
                         intervention.period = 1,
                         control.group = 1,
                         treatment.group = 2)

summary(spain_rev, level = 0.95) # , interval.type = "two-sided")
plot(spain_rev) 

# Time-Based Regression - Return On Ad Spend  ========== ========== #
spain_roas <- DoTBRROASAnalysis(spain_obj, 
                              response='Revenue', 
                              cost='Cost',
                              model='tbr1',
                              pretest.period=0,
                              intervention.period=1,
                              control.group=1,
                              treatment.group=2)

summary(spain_roas, level=0.95) #, interval.type="two-sided")
plot(spain_roas) 
