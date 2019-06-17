# Geo Research - Traffic Generation and Analytics

# geos: non-overlapping geographic regions, that are randomly, or systematically, assigned to a control or treatment condition.

# 'Control + G' to jump to row in Excel

library(devtools)
# install_github("google/GeoexperimentsResearch")
library(GeoexperimentsResearch)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)

# Netherlands - rehearsal ========== ========== ========== ========== ==========

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

test = summary(tbr_cost, level = 0.95, interval.type = "two-sided")
plot(tbr_cost) 

# mu = -3810; st_dev = 1429.594
mu = test[1, 1]; st_dev = test[1, 5]
x <- seq(mu - 4*st_dev, mu + 4*st_dev, length=100)
hx <- dnorm(x, mean = mu, sd = st_dev)
ggplot2::ggplot() + geom_line(aes(x = x, y = hx))

# "We refer to this confidence interval half-width by precision"
# --> so this changes dep on %, whereas SE is SE

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

# NL (Jan1, March5), (March29, May19) wrangling =============== 
nlgeo <- read_excel("NL geo test and pretest period.xlsx", 
  col_types = c("text", "text", "text", "text", "text", 
                "text", "skip", "skip", "text", "text", 
                "text", "text", "text")
  ) 

nl1 <- nlgeo[-1, 1:6] %>% setNames(c('Phase', 'Month', 'Day', 'Cost', 'Revenue', 'ROI'))
nl2 <- nlgeo[-1,c(1:2, 8:11)] %>% setNames(c('Phase', 'Month', 'Day', 'Cost', 'Revenue', 'ROI'))
nlgeo = rbind(nl1, nl2); rm(nl1, nl2)

nlgeo2 <- nlgeo %>% 
  tidyr::fill(Phase, Month) %>%
    mutate(geo.group = c(rep(1, 139), rep(2, 139)), # Control = 1, Trt = 2,
           geo = c(rep(1, 139), rep(2, 139)),
           date = as.Date(str_c(Day, Month, '2019'),  format = '%d%B%Y'),
           Cost = round(as.numeric(Cost), 2),
           Revenue = round(as.numeric(Revenue), 2),
           ROI = round(as.numeric(ROI), 2)
           ) %>%
  select(-Month, -Day) %>%
  filter(!(date > as.Date('2019-03-04') & date < as.Date('2019-03-29')))

# write.csv(nlgeo2, 'nl_geo.csv')



geo_ts <- GeoTimeseries(nlgeo2, metrics = c('Cost', 'Revenue'))

phases <- ExperimentPeriods(c("2019-01-01", "2019-03-06", "2019-05-19"))

geo_assign <- nlgeo2 %>%
  select(geo, geo.group) %>%
  distinct() %>%
  GeoAssignment()

geo_obj <- GeoExperimentData(geo.timeseries = geo_ts, 
                             periods = phases, 
                             geo.assignment = geo_assign)

aggregate(geo_obj, by=c('geo.group', 'period'))

# NL Cost TBR ====== 
tbr_cost <- DoTBRAnalysis(geo_obj, 
                          response = 'Cost',
                          model = 'tbr1',
                          pretest.period = 0,
                          intervention.period = 1,
                          control.group = 1,
                          treatment.group = 2)

summary(tbr_cost, level = 0.95, interval.type = "two-sided")
#             estimate precision    lower    upper       se level thres prob model
# incremental 5770.485  1562.892 4207.593 7333.378 781.5933  0.95     0    1  tbr1
plot(tbr_cost) 

# NL revenue TBR ========== 
tbr_rev <- DoTBRAnalysis(geo_obj, 
                         response = 'Revenue',
                         model = 'tbr1',
                         pretest.period = 0,
                         intervention.period = 1,
                         control.group = 1,
                         treatment.group = 2)

summary(tbr_rev, level = 0.95, interval.type = "two-sided")
#             estimate precision    lower    upper       se level thres  prob model
# incremental 49470.51  31292.24 18178.27 80762.75 15649.07  0.95     0 0.999  tbr1
plot(tbr_rev) 

#' "The three panels show 
#' (a) the observed treatment time series yt along with its estimated counterfactual yt∗; 
#' (b) the pointwise (daily) causal effects φt, and 
#' (c) the cumulative causal effect ∆(t)."

# NL iROAS TBR ========== 
tbr_roas <- DoTBRROASAnalysis(geo_obj, 
                              response='Revenue', 
                              cost='Cost',
                              model='tbr1',
                              pretest.period=0,
                              intervention.period=1,
                              control.group=1,
                              treatment.group=2)

summary(tbr_roas, level=0.95, interval.type="two-sided")
#       estimate precision    lower    upper level incr.resp incr.cost thres  prob model
# iROAS 8.566556  6.203253 3.027721 15.43423  0.95  49470.51  5770.485     0 0.999  tbr1

# The CI is not symmetric b/c the quantities estimates form the ratio; e.g.
# Consider intervals: 10 with (5, 15) and 15 with (10, 20) -- > 2/3 and (1/2, 3/4) --- notice 2/3 is not in the middle of this interval

plot(tbr_roas) 

# Recall: linear regression gives expected value of response, and associated variance of THAT; then there is additional uncertainty associated with a response; i.e. variance of estimate of mean, and then epsilon

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
