# Shane's code

#' R script:
#'     - fit "best" ARIMA models to each EC (or Strata)  
#'     - Use to forecast next X months
#'     We still need
#'          - the weights to calculate roll-ups 
#'          - to see which categories lead to the biggest gaps in prediction

package_loader <- function(){
  library(lattice)
  library(tidyverse)
  library(lubridate)
  library(reshape2)
  library(magrittr)
  library(Rcpp)
  library(RcppRoll)
  library(rmarkdown)
  library(knitr)
  library(betareg)
  library(stringr)
  library(padr)
  library(zoo)
  library(mgcv)
  library(blscrapeR)
  library(xts)
  library(forecast)
  library(seasonal)
  library(seasonalview)
  library(fpp2)
  options(tibble.print_max = 200)
}; package_loader(); rm(package_loader)

# First read in data ======
# Read in cu.series descriptor to get filenames
seriesList <- read.table("cu.series.txt", header = T, 
                         sep = "\t", stringsAsFactors = F, quote = NULL)

itemStrata <- seriesList[390:631, ] %>% filter(nchar(item_code) != 5) 
itemStrataSub <- itemStrata[nchar(itemStrata$item_code) != 4, ]
# itemStrataSub has the series names/id's for the 170 item strata that are distinct from the expenditure class headings 

expenditureClasses <- itemStrata[nchar(itemStrata$item_code) == 4, ]
# expenditureClasses has the series names for all 70 expenditure classes 
# 15 of the expenditure classes can be re-used for the itemStrata series

# Now load data using series ID's from expenditureClasses and itemStrataSub
cuFiles <- system("ls cu*US*.txt", intern = T)

# read in cuFiles, extract expenditureClasses and itemStrata ======
iStrata <- list()
exClasses <- list()

for(i in 1:8){
  temp <- read.table(cuFiles[i], header = T, stringsAsFactors = F, sep = "\t")
  temp %>% filter(series_id %in% itemStrataSub$series_id) %>% 
    filter(period != "M13") %>%
    filter(year > 1969) %>% 
    mutate(DATE = as_date(paste(year, substr(period,2,3), "01", sep = "-"))) -> iStrata[[i]]
  temp %>% filter(series_id %in% expenditureClasses$series_id) %>%
    filter(period != "M13") %>%
    filter(year > 1969) %>% 
    mutate(DATE = as_date(paste(year, substr(period,2,3), "01", sep = "-")))-> exClasses[[i]]
}; rm(i)
iStrata <- do.call("rbind", iStrata)
exClasses <- do.call("rbind", exClasses)

iStrata <- iStrata %>% mutate(series = substr(series_id, 11, 14)) 
exClasses <- exClasses %>% mutate(series = substr(series_id, 11, 12)) 

itemStrataNames <- substr(itemStrataSub$item_code, 3, 6)
expenditureClassesNames <- substr(expenditureClasses$item_code, 3, 4)

# Plots =======
# time series: expenditure classes
exClasses %>% filter(as_date(DATE) > "1996-12-31") -> subEx
par(mfrow = c(3,4))
for(i in 1:70){
  tx <- subEx[subEx$series == expenditureClassesNames[i],]
  xx <- ts(tx$value, start = c(first(tx$year), first(as.numeric(substr(tx$period,2,3)))), 
           end = c(last(tx$year), last(as.numeric(substr(tx$period,2,3)))), frequency = 12)
  plot(xx, xlab = "time", ylab = "", main = expenditureClassesNames[i])
}

# time series: item strata
iStrata %>% filter(as_date(DATE) > "1996-12-31") -> subI
par(mfrow = c(3,5))
for(i in 1:169){
  tx <- subI[subI$series == itemStrataNames[i],]
  xx <- ts(tx$value, start = c(first(tx$year), first(as.numeric(substr(tx$period,2,3)))), 
           end = c(last(tx$year), last(as.numeric(substr(tx$period,2,3)))), frequency = 12)
  plot(xx, xlab = "time", ylab = "", main = itemStrataNames[i])
}

# check data completeness ========
exlis <- list()
for(i in 1:70){
  tx <- subEx[subEx$series == expenditureClassesNames[i],]
  xx <- ts(tx$value, start = c(first(tx$year), first(as.numeric(substr(tx$period,2,3)))), 
           end = c(last(tx$year), last(as.numeric(substr(tx$period,2,3)))), frequency = 12) 
  temp <- sum(diff(t(table(tx$year, tx$period)))^2)
  exlis[[i]] <- temp
}

ilis <- list()
for(i in 1:169){
  tx <- subI[subI$series == itemStrataNames[i],]
  xx <- ts(tx$value, start = c(first(tx$year), first(as.numeric(substr(tx$period,2,3)))), 
           end = c(last(tx$year), last(as.numeric(substr(tx$period,2,3)))), frequency = 12)
  temp <- sum(diff(t(table(tx$year, tx$period)))^2)
}

# SO THERE ARE 3 ITEM STRATA (HP01,HP02, HP04) with missing data 
# will deal with that later 

# Also there are 15 item strata that don't appear in the 169 iStrata listed -- these have a single
# (or double, if there's an unsampled item strata) item strata and so the expenditure class value is 
# same.  Later we will need to fill these in. 

# auto-forecast 1-month forward for EC and IS ==========
#   use auto.arima to fit model
#   use log transform 
#   can also create hierarchy of estimates 
#     a.  naive (last value) 
#     b.  mean over last year 
#     c.  last value 1 year ago (seasonal naive)
#     d.  linear trend over last year 
#     e.  ewma model 
#     f.  arima model (that's what next function is)

# function to automatically calculate best ARIMA model for each component =======
arimaModCPI <- function(xx, 
                        xName = expenditureClassesNames[1], 
                        startTrain = "1997-01-01",          # START DATE
                        endTrain = "2016-12-31") {          # END DATE
  
  xx <- xx %>% filter(DATE >= startTrain) 
  
  xstart <- c(first(year(xx$DATE)), first(month(xx$DATE)))  
  endTrain.ts <- c(year(endTrain), month(endTrain))         
  xend <- c(last(year(xx$DATE)), last(month(xx$DATE)))
  
  xxt <- ts(xx$value, start= xstart, end = endTrain.ts, freq = 12) # TRAIN data
  xxs <- ts(xx$value, start = xstart, end = xend, freq = 12) # TRAIN & FORECAST data
  
  lxxt <- log(xxt)
  lxxs <- log(xxs)
  
  flength <- length(lxxs) - length(lxxt) # Forecast length
  
  lxxt.mod <- auto.arima(lxxt) # **** 
  lxxt.fcst <- forecast(lxxt.mod, h = flength)$mean # **** 
  
  lxxs.mod <- Arima(lxxs, model = lxxt.mod) # **** model = lxxt.mod 
  lxxs.fcst <- lxxs.mod$fitted[(length(lxxt) + 1) : length(lxxs)] # ****
  
  actual <- exp(lxxs)
  
  fcst1 <- ts(exp(c(lxxt, lxxt.fcst)), start = xstart, end = xend, frequency = 12)
  fcst2 <- ts(exp(c(lxxt, lxxs.fcst)), start = xstart, end = xend, frequency = 12)
  
  return(list(seriesName = xName, model = lxxt.mod, 
              actual = actual, fcst1 = fcst1, 
              fcst2 = fcst2)
  )
}

exMod <- list()
for(i in 1:70){
  if(i %% 5 == 0) print(i)
  xxx <- exClasses %>% filter(series == expenditureClassesNames[i])
  exMod[[i]] <- arimaModCPI(xxx, xName = expenditureClassesNames[i], 
                            endTrain = "2017-12-31")
}
exMod.arma <- lapply(exMod, function(x)x$model$arma) # AR, MA, seas, diff, etc. 
names(exMod) <- names(exMod.arma) <- expenditureClassesNames

# Can add options for log transform or not
# May want to force first differences since that is how data is generated 

# Now need to fill in *missings* (dates) in 4 item strata =======
iStrata <- iStrata %>% group_by(series) %>% 
  pad("month", by = "DATE") 
# exClasses is ok

# 5 series (really only 4 since AC01 is in 1977) 
# now have NA's for some values 

# Now do same estimates for iStrata =======
isMod <- list()
for(i in 1:169){
  print(i)
  xxx <- iStrata %>% filter(series == itemStrataNames[i]) 
  isMod[[i]] <- arimaModCPI(xxx, xName = itemStrataNames[i], 
                            endTrain = "2017-12-31")
}
isMod.arma <- lapply(isMod, function(x)x$model$arma)
names(isMod) <- names(isMod.arma) <- itemStrataNames

# Now get simple forecasting estimates =======
simpleTSCPI <- function(x, endHistory = "2016-12-31") {
  xx <- x$value
  lagxx <- lag(xx, 1)
  xmean <- rollapply(xx, 12, "mean", align = "right", partial = T, fill = NA)
  lagxxs <- lag(xx, 12)
  xtrend <- lagxx + (lagxx - lagxxs)/12
  return(list(actual = xx[x$DATE > endHistory], 
              fnaive = lagxx[x$DATE > endHistory], 
              fmean12 = xmean[x$DATE > endHistory], 
              fnaiveS = lagxxs[x$DATE > endHistory], 
              ftrend12 = xtrend[x$DATE > endHistory]))
}

exSim <- list()
for(i in 1:70){
  xi <- exClasses %>% filter(series == expenditureClassesNames[i])
  exSim[[i]] <- simpleTSCPI(xi, "2017-12-31")
}
names(exSim) <- expenditureClassesNames

isSim <- list()
for(i in 1:169){
  xi <- iStrata %>% filter(series == itemStrataNames[i]) 
  isSim[[i]] <- simpleTSCPI(xi, "2017-12-31")
}
names(isSim) <- itemStrataNames

# Now append the arima forecasts to these lists ======
for(i in 1:70){
  nx <- length(exMod[[i]]$fcst2)
  exSim[[i]]$farima <- exMod[[i]]$fcst2[(nx - 4): nx]
}

for(i in 1:169) {
  nx <- length(isMod[[i]]$fcst2)
  isSim[[i]]$farima <- isMod[[i]]$fcst2[(nx - 4): nx]
}

# Now summarize results =======
mse <- function(x, y){mean((x-y)^2, na.rm = T)}
mae <- function(x, y){mean(abs(x-y), na.rm = T)}

compFcsts <- function(x){
  naiveErr <- x$actual - x$fnaive
  meanErr <- x$actual - x$fmean12
  naiveSErr <- x$actual - x$fnaiveS
  trendErr <- x$actual - x$ftrend12
  arimaErr <- x$actual - x$farima
  naiveMSE <- mean(naiveErr^2)
  naiveMAE <- mean(abs(naiveErr))
  meanMSE <- mean(meanErr^2)
  meanMAE <- mean(abs(meanErr))
  naiveSMSE <- mean(naiveSErr^2)
  naiveSMAE <- mean(abs(naiveSErr))
  trendMSE <- mean(trendErr^2)
  trendMAE <- mean(abs(trendErr))
  arimaMSE <- mean(arimaErr^2)
  arimaMAE <- mean(abs(arimaErr))
  return(list(
    monthlyErr = matrix(c(naiveErr, meanErr, naiveSErr, trendErr, arimaErr), nr = 5),  
    MSE = c(naiveMSE, meanMSE, naiveSMSE, trendMSE, arimaMSE), 
    MAE = c(naiveMAE, meanMAE, naiveSMAE, trendMAE, arimaMAE)))
}

exRes <- lapply(exSim, compFcsts)
isRes <- lapply(isSim, compFcsts)
lapply(exRes, function(x)rank(x$MSE)) %>% do.call("rbind", .) -> exMSErank
lapply(exRes, function(x)rank(x$MAE)) %>% do.call("rbind", .) -> exMAErank

lapply(isRes, function(x)rank(x$MSE)) %>% do.call("rbind", .) -> isMSErank
lapply(isRes, function(x)rank(x$MAE)) %>% do.call("rbind", .) -> isMAErank

# Arima does best but not uniformly
# Next need to add relative importance weights to 
#   - measure effect on CPI
#   - see if itemStrata-level prediction improves on expenditure class prediction


# PART 2 -- model CPI w/ modeling above ===========

# January ======
rw301 <- scan("cpi-u-201801.csv", sep = ",", what = "character", quote = '\"') %>%
  data.frame(matrix(., nc = 44, byr = T), stringsAsFactors = F) %>%
  mutate(Label = X2 %>% gsub("\\(|\\)", "", .) %>% gsub("[0-9]+", "", .)) %>%
  filter(X3 != "")

exc <- exClasses %>% filter(DATE == "2018-01-01") 
  test1 <- match(exc$value, as.numeric(rw301$X16))
exc2 <- exClasses %>% filter(DATE == "2017-12-01") 

exc$weight <- as.numeric(rw301$X3[test1])/100
a2 <- lapply(exSim, function(x)c(x[[1]][1], x[[2]][1], 
                                 x[[3]][1], x[[4]][1], 
                                 x[[5]][1], x[[6]][1])) %>% 
  do.call("rbind", .) %>%
  data.frame(.)
names(a2) <- names(exSim[[1]])
a2$series <- names(exSim)

a4 <- data.frame(series = exc$series, DATE = exc$DATE, 
                 wgt = exc$weight, val0 = exc2$value, 
                 val1 = exc$value) %>% 
  merge(., a2, by = "series") %>%
  mutate(err = (actual - farima)/val0,
         wgterr = as.numeric(rw301$X15[1])* 
           (wgt * (actual - farima) / val0) / sum(wgt)
         )

rx15 <- as.numeric(rw301$X15[1])
res_201801 <- with(a4, 
                   c(rx15 * sum(wgt * actual/val0)/sum(wgt),
                     rx15 * sum(wgt * fnaive/val0)/sum(wgt),
                     rx15 * sum(wgt * fmean12/val0)/sum(wgt),
                     rx15 * sum(wgt * fnaiveS/val0)/sum(wgt),
                     rx15 * sum(wgt * ftrend12/val0)/sum(wgt),
                     rx15 * sum(wgt * farima/val0)/sum(wgt)
                     )
                   )

res_201801 - res_201801[1]
res_201801

# february ========
rw <- scan("cpi-u-201802.csv", sep = ",", what = "character", quote = '\"')
rw2 <- data.frame(matrix(rw, nc = 44, byr = T), stringsAsFactors = F)
rw2Lab <- rw2$X2
rw2Lab <- gsub("\\(|\\)", "", rw2Lab)
rw2Lab <- gsub("[0-9]+", "", rw2Lab)
rw2$Label <- rw2Lab
rw2 %>% filter(rw2$X3 != "") -> rw3

rw202 <- rw2
rw302 <- rw3

exClasses %>% filter(DATE == "2018-02-01") -> exc
test1 <- match(exc$value, as.numeric(rw302$X16))
# doublecheck by doing a second month
exClasses %>% filter(DATE == "2018-01-01") -> exc2
test2 <- match(exc2$value, as.numeric(rw302$X15)) 
test1 - test2

exc$weight <- as.numeric(rw302$X3[test1])/100
a1 <- names(exSim)
a2 <- lapply(exSim, function(x)c(x[[1]][2], x[[2]][2], x[[3]][2], x[[4]][2], x[[5]][2], x[[6]][2]))
a2 <- do.call("rbind", a2)
a2 <- data.frame(a2)
names(a2) <- names(exSim[[1]])
a2$series <- a1

# now merge with file of actuals and weights (exc)
a3 <- data.frame(series = exc$series, DATE = exc$DATE, wgt = exc$weight, 
                 val0 = exc2$value, val1 = exc$value)

a4 <- merge(a3, a2, by = "series")
a5 <- data.frame(series = a4$series, err = as.numeric(rw302$X15[1])*(a4$wgt * (a4$actual - a4$farima)/a4$val0)/sum(a4$wgt))

a4$err <- (a4$actual - a4$farima)/a4$val0
a4$wgterr <- as.numeric(rw302$X15[1])*(a4$wgt * (a4$actual - a4$farima)/a4$val0)/sum(a4$wgt)
a4_201802 <- a4
res_201802 <- c(as.numeric(rw302$X15[1]) * sum(a4$wgt * a4$actual/a4$val0)/sum(a4$wgt),
                as.numeric(rw302$X15[1]) * sum(a4$wgt * a4$fnaive/a4$val0)/sum(a4$wgt),
                as.numeric(rw302$X15[1]) * sum(a4$wgt * a4$fmean12/a4$val0)/sum(a4$wgt),
                as.numeric(rw302$X15[1]) * sum(a4$wgt * a4$fnaiveS/a4$val0)/sum(a4$wgt),
                as.numeric(rw302$X15[1]) * sum(a4$wgt * a4$ftrend12/a4$val0)/sum(a4$wgt),
                as.numeric(rw302$X15[1]) * sum(a4$wgt * a4$farima/a4$val0)/sum(a4$wgt))
res_201802 - res_201802[1]

res_201802



# march =========
rw <- scan("cpi-u-201803.csv", sep = ",", what = "character", quote = '\"')
rw2 <- data.frame(matrix(rw, nc = 44, byr = T), stringsAsFactors = F)
rw2Lab <- rw2$X2
rw2Lab <- gsub("\\(|\\)", "", rw2Lab)
rw2Lab <- gsub("[0-9]+", "", rw2Lab)
rw2$Label <- rw2Lab
rw2 %>% filter(rw2$X3 != "") -> rw3

rw203 <- rw2
rw303 <- rw3

exClasses %>% filter(DATE == "2018-03-01") -> exc
test1 <- match(exc$value, as.numeric(rw303$X16))
# doublecheck by doing a second month
exClasses %>% filter(DATE == "2018-02-01") -> exc2
test2 <- match(exc2$value, as.numeric(rw303$X15)) 
test1 - test2

exc$weight <- as.numeric(rw303$X3[test1])/100
a1 <- names(exSim)
a2 <- lapply(exSim, function(x)c(x[[1]][3], x[[2]][3], x[[3]][3], 
                                 x[[4]][3], x[[5]][3], x[[6]][3]))
a2 <- do.call("rbind", a2)
a2 <- data.frame(a2)
names(a2) <- names(exSim[[1]])
a2$series <- a1

# now merge with file of actuals and weights (exc)
a3 <- data.frame(series = exc$series, DATE = exc$DATE, wgt = exc$weight, 
                 val0 = exc2$value, val1 = exc$value)

a4 <- merge(a3, a2, by = "series")
a5 <- data.frame(series = a4$series, err = as.numeric(rw303$X15[1])*(a4$wgt * (a4$actual - a4$farima)/a4$val0)/sum(a4$wgt))

a4$err <- (a4$actual - a4$farima)/a4$val0
a4$wgterr <- as.numeric(rw303$X15[1])*(a4$wgt * (a4$actual - a4$farima)/a4$val0)/sum(a4$wgt)
a4_201803 <- a4
res_201803 <- c(as.numeric(rw303$X15[1]) * sum(a4$wgt * a4$actual/a4$val0)/sum(a4$wgt),
                as.numeric(rw303$X15[1]) * sum(a4$wgt * a4$fnaive/a4$val0)/sum(a4$wgt),
                as.numeric(rw303$X15[1]) * sum(a4$wgt * a4$fmean12/a4$val0)/sum(a4$wgt),
                as.numeric(rw303$X15[1]) * sum(a4$wgt * a4$fnaiveS/a4$val0)/sum(a4$wgt),
                as.numeric(rw303$X15[1]) * sum(a4$wgt * a4$ftrend12/a4$val0)/sum(a4$wgt),
                as.numeric(rw303$X15[1]) * sum(a4$wgt * a4$farima/a4$val0)/sum(a4$wgt))
res_201803 - res_201803[1]




 
# april =======
rw <- scan("cpi-u-201804.csv", sep = ",", what = "character", quote = '\"')
rw2 <- data.frame(matrix(rw, nc = 44, byr = T), stringsAsFactors = F)
rw2Lab <- rw2$X2
rw2Lab <- gsub("\\(|\\)", "", rw2Lab)
rw2Lab <- gsub("[0-9]+", "", rw2Lab)
rw2$Label <- rw2Lab
rw2 %>% filter(rw2$X3 != "") -> rw3

rw204 <- rw2
rw304 <- rw3

exClasses %>% filter(DATE == "2018-04-01") -> exc
test1 <- match(exc$value, as.numeric(rw304$X16))
# doublecheck by doing a second month
exClasses %>% filter(DATE == "2018-03-01") -> exc2
test2 <- match(exc2$value, as.numeric(rw304$X15)) 
test1 - test2


exc$weight <- as.numeric(rw304$X3[test1])/100
a1 <- names(exSim)
a2 <- lapply(exSim, function(x)c(x[[1]][4], x[[2]][4], x[[3]][4], x[[4]][4], x[[5]][4], x[[6]][4]))
a2 <- do.call("rbind", a2)
a2 <- data.frame(a2)
names(a2) <- names(exSim[[1]])
a2$series <- a1

# now merge with file of actuals and weights (exc)
a3 <- data.frame(series = exc$series, DATE = exc$DATE, wgt = exc$weight, 
                 val0 = exc2$value, val1 = exc$value)

a4 <- merge(a3, a2, by = "series")
a5 <- data.frame(series = a4$series, err = as.numeric(rw304$X15[1])*(a4$wgt * (a4$actual - a4$farima)/a4$val0)/sum(a4$wgt))

a4$err <- (a4$actual - a4$farima)/a4$val0
a4$wgterr <- as.numeric(rw304$X15[1])*(a4$wgt * (a4$actual - a4$farima)/a4$val0)/sum(a4$wgt)
a4_201804 <- a4
res_201804 <- c(as.numeric(rw304$X15[1]) * sum(a4$wgt * a4$actual/a4$val0)/sum(a4$wgt),
                as.numeric(rw304$X15[1]) * sum(a4$wgt * a4$fnaive/a4$val0)/sum(a4$wgt),
                as.numeric(rw304$X15[1]) * sum(a4$wgt * a4$fmean12/a4$val0)/sum(a4$wgt),
                as.numeric(rw304$X15[1]) * sum(a4$wgt * a4$fnaiveS/a4$val0)/sum(a4$wgt),
                as.numeric(rw304$X15[1]) * sum(a4$wgt * a4$ftrend12/a4$val0)/sum(a4$wgt),
                as.numeric(rw304$X15[1]) * sum(a4$wgt * a4$farima/a4$val0)/sum(a4$wgt))
res_201804 - res_201804[1]








# may ======
rw <- scan("cpi-u-201805.csv", sep = ",", what = "character", quote = '\"')
rw2 <- data.frame(matrix(rw, nc = 44, byr = T), stringsAsFactors = F)
rw2Lab <- rw2$X2
rw2Lab <- gsub("\\(|\\)", "", rw2Lab)
rw2Lab <- gsub("[0-9]+", "", rw2Lab)
rw2$Label <- rw2Lab
rw2 %>% filter(rw2$X3 != "") -> rw3

rw205 <- rw2
rw305 <- rw3


exClasses %>% filter(DATE == "2018-05-01") -> exc
test1 <- match(exc$value, as.numeric(rw305$X16))
# doublecheck by doing a second month
exClasses %>% filter(DATE == "2018-04-01") -> exc2
test2 <- match(exc2$value, as.numeric(rw305$X15)) 
test1 - test2



exc$weight <- as.numeric(rw305$X3[test1])/100
a1 <- names(exSim)
a2 <- lapply(exSim, function(x)c(x[[1]][5], x[[2]][5], x[[3]][5], x[[4]][5], x[[5]][5], x[[6]][5]))
a2 <- do.call("rbind", a2)
a2 <- data.frame(a2)
names(a2) <- names(exSim[[1]])
a2$series <- a1

# now merge with file of actuals and weights (exc)
a3 <- data.frame(series = exc$series, DATE = exc$DATE, wgt = exc$weight, 
                 val0 = exc2$value, val1 = exc$value)

a4 <- merge(a3, a2, by = "series")
a5 <- data.frame(series = a4$series, err = as.numeric(rw305$X15[1])*(a4$wgt * (a4$actual - a4$farima)/a4$val0)/sum(a4$wgt))

a4$err <- (a4$actual - a4$farima)/a4$val0
a4$wgterr <- as.numeric(rw305$X15[1])*(a4$wgt * (a4$actual - a4$farima)/a4$val0)/sum(a4$wgt)
a4_201805 <- a4
res_201805 <- c(as.numeric(rw305$X15[1]) * sum(a4$wgt * a4$actual/a4$val0)/sum(a4$wgt),
                as.numeric(rw305$X15[1]) * sum(a4$wgt * a4$fnaive/a4$val0)/sum(a4$wgt),
                as.numeric(rw305$X15[1]) * sum(a4$wgt * a4$fmean12/a4$val0)/sum(a4$wgt),
                as.numeric(rw305$X15[1]) * sum(a4$wgt * a4$fnaiveS/a4$val0)/sum(a4$wgt),
                as.numeric(rw305$X15[1]) * sum(a4$wgt * a4$ftrend12/a4$val0)/sum(a4$wgt),
                as.numeric(rw305$X15[1]) * sum(a4$wgt * a4$farima/a4$val0)/sum(a4$wgt))
res_201805 - res_201805[1]

# accumulate results ======
res_2018 <- rbind(res_201801, res_201802, res_201803, res_201804, res_201805)
dimnames(res_2018)[[2]] <- names(exSim[[1]])

res_2018 - res_2018[, 1]
res_MAE <- apply(res_2018 - res_2018[, 1], 2, function(x)sum(abs(x)))/5
res_MSE <- apply(res_2018 - res_2018[, 1], 2, function(x)sum(x^2))/5
res_MAE
res_MSE














