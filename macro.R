devtools::load_all("/Users/cwcomiskey/Desktop/ODG/Macro-models/macro")

macro::depends() # load all dependencies

# Regression: lm(CPI ~ Top25, ...) =======

lin_reg <- lm(CPI ~ ., data = select(reg_dat, -date))
summary(lin_reg) # R^2:  0.9865

sum_na <- function(x) sum(as.numeric(is.na(x)))
apply(reg_dat, FUN = sum_na, MARGIN = 2) # NAs by column; a lot

ggplot() + geom_line(aes(x = residuals(lin_reg)))

# previous monthly changes (lag 1, 12) as covariates
reg_dat <- reg_dat %>% mutate(last_month = lag(CPI, n = 1)) # last month's change
lin_reg <- lm(CPI ~ ., data = select(reg_dat, -date))
summary(lin_reg)
# --> last month's change is not a significant predictor
# --> 12 months ago change is not a significant predictor

# Plot regression results ======
# Add fitted values to reg_dat for plotting by date ==
reg_dat <- reg_dat %>% mutate(index = rownames(reg_dat))
fitt <- cbind.data.frame(fitted(lin_reg), residuals(lin_reg))
fitt <- mutate(fitt, index = row.names(fitt))
colnames(fitt) <- c("fit", "residuals", "index")
reg_dat <- left_join(reg_dat, fitt)

# Manual melt for plotting ==
plot_dat <- reg_dat %>% select(date, CPI, fit, residuals) %>% drop_na()
plot_dat_CPI <- data.frame(plot_dat[,1:2], "CPI")
names(plot_dat_CPI) <- c("date", "Change", "Source")
plot_dat_fit <- data.frame(plot_dat[,c(1,3)], "Fit")
names(plot_dat_fit) <- c("date", "Change", "Source")
plot_dat2 <- rbind.data.frame(plot_dat_CPI, plot_dat_fit)

# Residuals: geom_point, acf, pacf =====
ggplot(data = plot_dat) +
  geom_line(aes(y = residuals, x = date))
# ggsave("ModelResiduals.jpg", width = 8, height = 4)

autoplot(decompose(plot_dat$residuals))

autoplot(acf(plot_dat$residuals, na.action = na.pass, plot = FALSE, lag.max = 24)) +
  ggtitle("Model Residual Autocorrelation") +
  theme(plot.title = element_text(hjust = 0.5))
# ggsave("ModResidACF.jpg", width = 8, height = 4)

autoplot(pacf(plot_dat$residuals, na.action = na.pass, plot = FALSE, lag.max = 24)) +
  ggtitle("Model Residual Partial Autocorrelation") +
  theme(plot.title = element_text(hjust = 0.5))
# ggsave("ModResidPACF.jpg", width = 8, height = 4)


# Changes: CPI vs. Fit (plot) =====
ggplot(data = plot_dat2) +
  geom_line(aes(x = date, y = Change, color = Source), size = 1.25)
ggsave("CPIvFit.jpg", width = 10, height = 5)

ggplot() +
  geom_point(aes(x = fitted(lin_reg),
                 y = residuals.lm(lin_reg))) +
  xlab("Fitted Values") + ylab("Residuals")

autoplot(lin_reg)[1:2]

# Train and test ======
train_dat <- reg_dat[1:1243,]
test_dat <- reg_dat[1244:1263,]
lin_reg2 <- lm(CPI ~ ., data = select(train_dat, -date))

preds <- predict.lm(lin_reg2, test_dat)
1- sum( (preds - test_dat$CPI)^2) / sum( (test_dat$CPI - mean(test_dat$CPI))^2 )

test_dat$pred <- predict(lin_reg2, test_dat)

train_dat <- train_dat %>% drop_na()

ggplot() +
  geom_line(data = rbind.data.frame(train_dat[,1:2], test_dat[,1:2]),
            aes(x = date, y = CPI), color = "red", size = 1.25) +
  geom_line(data = test_dat, aes(x = date, y = pred),
            color = "blue", size = 1.25) +
  ylab("CPI Change") +
  ggtitle(expression(paste("Linear Regression, ", R^{2}, " = 0.91")))

ggsave("CPIvFit_train_test.jpg", width = 10, height = 5)

# auto.arima(...) and plots ======
lin_reg <- lm(CPI ~ ., data = select(reg_dat, -date))
t <- auto.arima(lin_reg$residuals, max.p = 3, max.d = 3, max.q = 3, max.P = 3, max.Q = 3, max.D = 13 )

autoplot(acf(t$residuals, lag.max = 48, plot = FALSE)) +
  ggtitle("New Residuals: Regression with AR(1) Errors") +
  theme(plot.title = element_text(hjust = 0.5))
# ggsave("Mod+AR1Resid.jpg", height = 4, width = 8)

# arima(...) and plots =======
d <- drop_na(reg_dat) %>% select(-date)
mod <- arima(d$CPI, order = c(1,0,0), xreg = select(d, -CPI))

1 - sum( (fitted(mod) - d$CPI)^2) / sum( (d$CPI - mean(d$CPI))^2 ) # [1] 0.9882776

ggplot() + geom_point(aes(x = 1:99, y = residuals(mod)))
autoplot(acf(mod$residuals, lag.max = 48, plot = FALSE)) 

# arima(...) -- train and test ======

# Divide into training data and test data
d_train <- d[1:90,]
d_test <- d[91:99,]

# Fit model with training data, white noise residuals ==== #
lin_reg <- lm(CPI ~ ., data = d_train)
summary(lin_reg)
autoplot(acf(lin_reg$residuals, plot = FALSE))

preds <- predict.lm(lin_reg, d_test) # predictions on test data
1- sum( (preds - d_test$CPI)^2) / sum( (d_test$CPI - mean(d_test$CPI))^2 ) # 0.9317969

# arima(...) fit with AR(1) residuals ====== #
lin_reg <- arima(d_train$CPI, order = c(1,0,0), xreg = select(d_train, -CPI))
autoplot(acf(lin_reg$residuals, plot = FALSE))

preds <- predict(lin_reg, newxreg = select(d_test, -CPI))$pred
1- sum( (preds - d_test$CPI)^2) / sum( (d_test$CPI - mean(d_test$CPI))^2 ) # 0.9368632



# CPI-change ~= top25 %*% RIWs (incomplete) ======

# Create RIW df ====== #
# riws <- read_table("riws") %>%
#   drop_na() %>%
#   mutate(`Item and group` = gsub("\\.*", "" , riws$`Item and group`))

# strata_riws <- riws %>% filter(`Item and group` %in% strata_dat$item_name)
# names(strata_riws) <- c("item_name", "CPI-U", "CPI-W")
# strata_riws <- left_join(strata_riws, item_dat)

# devtools::use_data(strata_riws, overwrite = TRUE)
# ===================== #

# Match: "reg_dat" columns to "strata_riws" rows, for matrix mult ==== #

# strata_riws$item_code %in% names(reg_dat)
# names(reg_dat) %in% strata_riws$item_code)

riws25 <- strata_riws %>% filter(item_code %in% names(reg_dat)) 
reg_dat <- reg_dat %>% select(-SERF01, -date, -CPI) %>% drop_na()

riws25 <- riws25[match(
  names(reg_dat)[-c(1,2)], 
  riws25$item_code
  ),
  ] # to make row order match reg_dat column order, for matrix mult



CPI <- data.frame(reg_dat[,c("date", "CPI")], "CPI") 
colnames(CPI) <- c("date", "value", "cat")
calc <- data.frame(reg_dat[,c("date")], w_avg, "RIWs")
colnames(calc) <- c("date", "value", "cat")
l_reg <- data.frame(reg_dat[,c("date")], lin_reg$fitted.values, "Lin_Reg")
colnames(l_reg) <- c("date", "value", "cat")


plot_dat <- rbind.data.frame(CPI, calc, l_reg)
                
ggplot(data = plot_dat) +
  geom_line(aes(x = date, y = value, color = cat), size = 1.25)
ggsave("CPI_RIWs_LinReg.jpg", width = 12, height = 5)



# lm(...), f_25, f_71 =====================================
# Note: 2015 - 2016 weights
# https://www.bls.gov/cpi/tables/relative-importance/home.htm

riws25 <- strata_riws %>% filter(item_code %in% names(reg_dat)) 
strata25 <- strata_dat %>% 
  filter(item_code %in% riws25$item_code)

# Create proper df for regression ========= %
for(i in 1:24){
  if(i == 1) {
    strata_reg_dat <- CPI %>% select(date, value) 
    names(strata_reg_dat) <- c("date", "CPI")
  }
  
  strata_i <- strata25 %>% 
    filter(item_code == riws25[i,"item_code"]) %>%
    select(value, date)
  
  strata_reg_dat <- full_join(strata_reg_dat, strata_i, by = "date") 
  strata_reg_dat <- within(strata_reg_dat, rm(series_id))
  names(strata_reg_dat)[names(strata_reg_dat) == 'value'] <- paste(riws25[i,"item_code"])
  
  if(i == 24) {
    rm(strata_i, i)
    strata_reg_dat <- drop_na(strata_reg_dat)
    }
}

# Regression ========= %
lin_reg <- lm(CPI ~ ., data = select(strata_reg_dat, -date))

# Top 25 RIW calculation ========= %

riws25 <- riws25[match(
  names(strata_reg_dat)[-c(1,2)], 
  riws25$item_code),] 

riws <- as.vector(t(riws25[,2]))
CPI <- as.vector(strata_reg_dat[,2])
strata <- strata_reg_dat %>% select(-date, -CPI)

for(i in 2:100){
  if(i ==2) CPI.hat25 <- CPI[1]
  CPI.hat25[i] <- CPI[i-1]*(sum(riws*(strata[i,]/strata[i-1,]))/sum(riws))
  if(i == 100) rm(i)
}

1- sum( (CPI.hat25 - strata_reg_dat$CPI)^2) / sum( (strata_reg_dat$CPI - mean(strata_reg_dat$CPI))^2 )

# CPI from 70 RIWs ================= #

strata_riws_ordered <- strata_riws[match(
  names(strata70_reg_dat)[-c(1,2)],
  strata_riws$item_code),]

riws <- as.vector(t(strata_riws_ordered[,2])); rm(strata_riws_ordered)
# CPI <- as.vector(strata70_reg_dat[,2])
strata <- strata70_reg_dat %>% select(-date, -CPI)

for(i in 2:100){
  if(i ==2) CPI.hat <- CPI[1]
  CPI.hat[i] <- CPI[i-1]*(sum(riws*(strata[i,]/strata[i-1,])))/100
}

1- sum( (CPI.hat - strata_reg_dat$CPI)^2) / sum( (strata_reg_dat$CPI - mean(strata_reg_dat$CPI))^2 )

# Plot ============== #

CPI <- data.frame(strata_reg_dat[,c("date", "CPI")], "CPI") 
colnames(CPI) <- c("date", "value", "cat")
calc <- data.frame(strata_reg_dat[,c("date")], CPI.hat25, "RIW25")
colnames(calc) <- c("date", "value", "cat")
l_reg <- data.frame(strata_reg_dat[,c("date")], lin_reg$fitted.values, "Lin_Reg")
colnames(l_reg) <- c("date", "value", "cat")
calc70 <- data.frame(strata70_reg_dat[,c("date")], CPI.hat, "RIW70")
colnames(calc70) <- c("date", "value", "cat")

plot_dat <- rbind.data.frame(CPI, calc, l_reg, calc70); rm(CPI, calc70)

ggplot(data = plot_dat) +
  geom_line(aes(x = date, y = value, color = cat), size = 1.75)

# ggsave("All.jpg", width = 12, height = 5)

# Changes plot ===========

CPI <- data.frame(strata_reg_dat$`date`[-1],  diff(strata_reg_dat$`CPI`), "CPI") 
colnames(CPI) <- c("date", "value", "cat")
calc <- data.frame(strata_reg_dat$`date`[-1], diff(CPI.hat25), "RIW25")
colnames(calc) <- c("date", "value", "cat")
l_reg <- data.frame(strata_reg_dat$`date`[-1], diff(lin_reg$fitted.values), "Lin_Reg")
colnames(l_reg) <- c("date", "value", "cat")
calc70 <- data.frame(strata_reg_dat$`date`[-1], diff(CPI.hat), "RIW70")
colnames(calc70) <- c("date", "value", "cat")

plot_dat2 <- rbind.data.frame(CPI, calc, l_reg, calc70); rm(CPI, calc, l_reg, calc70)

ggplot(data = plot_dat2) +
  geom_line(aes(x = date, y = value, color = cat), size = 1.75) + 
  ggtitle("Month-to-month Changes") +
  theme(plot.title = element_text(hjust = 0.5))

# ggsave("All_changes.jpg", width = 12, height = 5)

1- sum( (diff(CPI.hat) - diff(strata_reg_dat$`CPI`))^2) / sum( (diff(strata_reg_dat$`CPI`) - mean(diff(strata_reg_dat$`CPI`)))^2 )
# RIW25 = -0.2146562
# LinReg = 0.9744422
# RIW70 = 0.9445965

# 2010 CPI =================================================
# Create RIW df ====== #
load2009riws <- function(){
riws2009 <- read_table("2009RIWs_0708wts") %>%
  drop_na() %>%
  mutate(Item_name = gsub("\\.*", "" , `Expenditure category`)) %>% 
  select(Item_name, CPI_U = X2) %>%
  left_join(., item_dat, by = c("Item_name" = "item_name")) %>% 
  # ...riws2009 has __(not sure)__ and item_dat == "Recorded music and music subscriptions"
  filter(display_level == 2 | 
           Item_name %in% c("Airline fare", "Cable and satellite television and radio service")) 
  # mutate(RIW_norm = CPI_U/sum(CPI_U) * 100)

  # Correct and add
  riws2009[riws2009$Item_name == "Airline fare", c("Item_name", "item_code")] <- c("Airline fares", "SETG01")
  riws2009[riws2009$Item_name == "Cable and satellite television and radio service", c("Item_name", "item_code")] <- c("Cable and satellite television service", "SERA02")
  riws2009[68,] <- NA 
  riws2009[68,1] <- "Recorded music and music subscriptions"
  riws2009[68,2] <- 0.638
  riws2009[68,3] <- "SERA06"                 
  riws2009[68,4] <- 2
  
  return(riws2009)
  
  # Diagnostics
  # missing <- names(strata70_reg_dat)[!(names(strata70_reg_dat) %in% riws2009$item_code)]
  # item_dat[item_dat$item_code %in% missing,]
  # unique(filter(strata_dat, item_code %in% missing)[,"item_name"])
  }
# riws2009 <- load2009riws()
data("riws2009"); head(riws2009)
data("strata70_reg_dat"); head(strata70_reg_dat)

sum(riws2009$CPI_U) 

riws2009 <- riws2009[match(
  names(strata70_reg_dat)[-c(1,2)], 
  riws2009$item_code),]

# Calculate January 2010 weights ============ #
# # (1) Dec 2009 RIWs: riws2009
# D09w <- as.data.frame(t(riws2009[,"CPI_U"]))
#   
# # (2) Dec2009/Jan2010 subindices and CPI: strata70_reg_dat
# D09 <- strata70_reg_dat[1,3:70]
#   colnames(D09w) <- colnames(D09)
# J10 <- strata70_reg_dat[2,3:70]
# 
# weights <- D09w * (J10/D09) # Jan'10 weights

# Try to automate it ====== #
riws2009 <- riws2009[match(
  names(strata70_reg_dat)[-c(1,2)], 
  riws2009$item_code),]

s70 <- strata70_reg_dat[,-c(1,2)] # shorter name!
cpi.hat <- data.frame(index = strata70_reg_dat$CPI[1]) # initialize CPI estimate cont.
CPI <- data.frame(index = strata70_reg_dat$CPI) # CPI
rm(riws2009)

for(m in 1:29){
  if(m == 1){
    weights <- as.data.frame(t(riws2009[,"CPI_U"])) # initialize calculated weights cont.
    colnames(weights) <- riws2009$item_code # name weights same as index names
  } else{
  if(year(strata70_reg_dat$date[m]) > 2011){rm(m); break}
  weights[m,] <- weights[m-1,] * (s70[m,] / s70[m-1,]) * (CPI[m-1,1] / CPI[m,1])
  }
} # Calculate weights for CPI 

apply(weights, 1, sum)
qplot(x = apply(weights, 1, sum), geom = "histogram")
100/apply(weights, 1, sum)

for(m in 2:100){
  if(year(strata70_reg_dat$date[m]) > 2011){break}
  cpi.hat[m,1] <- CPI[m-1,1] * sum( (s70[m,] / s70[m-1,]) * weights[m-1,])/100
  if(m == 100) rm(m)
} # Calculate CPI

summary(cpi.hat - CPI[1:25,1])

CPIs <- data.frame(strata70_reg_dat$date[1:25],  cpi.hat$index, "CPI_hat")
colnames(CPIs) <- c("date", "value", "Cat")
CPI.hats <- data.frame(strata70_reg_dat$date[1:25], CPI$index[1:25], "CPI")
colnames(CPI.hats) <- c("date", "value", "Cat")

# CPIs <- data.frame(strata70_reg_dat$date[2:25],  diff(cpi.hat$index), "CPI_hat") 
# colnames(CPIs) <- c("date", "value", "Cat")
# CPI.hats <- data.frame(strata70_reg_dat$date[2:25], diff(CPI$index[1:25]), "CPI")
# colnames(CPI.hats) <- c("date", "value", "Cat")

plot_dat <- rbind.data.frame(CPIs, CPI.hats)

ggplot(data = plot_dat) + 
  geom_line(aes(x = date, y = value, color = Cat), size = 1.5) 
  # ggtitle("CPI Month-to-month Change Estimates with Calculated Weights") +
  # theme(plot.title = element_text(hjust = 0.5))

# ggsave("Weights.jpg", width = 12, height = 5)
