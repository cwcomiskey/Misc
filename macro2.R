# Linear regression: lm(CPI ~ Top25, ...) =======

lin_reg <- lm(CPI ~ ., data = select(reg_dat, -date))

s <- summary(lin_reg)
lin_reg <- lm(CPI ~ SETB + SEHB + SAH21 + SETG01 + SEFV + SAF11 +
                SEAC + SEAA + SEMC + SEHA + SEMF - 1, data = reg_dat)
s <- summary(lin_reg)
names(s)
summary(lin_reg)
ggplot() + geom_line(aes(x = residuals(lin_reg)))

# ID hunting ==
IDs <- c("SETB", "SEHB", "SAH21", "SETG01", "SEFV", "SAF11",
         "SEAC", "SEAA", "SEMC", "SEHA", "SEMF")
keepers <- corr_matr %>% filter(substring(series_id, 9) %in% IDs)
corr_matr <- corr_matr %>% mutate(keeper = as.numeric(substring(series_id, 9) %in% IDs))
# ============= %

# Formatting for plotting regression results ======
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

# Auto ARIMA and plot======
library(forecast)

t <- auto.arima(ts(lin_reg$residuals), max.p = 3, max.d = 3, max.q = 3, max.P = 3, max.Q = 3, max.D = 13 )
autoplot(acf(t$residuals, lag.max = 48)) +
  ggtitle("New Residuals: Regression with AR(1) Errors") +
  theme(plot.title = element_text(hjust = 0.5))
# ggsave("Mod+AR1Resid.jpg", height = 4, width = 8)

reg_dat2 <- drop_na(reg_dat) %>% select(-date, -index, -fit, -residuals)
arima(reg_dat2$CPI, order = c(1,1,0), xreg = reg_dat2[,-1])


