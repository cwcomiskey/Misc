# 2010 CPI with calculated monthly weights

devtools::load_all("/Users/cwcomiskey/Desktop/ODG/Macro-models/macro")
macro::depends() # load all dependencies

s70 <- strata70_reg_dat
s68 <- s70[,-c(1,2)] # remove date, CPI

w <- riws2009 %>%
  select(item_code, CPI_U) %>% 
  spread(item_code, CPI_U) %>%
  select(names(s68))

cpi.hat <- data.frame(index = s70$CPI[1]) # container
CPI <- data.frame(index = s70$CPI) 

#   s0/1, w0, CPI0/1 --- subindex, weight, CPI --> reference/current
weight <- function(w0, s0, CPI0, s1,  CPI1){
  w0 * (s1 / s0) * (CPI0 / CPI1)
}
for(m in 2:29){
  if(year(s70$date[m]) > 2011){rm(m); break}
  w[m,] <- weight(w[1,], s68[1,], CPI[1,1], s68[m,], CPI[m,1])
} # calculate weights

cpi_calc <- function(w0, s0, CPI0, s1){
  CPI0 * sum(w0 * (s1 / s0) )/sum(w0)
}
for(m in 2:100){
  if(year(s70$date[m]) > 2011){break; rm(m)}
  cpi.hat[m,1] <- cpi_calc(w[m-1,], s68[m-1,], CPI[m-1,1], s68[m,])
  if(m == 100) rm(m)
} # calculate CPI

r2 <- function(y, yhat){
  1- sum((yhat - y)^2) / sum( (y - mean(y))^2)
} # calculate r^2

r2(y = diff(CPI$index[1:25]), yhat = diff(cpi.hat$index))

# Plotting ================== #

plot_dat <- rbind.data.frame(
  data.frame(date = s70$date[1:25],
             value = cpi.hat$index, 
             Cat = "CPI_hat"), 
  data.frame(date = s70$date[1:25],
             value = CPI$index[1:25], 
             Cat = "CPI")
  )

ggplot(data = plot_dat) + 
  geom_line(aes(x = date, y = value, color = Cat), size = 1.5) +
  ggtitle("CPI Month-to-month Change Estimates with Calculated Weights") +
  theme(plot.title = element_text(hjust = 0.5))
