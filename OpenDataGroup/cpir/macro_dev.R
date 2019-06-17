# 2010 CPI with calculated monthly weights

devtools::load_all("/Users/cwcomiskey/Desktop/ODG/Macro-models/macro")
macro::depends() # load all dependencies

# Dev ===============

# devtools::use_data(index)

weights[1,1:5]
subindices[1:5, 1:5]
head(index)

# ==============

s70 <- macro::strata70_reg_dat
s68 <- s70[,-c(1,2)] # remove date, CPI

w <- macro::riws2009 %>%
  select(item_code, CPI_U) %>% 
  spread(item_code, CPI_U) %>%
  select(names(s68)) 

# =========

for(m in 2:29){
  if(year(subindices$date[m]) > 2011){rm(m); break}
  weights[m,2:69] <- cpir::weightr(weights[1,-1], 
                               subindices[1,-1],
                               index[1,2],
                               subindices[m,-1],
                               index[m,2])
  weights[m,1] <- subindices[m,1]
} # calculate weights

cpi.hat <- data.frame(index = index[1,"CPI"]) # container
CPI <- data.frame(index = index[1,"CPI"]) 

for(m in 2:100){
  if(year(subindices$date[m]) > 2011){rm(m); break}
  cpi.hat[m,1] <- cpir::indexr(weights[m-1,-1], 
                               subindices[m-1,-1], 
                               index[m-1,2], 
                               subindices[m,-1])
} # calculate CPI

r2 <- function(y, yhat){
  1- sum((yhat - y)^2) / sum( (y - mean(y))^2)
} # calculate r^2

r2(y = diff(CPI$index[1:25]), yhat = diff(cpi.hat$index))

# Plotting ================== #

plot_dat <- rbind.data.frame(
  data.frame(date = subindices$date[1:25],
             value = cpi.hat$index, 
             Cat = "CPI_hat"), 
  data.frame(date = subindices$date[1:25],
             value = index$CPI[1:25], 
             Cat = "CPI")
  )

ggplot(data = plot_dat) + 
  geom_line(aes(x = date, y = value, color = Cat), size = 1.5) +
  ggtitle("CPI Month-to-month Change Estimates with Calculated Weights") +
  theme(plot.title = element_text(hjust = 0.5))
