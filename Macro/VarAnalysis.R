# Variability Analysis 

# Variability analysis (EDA) ======
dat <- strata70_reg_dat # %>% select(one_of(names(reg_dat)))
dat2 <- data.frame(Date = rep(dat$date, 25), Strata = 0, Index = 0)
for(i in 2:26){
  l <- (i-2)*100 + 1
  u <- (i-1)*100
  dat2[l:u,"Strata"] = names(dat)[i]
  dat2[l:u, "Index"] = dat[,i]
  if(i == 26) rm(i,l,u)
}

ggplot(data = dat2) + 
  geom_line(aes(x = Date, y = Index, color = Strata)) +
  theme(legend.position="none")
# ggtitle("CPI Month-to-month... Calculated Weights") +
# theme(plot.title = element_text(hjust = 0.5))

ggplot(data = dat2) + 
  geom_line(aes(x = Date, y = Index)) +
  facet_wrap(~ Strata, scales = "free")

apply(dat, 2, sd)

# ggsave("24strata_var.jpg", width = 12, height = 5)

r <- corr_matr %>% 
  mutate(series_id = substring(series_id, 9)) %>%
  filter(series_id %in% names(dat)) %>%
  arrange(desc(Cor))

dat[,3:26] <- riws25[match(
  names(dat)[-c(1,2)], 
  r$series_id
),
]

# Weighted Strata Variability Analysis =====
reg_dat <- strata70_reg_dat
w <- riws2009 
s70 <- strata70_reg_dat %>% drop_na() %>% 
  filter(year(date) <  2012) %>% 
  select(one_of("date", "CPI", w$item_code)) 
w <- w[match(names(s70[-c(1,2)]), w$item_code),]

weights70 <- cbind.data.frame(date = s70$date, CPI = 0, matrix(nrow = 25, ncol = 68))
colnames(weights70) <- c("date", "CPI.hat", w$item_code) 
weights70[1,3:70] <- w$CPI_U; rm(w); rm(reg_dat)

for(m in 2:25){
  weights70[m,3:70] <- weights70[1,3:70] * (s70[m,3:70] / s70[1,3:70]) * (s70[1,"CPI"] / s70[m,"CPI"])
  if(m == 25) {rm(m); weights70$CPI.hat[1] <- s70$CPI[1]}
} # Calculate weights70 for CPI 

for(m in 2:25){
  weights70[m,"CPI.hat"] <- s70[m-1,"CPI"] * 
    sum(weights70[m-1,3:70] * (s70[m,3:70] / s70[m-1,3:70]))/sum(weights70[m-1,3:70])
  if(m == 25) rm(m)
} # Calculate CPI

# Weighted indices ==== #
windex <- s70[,3:70] * weights70[,3:70]

# Plot action ===== #
dat <- cbind.data.frame(s70[,c(1,2)], windex)
dat2 <- data.frame(Date = rep(dat$date, 69), Strata = 0, WeightedIndex = 0)
for(i in 2:70){
  l <- (i-2)*25 + 1
  u <- (i-1)*25
  dat2[l:u,"Strata"] = names(dat)[i]
  dat2[l:u, "WeightedIndex"] = dat[,i]
  if(i == 70) rm(i,l,u)
}

ggplot(data = dat2) + 
  geom_line(aes(x = Date, y = WeightedIndex, color = Strata)) +
  theme(legend.position="none") + 
  ggtitle("Weighted Strata Indices") +
  theme(plot.title = element_text(hjust = 0.5))

id <- item_dat %>% filter(item_code %in% names(dat))
