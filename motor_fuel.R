# Transportation: CUUR0000SAT

# Transportation Major Group=====

t <- function(){
trans <- httr::GET(
  url = "https://download.bls.gov/pub/time.series/cu/cu.data.14.USTransportation") %>% 
  content("text") %>% 
  data.table::fread() %>%
  filter(period %in% c("M01", "M02", "M03", "M04", "M05", "M06", 
                       "M07", "M08", "M09", "M10", "M11", "M12")) %>%
  mutate(month = substring(period, 2), 
         date = ymd(paste0(year, month, "01"))) %>%
  select(-footnote_codes, -period) %>%
  left_join(., meta_dat, by = "series_id") %>%
  filter(area_code == "0000", seasonal == "U", periodicity_code == "R") %>%
  mutate(series_title = str_replace(series_title, 
    " in U.S. city average, all urban consumers, not seasonally adjusted", "")) %>%
  select(-seasonal, -periodicity_code, -base_code, -area_code, 
         -footnote_codes, -begin_period, -end_period) %>%
  filter(series_title %in% c(# "Transportation", 
                             # "Private transportation", 
                             # "Motor fuel",  
                             # "Other motor fuels", 
                             "Gasoline (all types)",
                             "Gasoline, unleaded regular", 
                             "Gasoline, unleaded midgrade", 
                             "Gasoline, unleaded premium" ))
return(trans)
}
trans <- t(); rm(t)

r <- function(){
riws09 <- read_table("2009RIWs_0708wts") %>%
  drop_na() %>%
  mutate(Item_name = gsub("\\.*", "" , `Expenditure category`)) %>% 
  select(Item_name, CPI_U = X2) %>%
  left_join(., item_dat, by = c("Item_name" = "item_name"))
return(riws09)
}
riws09 <- r(); rm(r)

unique(trans$series_title)

trans <- trans %>% select(series_title, item_code, date, value)
gas <- trans %>% filter(item_code == "SETB01") %>% select(date, gas = value)
g_reg <- trans %>% filter(item_code == "SS47014") %>% select(date, reg= value)
g_mid <- trans %>% filter(item_code == "SS47015") %>% select(date, mid = value)
g_prem <- trans %>% filter(item_code == "SS47016") %>% select(date, prem = value)
g <- full_join(gas, g_reg) %>% full_join(., g_mid) %>% full_join(.,g_prem)

gas_regr <- lm(diff(gas) ~ diff(reg) + diff(mid) + diff(prem), data = g)
summary(gas_regr)
sum(gas_regr$coefficients)
