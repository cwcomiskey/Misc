# Original parsing

# Shift negative months_in_field units ======

thermoking <- read_csv("~/Desktop/ODG/IngersolRand/NA DTF wPREDICTORS 3.csv")

# Modify two variables for units with negative "months_in_field" values:
#    (1) "months_in_field" and (2) "max_months_in_field"

# Determine minimum months_in_field for each serial number
tk_neg <- thermoking %>%
  filter(serial %in% unique(thermoking[thermoking$months_in_field < 0,]$serial)) %>%
  group_by(serial) %>%
  summarise(Min = min(months_in_field))

x <- Sys.time()
for(i in 1:dim(tk_neg)[1]){

  # Filter to one serial number
  thermoking_i <- thermoking %>%
    filter(serial == paste(tk_neg[i,"serial"])) %>%
    mutate(months_in_field = months_in_field - as.numeric(tk_neg[i,"Min"]),
           max_months_in_field = max_months_in_field - as.numeric(tk_neg[i,"Min"]))

  # Remove old values of serial number i, add new ones
  thermoking <- thermoking %>%
    filter(serial != paste(tk_neg[i,"serial"])) %>%
    rbind.data.frame(., thermoking_i)

  if(i == dim(tk_neg)[1]){
    rm(i, tk_neg, thermoking_i)
    thermoking <- thermoking %>%
      mutate(months_in_field = as.numeric(months_in_field),
             max_months_in_field = as.numeric(max_months_in_field))
  }

}
Sys.time() - x; rm(x)

# 100 Sample units
set.seed(0)
serials <- sample(unique(thermoking$serial), 100) # 100 units

# Data for sample units **BEFORE** Anderson-Gill change
thermoking_sample_raw <- thermoking %>%
  filter(serial %in% serials)# 534 observations

# Transform to Anderson-Gill style ======
# "AG_event" definition: last full month of survival

thermoking <- thermoking %>%
  arrange(serial, months_in_field) %>%
  group_by(serial) %>%
  mutate(
    time1 = months_in_field,
    time2 = lead(months_in_field),
    AG_event = lead(event)
    ) %>%
  filter(is.na(AG_event) == FALSE) %>%
  select(-event)

# *Row reduction note ===== #
# Original: 53619 rows (10638 unique serials)
# AG version: 42981 rows (10053 unique serials)
# ---> Lose one row from every serial
# ---> 585 serials only have one month data
# Used: thermoking %>% group_by(serial) %>% summarise(n())

# Thermo King sample POST AG  =====

# The 100 sample units, AG format
thermoking_sample <- thermoking %>%
  filter(serial %in% serials) # 434 observations

# Convert thermoking_sample_raw (pre-AG) to thermoking_sample (AG format) ====
hope <- ag.transform(thermoking_sample_raw,
                     id = serial,
                     age = months_in_field,
                     event = event)

all.equal(thermoking_sample, hope)

