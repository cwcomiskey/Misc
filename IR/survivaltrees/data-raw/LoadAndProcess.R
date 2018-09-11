# Original parsing

thermoking <- read_csv("~/Desktop/ODG/IngersolRand/NA DTF wPREDICTORS 3.csv")

# Modify two variables for units with negative "months_in_field" values:
#    (1) "months_in_field" and (2) "max_months_in_field"

# Determine minimum months_in_field for each serial number
tk_neg <- thermoking %>%
  filter(serial %in% unique(thermoking[thermoking$months_in_field < 0,]$serial)) %>%
  group_by(serial) %>%
  summarise(Min = min(months_in_field))

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
  }

}

# Format to Anderson-Gill style ======
# A-G event: I{Last full month of survival}

thermoking <- thermoking %>%
  mutate(
    time1 = months_in_field,
    time2 = lead(months_in_field),
    AG_event = lead(event)
    ) %>%
  filter(event != 1) %>%
  select(-event)



