# Hotwire API

library(httr)

# The Rental Car Shopping API base URL is:
base <- "http://api.hotwire.com/v1/search/car"

# e.g. "http://api.hotwire.com/v1/search/car?apikey=abc123&dest=LAX&startdate=01/20/2010&enddate=01/23/2010&pickuptime=10:00&dropofftime=13:30"

?modify_url

modify_url("http://api.hotwire.com/v1/search/car", 
           query = list(apikey = "abc123",
                        dest = "LAX",
                        startdate = "01/20/2010"))

GET("http://api.hotwire.com/", path = "v1/search/car", 
    query = list(apikey="abc123", dest="LAX", 
                 startdate="01/20/2010", enddate="01/23/2010", 
                 pickuptime="10:00", dropofftime="13:30"))
