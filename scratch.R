# Scratch

# 1 string functions ======
paste()
paste0()
file.path()
gsub()

cat("testing, 1, 3, ", x, ", 7, 9", sep = "")

paste("~/a/very/long/path/here",
      "/and/then/some/more",
      "/and/then/some/more",
      "/and/then/some/more", sep=",")

?cat


# 1.5 if{} else{}
# 2 return() =========
.test <- function() "This work?"
test()

t <- function(){
  d <- list()
  for(i in 1:5){
    d[[paste(i)]] <- i
  }
  return(d)
}
t()
dat <- t()
x <- dat["2"]
str(x)

t2 <- function(){
  d <- list()
  for(i in names(dat)){
    d[[paste(i)]] <- i
  }
  d
}
t2()

# "If there are no explicit returns from a function, the value of the last evaluated expression is returned automatically in R."

# 3: FastScoreError$new() ======
fse <- FastScoreError$new(message = "Unable to retrieve active sensors", caused_by = "This was caused by something.")
fse$error_string()
FastScoreError$new(message = "Unable to retrieve active sensors", caused_by = "This was caused by something.")$error_string()

# 4: InstanceBase$new()$active_sensors() =======

x <- InstanceBase$new(name = "Betsy")
InstanceBase$new(name = "Betsy")$active_sensors()
x$active_sensors()

# 5: InstanceBase$new()$tapping_points() =======
x <- InstanceBase$new(name = "Besty")
x$tapping_points()
InstanceBase$new(
  
)$tapping_points()


# 6: InstanceBase$new()$un/install_sensor() ====
b <- InstanceBase$new(name = "Betsy")
name
b$name
# 6.01: InstanceBase$new ========
# api_cli <- InstanceBase$new(name = "FS") # don't need this; see 7

# 6.02: parse_url(), build_url() ======
A <- httr::parse_url("https://localhost/api/1/service")
B <- httr::parse_url("https://localhost:15080")

A$port <- B$port
httr::build_url(A)
# 7: Connect$new() =====
#     Proxy prefix = https://localhost:15080
#     Base path = api/1/service

httr::set_config(httr::config(ssl_verifypeer = FALSE))

api <- fastscore::InstanceBase$new()
con <- fastscore::Connect$new(proxy_prefix = "https://localhost:15080")

  A <- parse_url(con$apiClient$basePath) # [1] "https://localhost/api/1/service"
  B <- parse_url(con$proxy_prefix) # [1] "https://localhost:15080"
  A$port <- B$port
  con$apiClient$basePath <- build_url(A)
  
  health <- con$health_get(instance = "connect")



