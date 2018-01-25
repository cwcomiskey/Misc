# Scratch

httr::set_config(httr::config(ssl_verifypeer = FALSE)) # global ignore-self-certify config

# 1: string functions ======
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

# gsub(...)
# gsub(pattern, replacement, x) 
# look for 'pattern' in 'x' and replace it with 'replacement'
urlPath <- "/{instance}/1/connect"
instance <- "woohoo"
gsub(paste0("\\{", "instance", "\\}"), `instance`, urlPath)

# 1.5: if{} else{}
# 2: return() =========
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
fse <- FastScoreError$new(message = "Unable to retrieve active sensors", 
                          caused_by = "This was caused by something.")
fse$error_string()
FastScoreError$new(message = "Unable to retrieve active sensors", 
                   caused_by = "This was caused by something.")$error_string()

# 4: InstanceBase$new()$active_sensors() =======

x <- InstanceBase$new(name = "Betsy")
InstanceBase$new(name = "Betsy")$active_sensors()
x$active_sensors()

# 5:  $tapping_points() =======
x <- InstanceBase$new(name = "Besty")
x$tapping_points()
InstanceBase$new(
  
)$tapping_points()


# 6:  $install_sensor() ====
b <- InstanceBase$new(name = "Betsy")
name
b$name
# 6.1: InstanceBase$new ========
# api_cli <- InstanceBase$new(name = "FS") # don't need this; see 7

# 6.2: parse_url(), build_url() ======
httr::parse_url("https://localhost:15080/api/1/service")
  # $scheme
  # [1] "https"
  # $hostname
  # [1] "localhost"
  # $port
  # [1] "15080"
  # $path
  # [1] "api/1/service"
A <- httr::parse_url("https://localhost/api/1/service")
B <- httr::parse_url("https://localhost:15080")


A$port <- B$port
httr::build_url(A)
# 7: Connect$new() =====
#     Proxy prefix = https://localhost:15080
#     Base path = api/1/service

httr::set_config(httr::config(ssl_verifypeer = FALSE))

con <- fastscore::Connect$new(proxy_prefix = "https://localhost:15080")

  A <- parse_url(con$apiClient$basePath) # [1] "https://localhost/api/1/service"
  B <- parse_url(con$proxy_prefix) # [1] "https://localhost:15080"
  A$port <- B$port
  con$apiClient$basePath <- build_url(A)
  
  health <- con$health_get(instance = "connect")




# 7.1 missing() ====
  
  # Test 1
  test <- function(A, B){
    if (missing(A)) {A <- B}
    A + B
  }
  
  test(3, 4)
  test(B = 4)
  
  # Test 2
  test <- function(A = NA, B = NA){
    if (missing(A)) {A <- B}
    A + B
  }
  
  test(3, 4)
  test(B = 4)
  
  # Test 3
  test <- function(A = NULL, B = NA){
    if (missing(A)) {A <- B}
    A + B
  }
  
  test(3, 4)
  test(B = 4)
  
# 8: Client API + Connect API ======

# Client API
api_cli <- InstanceBase$new() # Option 1
# api_cli <- InstanceBase$new(basePath = "https://localhost:15080/api/1/service") # Option 2
  
# Connect API
con <- fastscore::Connect$new(apiClient = api_cli, proxy_prefix = "https://localhost:15080") # Use previously instantiated client API
# con <- fastscore::Connect$new(proxy_prefix = "https://localhost:15080") # No previously instantiated client API
  
# 9: Connect$fleet ======
InstanceBase$new(basePath = "http://crap") # TODO: NEED TO MAKE THIS THROW FS ERROR

api <- fastscore::InstanceBase$new(basePath = "https://localhost:15080")
con <- fastscore::Connect$new(apiClient = api)
  con$basePath 
  con_get <- con$fleet(instance = "connect")

# 9.01 Connect$fleet  Garbage =======
# swagger::ConnectAPI$connect_get() excerpt ====== #
  urlPath <- "/{instance}/1/connect"
  if (!missing(`instance`)) {
    urlPath <- gsub(paste0("\\{", "instance", "\\}"), `instance`, urlPath)
  }
  resp <- self$apiClient$callApi(url = paste0(self$apiClient$basePath, urlPath), method = "GET", ...)

# Testing ======== # 
  instance <- "connect"
  urlPath <- "/{instance}/1/connect"
  urlPath <- gsub(paste0("\\{", "instance", "\\}"), `instance`, urlPath); urlPath
  paste0(con$apiClient$basePath, urlPath) # [1] "https://localhost:15080/connect/1/connect"
  
# 10: stopifnot() ======
stopifnot(FALSE)
stop(
     FastScoreError$new(
       message = "basePath must use HTTPS scheme, e.g. https://dashboard:8000")$error_string()
     )

# 11: ModelManage$new() =====
api <- fastscore::InstanceBase$new(basePath = "https://localhost:15080")
modman <- fastscore::ModelManage$new(apiClient = api)  
  modman$model_list(instance = "model-manage-1")
  