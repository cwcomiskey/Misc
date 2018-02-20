# Scratch

httr::set_config(httr::config(ssl_verifypeer = FALSE)) # global ignore-self-certify config
devtools::document()

# 1: strings ======
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

instance <- "connect"
urlPath <- "/{instance}/1/connect"
urlPath <- gsub(paste0("\\{", "instance", "\\}"), `instance`, urlPath); urlPath
paste0(con$apiClient$basePath, urlPath) # [1] "https://localhost:15080/connect/1/connect"


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
  
# 7.3: stopifnot() ======
stopifnot(FALSE)
stop(
     FastScoreError$new(
       message = "basePath must use HTTPS scheme, e.g. https://dashboard:8000")$error_string()
     )

# 7.4: function(){} w/ tryCatch() w/ stop() ======
  t3 <- function(){
    tryCatch(
      xx <- mean(rnorm(a2)), # swagger::connect_get()
      error = function(e) stop("Cannot retrieve fleet info, ", e)
    )
    print(xx)
  }
  t3()
# 8: Client API + Connect API ======

# Client API
api_cli <- InstanceBase$new() # Option 1
# api_cli <- InstanceBase$new(basePath = "https://localhost:15080/api/1/service") # Option 2
  
# Connect API
con <- fastscore::Connect$new(apiClient = api_cli, proxy_prefix = "https://localhost:15080") # Use previously instantiated client API
# con <- fastscore::Connect$new(proxy_prefix = "https://localhost:15080") # No previously instantiated client API
  
# Dev: InstanceBase$new() ======
InstanceBase$new(basePath = "http://crap") # TODO: NEED THIS TO THROW FS ERROR

api <- fastscore::InstanceBase$new(basePath = "https://localhost:15080")

# Dev: Connect$new() =====
con <- fastscore::Connect$new(apiClient = api)
  con$basePath 
  con_get <- con$connect_get(instance = "connect")
  con_get <- con$fleet(instance = "connect")

# Dev: ModelManage$new() =======
api <- fastscore::InstanceBase$new(basePath = "https://localhost:15080")
con <- fastscore::Connect$new(apiClient = api)
mod_man <- fastscore::ModelManage$new(apiClient = api)  
  
# Dev: list/get model =====
  mod_man$model_list(instance = "model-manage-1")$content 
  
  hw_mod <- mod_man$model_get(instance = "model-manage-1", model = "hello-world") # get 'hello-world' model
    hw_mod
    hw_mod$response
    http_type(hw_mod$response) # [1] "application/vnd.fastscore.model-python"
    hw_mod$response$headers$`content-type` # same
    hw_mod$content
    cat(hw_mod$content) # 'hello-world' model
    
# Dev: create/add/delete model ======    

# Create fastscore model string
my_mod_string <- "
# fastscore.input: my_mod_input
# fastscore.output: my_mod_output

fs_mod_fun <- function(x){x + 1}
"

cat(my_mod_string)

my_mod <- Model$new(
  name = "my_mod",
  mtype = 'r',
  source = my_mod_string,
  model_manage = mod_man
)

m_put <- mod_man$model_put(instance = "model-manage-1", 
                   model = "new_model", 
                   source = my_mod$source,
                   content_type = 'application/vnd.fastscore.model-r')    

mod_man$model_delete(instance = "model-manage-1",
                     model = "new_model") # success!!


    
# Dev: get/list schemas ======
  mod_man$schema_list(instance = "model-manage-1")$content
    gbm_sch <- mod_man$schema_get(instance = "model-manage-1", schema = "gbm_input")
      gbm_sch$response
      gbm_sch$content
  
# Dev: create/add/delete schema: Schema$new() ========
api <- fastscore::InstanceBase$new(basePath = "https://localhost:15080")
con <- fastscore::Connect$new(apiClient = api)
mod_man <- fastscore::ModelManage$new(apiClient = api)  

# devtools::install_github("RevolutionAnalytics/ravro/pkg/ravro")

d <- data.frame(x = c(1,2,3), y = c(4, 5, 6))
d <- ravro::avro_make_schema(d, name = "my_schema")

new_schema <- Schema$new(name = "my_schema", 
   model_manage = mod_man,
   source = d
   )

# str(new_schema$source)
# cat(toJSON(new_schema$source))

mod_man$schema_put(instance = "model-manage-1", 
                   schema = "new_schema", 
                   source = d,
                   content_type('application/json'))

# mod_man$schema_list(instance = "model-manage-1")$content

mod_man$schema_delete(instance = "model-manage-1",
                      schema = "new_schema") # NULL = successfully deleted

# mod_man$schema_list(instance = "model-manage-1")$content

# To-do list ======
# "Here's what you need to be able to do in Model Manage:"
# 
# There are four types of assets (models, streams, schemas, and sensors)
# For each type:
# 1. List the names of all assets of that type currently in model manage.
# 2. Retrieve an object of that type from Model Manage by name.
# 3. Create an object of that type and add it to Model Manage.
# 4. Delete an object of that type by name from Model Manage.


# Dev: streams ======
mod_man$stream_get(instance = "model-manage-1", stream = "rest-in")$content
mod_man$stream_list(instance = "model-manage-1")$content  


