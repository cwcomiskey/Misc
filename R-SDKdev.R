# Working on R-SDK built on swagger-codegen generated client

# Package 'fastscore' dependencies: rjson, yaml, httr

# devtools ======
# devtools::document() # namespace for codegen packages

install.packages("roxygen2"); library(roxygen2); install.packages("devtools"); library(devtools)

devtools::install("./R-SDK2.0/swagger") # 

library(help = "swagger") 

# codegen w/ CLI ==============
# CLI: java -jar swagger-codegen-cli-2.3.0.jar help generate

# CLI: java -jar swagger-codegen-cli-2.3.0.jar generate -i fastscore-api-specs/suite-proxy-v1.yaml -l r

# Stable Version, swagger-codegen-2.2.3:
# CLI: swagger-codegen generate -i fastscore-api-specs/suite-proxy-v1.yaml -l python

# ignore self-certify  ========
# Had to use, from ApiClient class:
# httr::GET(url = paste0(basePath, urlPath),
#           config = httr::config(ssl_verifypeer = FALSE))
# ...inside of ApiClient$ConnectApi$health_get
# ...to ignore "self certify" 

httr::set_config(httr::config(ssl_verifypeer = FALSE)) # global ignore-self-certify config


# ApiClient$new(...) =======
# R client for FS API -- api_cli = instance of R6 class 'ApiClient'
api_cli <- swagger::ApiClient$new(
  basePath = "https://localhost:15080/api/1/service") 


# ConnectApi$new(...) ======
# R6 'ConnectApi' class instance, (parent class = 'ApiClient', instance = 'swg')
swg <- swagger::ConnectApi$new(apiClient = api_cli) 
  health <- swg$health_get(instance = "connect") # instance == FS_service
  con_get <- swg$connect_get(instance = "connect")

# ModelManageApi$new(...) =======
# Instance of R6 class 'ModelManageApi', with parent class/instance = 'ApiClient'/'api_cli'
  # instantiating class requires "parent class" argument
modman <- swagger::ModelManageApi$new(apiClient = api_cli) 
  
  modlist <- modman$model_list(instance = "model-manage-1") 
    modlist$content # [1] "hello-world"
    modman$model_get(instance = "model-manage-1", model = "hello-world") 
    modman$stream_get(instance = "model-manage-1", stream = "rest-in")

  strmlist <- modman$stream_list(instance = "model-manage-1") 
    strmlist$content # [1] "demo-1"   "demo-2"   "rest-in"  "rest-out"

# EngingeApi$new(...) =========
eng <- swagger::EngineApi$new(apiClient = api_cli)

# Old naming (saved just in case!) ======
# ApiClient$new(...) 
# ConnectApi$new(...) 
# ModelManageApi$new(...) 
# EngingeApi$new(...) 