# Working on R-SDK built on swagger-codegen generated client

# Package 'fastscore' dependencies: rjson, yaml, httr

# devtools ======
# devtools::document() # namespace for codegen packages

install.packages("roxygen2"); library(roxygen2); install.packages("devtools"); library(devtools)

devtools::install("./R-SDK2.0/swagger") # 

library(help = "swagger") 

# codegen w/ CLI ==============
# Find file here:
#   https://mvnrepository.com/artifact/io.swagger/swagger-codegen-cli/2.3.1

# CLI: java -jar swagger-codegen-cli-2.3.0.jar help generate

# CLI: java -jar swagger-codegen-cli-2.3.0.jar generate -i fastscore-api-specs/suite-proxy-v1.yaml -l r

# Stable Version, swagger-codegen-2.2.3:
# CLI: swagger-codegen generate -i fastscore-api-specs/suite-proxy-v1.yaml -l python

# Newest stable version
# CLI: java -jar swagger-codegen-cli-2.3.1.jar generate -i fastscore-api-specs/suite-proxy-v1.yaml -l r

# ignore self-certify  ========
# Had to use, from ApiClient class:
# httr::GET(url = paste0(basePath, urlPath),
#           config = httr::config(ssl_verifypeer = FALSE))
# ...inside of ApiClient$ConnectApi$health_get
# ...to ignore "self certify" 

httr::set_config(httr::config(ssl_verifypeer = FALSE)) # global ignore-self-certify config


# ApiClient$new(...) =======
api_cli <- swagger::ApiClient$new(
  basePath = "https://localhost:15080/api/1/service") 


# ConnectApi$new(...) ======
con <- swagger::ConnectApi$new(apiClient = api_cli)   
  con$apiClient$basePath
  con_get <- con$connect_get(instance = "connect")
  health <- con$health_get(instance = "connect") # instance == FS_service

# ModelManageApi$new(...) =======
modman <- swagger::ModelManageApi$new(apiClient = api_cli) 
  
  modman$model_list(instance = "model-manage-1") 
  modman$model_get(instance = "model-manage-1", model = "auto_gbm") # getting "lexical error"
  modman$stream_get(instance = "model-manage-1", stream = "rest-in")

  strmlist <- modman$stream_list(instance = "model-manage-1") 
    strmlist$content # [1] "demo-1"   "demo-2"   "rest-in"  "rest-out"

# EngingeApi$new(...) =========
eng <- swagger::EngineApi$new(apiClient = api_cli)
