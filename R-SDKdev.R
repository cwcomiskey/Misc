# Working on R-SDK built on swagger-codegen generated client

# Package 'fastscore' dependencies: rjson, yaml, httr

# devtools ======
# devtools::document() # namespace for codegen packages

install.packages("roxygen2"); library(roxygen2); install.packages("devtools"); library(devtools)

devtools::install(".") # load home directory package
devtools::install("./swagger") # 

library(help = "swagger") 

# codegen w/ CLI ==============
# CLI: java -jar swagger-codegen-cli-2.3.0.jar help generate

# CLI: java -jar swagger-codegen-cli-2.3.0.jar generate -i fastscore-api-specs/suite-proxy-v1.yaml -l r

# Stable Version, swagger-codegen-2.2.3:
# CLI: swagger-codegen generate -i fastscore-api-specs/suite-proxy-v1.yaml -l python

# API Client  ========
# Had to use, from ApiClient class:
# httr::GET(url = paste0(basePath, urlPath),
#           config = httr::config(ssl_verifypeer = FALSE))
# ...inside of ApiClient$ConnectApi$health_get
# ...to ignore "self certify" 

httr::set_config(httr::config(ssl_verifypeer = FALSE)) # global ignore-self-certify config

# ApiClient$new(...) =======
# R client for FS API -- swg = instance of R6 class 'ApiClient'
    # NOTICE: ** $new **
swg <- swagger::ApiClient$new(basePath = "https://localhost:15080/api/1/service")
    
# ConnectApi$new(...) ======
# R6 'ConnectApi' class instance, (parent class = 'ApiClient', instance = 'swg')
  con <- swagger::ConnectApi$new(apiClient = swg) # NOTICE: ** $new **
        health <- con$health_get(instance = "connect") # instance == parameter (service)
        con_get <- con$connect_get(instance = "connect")

# ModelManageApi$new(...) =======
# Instance of R6 'ModelManageApi' class, with parent class/instance = 'ApiClient'/'swg'
  # NOTICE: ** $new **
  modman <- swagger::ModelManageApi$new(apiClient = swg) # instantiate class req's parent class arg
        modlist <- modman$model_list(instance = "model-manage-1") 
              modlist$content # [1] "hello-world"
              modman$model_get(instance = "model-manage-1", model = "hello-world") # ******
              stream_test <- modman$stream_get(instance = "model-manage-1", stream = "rest-in")
               
      
        strmlist <- modman$stream_list(instance = "model-manage-1") 
              strmlist$content # [1] "demo-1"   "demo-2"   "rest-in"  "rest-out"

# EngingeApi$new(...) =========
  eng <- swagger::EngineApi$new(apiClient = swg)
