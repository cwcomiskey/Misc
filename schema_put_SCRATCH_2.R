d <- data.frame(x = c(1,2,3), y = c(4, 5, 6))
d <- ravro::avro_make_schema(d, name = "my_schema")
d <- rjson::toJSON(d)
cat(d)
is.character(d)

# =========
new_schema <- Schema$new(name = "my_schema",
                         model_manage = mod_man,
                         source = t
)

# what 'fastscore' and 'swagger' use =======
callApi = function(url,
                   method,
                   queryParams,
                   headerParams, # **************************
                   body, ...){
  headers <- httr::add_headers(headerParams) # **************
  httr::PUT(url, queryParams, headers, body = body, ...)}

# manual 'fastscore'/'swagger' schema_put =========
instance = "model-manage-1"
schema = "new_schema"
source = new_schema$source
  args <- list(...)
  queryParams <- list()
  headerParams <- character() # *****************************
      body <- rjson::toJSON(x)
      # body <- jsonlite::toJSON(source)
  urlPath <- "/{instance}/1/schema/{schema}"
  urlPath <- gsub(paste0("\\{", "instance", "\\}"), `instance`, urlPath)
  urlPath <- gsub(paste0("\\{", "schema", "\\}"), `schema`, urlPath)

  #  resp <-
    mod_man$apiClient$callApi(
    url = paste0(mod_man$apiClient$basePath, urlPath),
    method = "PUT",
    queryParams = queryParams,
    headerParams = headerParams, # ***************************
    body = body)
# }
