# ADD SCHEMA ===============================================
api.add_schema <- function(schema_name,
                           schema_content){

  ctype <- 'application/json'
  service.put(name = 'model-manage',
              path = paste('/1/schema/', schema_name, sep=''),
              ctype = ctype,
              data = schema_content)
}

# CALLED PUT FUNCTION =====================================
service.put <- function(name,
                        path,
                        ctype,
                        data,
                        generic=TRUE,
                        preferred=list()){
  PUT(paste(lookup(name, generic, preferred),
            path,
            sep=''),
      add_headers('Content-Type' = ctype),
      body = data
  )
}

# APPLIED ===========================================
PUT(
  url = paste(lookup('model-manage', TRUE, list()),
              paste('/1/schema/', schema_name, sep=''),
              sep=''),
  add_headers('Content-Type' = 'application/json'),
  body = schema_content
  )

