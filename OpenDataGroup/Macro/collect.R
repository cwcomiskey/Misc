# items <- ELI

collect <- function(){
  
  for(i in 1:dim(items)[1]){
    if(i == 1) dat <- data.frame()
    
    dat_i <- tryCatch(
      grab(items[i,1]),
      error = function(c){
        data.frame(itemId = i, 
                   name = items[i,1], 
                   salePrice = paste(c), 
                   categoryPath = NA, 
                   productUrl = NA, 
                   offerType = NA)
      }
    )
    
    print(dim(dat_i))
    
    print(i)
    
    dat <- rbind.data.frame(dat, cbind.data.frame(today(), items[i,1], dat_i))
    
    print(dim(dat))
    
  }
  
  return(dat)
  
}
# dat <- collect()


