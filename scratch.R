# Scratch

# 1 ======
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


# 2 =========
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
  return(d)
}
t2()

# 3: FastScoreError$new() ======
fse <- FastScoreError$new(message = "Unable to retrieve active sensors", caused_by = "This was caused by something.")
fse$error_string()
FastScoreError$new(message = "Unable to retrieve active sensors", caused_by = "This was caused by something.")$error_string()

# 4: InstanceBase$new() --> active_sensors() =======
x <- InstanceBase$new(name = "Betsy")
InstanceBase$new(name = "Betsy")$active_sensors()
x$active_sensors()

# 5: InstanceBase$new() --> tapping_points =======
