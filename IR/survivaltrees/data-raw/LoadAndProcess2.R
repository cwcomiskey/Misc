# LoadAndProcess2

library(survivaltrees)
data(package = "survivaltrees")

data("thermoking")
thermoking_sample_raw[1:5, c("serial", "months_in_field", "max_ambient", "event")]
thermoking_sample[1:5, c("serial", "months_in_field", "max_ambient", "time1", "time2", "AG_event")]
# thermoking[1:5, c("serial", "months_in_field", "max_ambient", "time1", "time2", "AG_event")]


# Andersen-Gill Style ======
?survivaltrees::ag_transform

ag_style <- survivaltrees::ag_transform(
  dataset = thermoking_sample_raw, 
  id = serial,  
  age = months_in_field, 
  event = event)

all.equal(thermoking_sample, ag_style)

# LTRCART(...) =====

## Fit ###
?LTRCART
# Note:
#  (i) formula, Surv(tim1, time2, event)
#  (ii) control

tree_fit <- LTRCtrees::LTRCART(
  survival::Surv(time1, time2, AG_event) ~ 
    max_engine_temp + avg_engine_temp + 
    max_engine_hoursa + min_engine_hoursa, 
  control = rpart::rpart.control(cp = 0.003), # ***
  data = as.data.frame(thermoking)[1:42000,]
  )

## Plot ###

?rpart.plot
rpart.plot::rpart.plot.version1(tree_fit, type = 0)
rpart.plot::rpart.plot.version1(tree_fit, type = 2)

rpart.plot.version1(tree_fit,
                    type = 0, extra = 1, # **
                    under = FALSE, fallen.leaves = FALSE,
                    digits = 2, varlen = -8, faclen = 3,
                    cex = NULL, tweak = 1,
                    snip = FALSE,
                    box.palette = 0, shadow.col = 0)

## Predict ###
?predict.rpart
predict(tree_fit, newdata = as.data.frame(thermoking)[42001:42981,])[1:5]

## Control parameters ======
?rpart.control

tree_fit <- LTRCtrees::LTRCART(
  survival::Surv(time1, time2, AG_event) ~ 
    max_engine_temp + avg_engine_temp + 
    max_engine_hoursa + min_engine_hoursa, 
  data = as.data.frame(thermoking)[1:42000,],
  control = rpart::rpart.control(
    cp = 0.001,              # complexity parameter
    minsplit = 20,           # min. obs. for split attempt
    minbucket = round(20/3), # term. bucket min
    maxcompete = 4,          # no. competitor splits retained
    maxsurrogate = 5,        # no. surrogate splits retained
    xval = 10,               # no. cross-validations
    surrogatestyle = 0,      # best surrogate selection
    maxdepth = 30            # max node depth
    ))
rpart.plot::rpart.plot.version1(tree_fit, type = 0)

# LTRCIT(...) ====== 

