library(readr)
library(ade4)
library(Information)
library("pROC")

# Prudential's code ====
#read data
df <- readr::read_csv("./model/Full_dataset_training.csv", col_names = T)
df<-cbind(df,acm.disjonctif(
  data.frame(df[,c("S","T","U","Y","DD","EE","FF","KK")]))
  )
df<-df[,!names(df) %in% c("S","T","U","Y","DD","EE","FF","KK","B","ID")]

#build model
infoTables <- create_infotables(data = df,y =c("RESPONSE"),bins = 10,parallel = T)
Ivsummary <- infoTables$Summary[order(-infoTables$Summary$IV), ]
Ivsummary<-Ivsummary[which(Ivsummary$IV>0.02 & Ivsummary$IV<0.5),]
df<-df[,names(df) %in% c("RESPONSE",Ivsummary$Variable)]

# remove missing values
df <- na.omit(df)

#build random forest
# model_rf <- randomForest(
#   as.factor(RESPONSE) ~ ., data=df, importance=TRUE, ntree=50
#   )

# save(model_rf, file = "model_rf.rda")

#predict score
df$prediction <- predict(model_rf, df, OOB=TRUE, type = "response")

summary(df$prediction)

# Go-Joe! ======
# readr::write_csv(df_rf, "df_rf.csv")
# df_rf <- read_csv("df_rf.csv")

df_rf <- df; rm(df, infoTables, Ivsummary)

# remove "." from variable names ======= 
t <- data.frame(name = names(df_rf))
gsub("\\.", "_", "avg.low.speed.engine.rpm") # e.g.
t <- t %>%
  mutate(name = gsub("\\.", "_", name))
names(df_rf) <- t$name; rm(t)

# readr::write_csv(df_rf, "df_rf.csv")
df_rf <- readr::read_csv("df_rf.csv")

# refit model with modified variable names =====

model_rf <- randomForest::randomForest(
  as.factor(RESPONSE) ~ ., data=df_rf, importance=TRUE, ntree=50
)

# save(model_rf, file = "model_rf.rda")

#  schema  ======= ======= 
fields <- data.frame(name = names(df_rf), type = "double")
fields <- jsonlite::toJSON(fields)
fields


# Liftoff =======
load("demo2/model_rf.rda")
df_rf <- read.csv("demo2/df_rf.csv")
preds <- as.numeric(as.character(
  predict(model_rf, df_rf, OOB = TRUE, type = "response")
  )) # type = "prob" is nice

# LIME =====
# http://topepo.github.io/caret/train-models-by-tag.html#random-forest
# Random Forest
# method = 'rf'
# Type: Classification, Regression
# Tuning parameters:
#   mtry (#Randomly Selected Predictors)
#     Required packages: randomForest
#     A model-specific variable importance metric is available.

?lime

x <- Sys.time()
rf_fit <- caret::train(
  as.factor(RESPONSE) ~ ., data=df_rf[1:6000,], method = "rf", 
  importance=TRUE, ntree=50
  )
Sys.time() - x
# n = 100: 1.3 secs
# n = 1000: 10.7 secs
# n = 2000: 40 secs
# n = 4000: 3 mins
# n = 6000: 7 mins


explainer <- lime(df_rf, rf_fit)
explanation <- explain(df_rf[1,], explainer = explainer, 
                       n_labels = 1, n_features = 6)

plot_features(explanation)
