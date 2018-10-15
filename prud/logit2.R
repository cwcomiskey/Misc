library(readr)
library(ade4)
library(Information)
library("pROC")

# Prudential's code =====
df <- read_csv("Full_dataset_training.csv", col_names = T)
df <- cbind(df, acm.disjonctif(data.frame(df[,c("S","T","U","Y","DD","EE","FF","KK")])))
df <- df[,!names(df) %in% c("S","T","U","Y","DD","EE","FF","KK","B","ID")]

infoTables <- create_infotables(data = df,y =c("RESPONSE"),bins = 10,parallel = T)
Ivsummary <- infoTables$Summary[order(-infoTables$Summary$IV), ]
Ivsummary <- Ivsummary[which(Ivsummary$IV>0.3 & Ivsummary$IV<0.5),]
df <- df[,names(df) %in% c("RESPONSE",Ivsummary$Variable)]

fullmod <- glm(RESPONSE ~ . , data= df, family = "binomial")  # base intercept only model
model.lg <- step(fullmod)  # perform step-wise algorithm

#final model
summary(model.lg)

#predict score
df$p_predict <- predict(model.lg, newdata=df, type="response")

# Go-Joe! ======
# readr::write_csv(df, "df.csv")
df <- read.csv("df.csv")

load("model.rda")
preds <- as.numeric(predict(
  model.lg, newdata = df, type = "response")
  ) # checks out
