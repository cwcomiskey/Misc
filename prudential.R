# Prudential 

library(dplyr); library(ggplot2)

# "Logistic regression model to predict customers' propensity of purchasing AL series policies"

# Prudential Model ======
# inverse logit function ==
inv_logit <- function(x){1/(1 + exp(-x))}

# matrix/vector multiplication ==
x <- matrix(c(1,2,3,4,5), nrow = 1)
B <- matrix(c(9,8,7,6,5), ncol = 1)

lin_pred <- x %*% B
x %*% B

# actual coefficients ==
B <- c(-5.209717064, 0.08002096405, 0.1095396855, 
       -0.9895324585, 0.8400433255, 0.8355533958, 
       0.7935252693, -0.8266344156, -0.8090678022)

B <- matrix(B, ncol = 1)

dimnames(B) <- list(c('Intercept', 'catastrophicFlag', 'agentTenure', 
                      'inPatientFlag', 'agentIsTOTFlag', 'rescindRecordFlag', 
                      'abnormalLengthOfStayFlag', 'LAIsNonOwnerFlag', 'paidAmount'), 'coefficients')

# Prediction ==

# Intercept == 1
# catastrophicFlag = 1, 
# agentTenure=0,          # Non-indicator
# inPatientFlag=0, 
# agentIsTOTFlag = 1, 
# rescindRecordFlag = 1, 
# abnormalLengthOfStayFlag = 1, 
# LAIsNonOwnerFlag = 1, 
# paidAmount=0            # Non-indicator

# Design matrix (with intercept) ==
# X <- matrix(c('intercept', 'catastrophic flag', 'agent tenure', ...))

X <- matrix(c(1, 1, 0, 0, 1, 1, 1, 1, 0), nrow = 1)
inv_logit(X %*% B) # [1,] 0.02967839

X_2 <- matrix(c(1,  # Intercept == 1
                1,  # catastrophicFlag = 1
                0,  # agentTenure=0 (NON-INDICATOR)
                0,  # inPatientFlag=0
                1,  # agentIsTOTFlag = 1
                1,  # rescindRecordFlag = 1
                1,  # abnormalLengthOfStayFlag = 1
                1,  # LAIsNonOwnerFlag = 1
                0), # paidAmount=0 (NON-INDICATOR) 
              nrow = 1)
inv_logit(X_2 %*% B)

# Function for plotting ==
prud <- function(cat = 1, agentT = 0, inP = 0, agentI = 1, res = 1, 
                 ab = 1, LAI = 1, paid = 0){
  
  B <- c(-5.209717064, 0.08002096405, 0.1095396855, 
         -0.9895324585, 0.8400433255, 0.8355533958, 
         0.7935252693, -0.8266344156, -0.8090678022)
  B <- matrix(B, ncol = 1)
  
  X <- matrix(c(1, cat, agentT, inP, agentI, res, ab, LAI, paid), nrow = 1)
  
  p <- inv_logit(X %*% B)

  return(p)
  
}

# Plots ==


plot_dat <- expand.grid(catastrophicFlag = c(0,1), 
                        # LAIsNonOwnerFlag = c(0,1), 
                        paidAmount = seq(-5, 2.5, length = 4), 
                        abnormalLengthOfStayFlag = c(0,1),
                        agentTenure = seq(0, 15, by = 1),
                        p = 0)


for(i in 1:dim(plot_dat)[1]){
  plot_dat[i,'p'] <- prud(cat = plot_dat[i, 'catastrophicFlag'], 
                          paid = plot_dat[i, 'paidAmount'], 
                          # LAI = plot_dat[i, 'LAIsNonOwnerFlag'],
                          ab = plot_dat[i, 'abnormalLengthOfStayFlag'],
                          agentT = plot_dat[i, 'agentTenure'])
}

ggplot(data = plot_dat, aes(x = agentTenure, y = p)) + 
  geom_line(aes(colour = as.factor(catastrophicFlag), 
                linetype = as.factor(abnormalLengthOfStayFlag)),
            size = 2) +
  facet_grid(~ paidAmount) +
  labs(colour='Catastrophic Flag',
       linetype = 'Ab. Length of Stay') +
  ggtitle("Amount Paid") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("Black", "Orange"))

ggsave("Prud.jpg", height = 7, width = 15)


# Examples from my research =====
mod.polar <- glm(hit ~ r*theta + I(r^2)*I(theta^2), 
                 family = binomial, data = hitter)

PredCI <- predict(mod.polar, 
                  newdata=hitzone, 
                  type = "link", # use type = "response" for p
                  se.fit = TRUE) 

# Linear predictor =  log[p/(1-p)] --- fits & CIs 
fit <- PredCI$fit
upper <- PredCI$fit + 1.96*PredCI$se.fit # MLE is Normally distributed
lower <- PredCI$fit - 1.96*PredCI$se.fit

# Expected value of response: E(y) = p --- fits, CIs
fit2 <- mod.polar$family$linkinv(fit)
upper2 <- mod.polar$family$linkinv(upper)
lower2 <-mod.polar$family$linkinv(lower)