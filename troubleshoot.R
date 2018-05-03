dummyData <- data.frame(y=1, catastrophicFlag = 1, 
                        agentTenure=2, inPatientFlag=3, agentIsTOTFlag = 4, 
                        rescindRecordFlag = 5, abnormalLengthOfStayFlag = 6, 
                        LAIsNonOwnerFlag = 7, paidAmount=8)

model <- glm(formula = y ~ catastrophicFlag 
              + agentTenure + inPatientFlag 
              + agentIsTOTFlag + rescindRecordFlag 
              + abnormalLengthOfStayFlag + LAIsNonOwnerFlag + paidAmount, data=dummyData, family=binomial('logit'))

model$coefficients <- c('(Intercept)'=-5.209717064, 
                         catastrophicFlag=0.08002096405,
                         agentTenure=0.1095396855,
                         inPatientFlag=-0.9895324585,
                         agentIsTOTFlag=0.8400433255,
                         rescindRecordFlag=0.8355533958,
                         abnormalLengthOfStayFlag=0.7935252693,
                         LAIsNonOwnerFlag=-0.8266344156,
                         paidAmount=-0.8090678022)

x1 <- data.frame(catastrophicFlag = 0, 
                 agentTenure=0, inPatientFlag=0, agentIsTOTFlag = 0, 
                 rescindRecordFlag = 0, abnormalLengthOfStayFlag = 0, 
                 LAIsNonOwnerFlag = 0, paidAmount=0)

predict(model, x1, type='response')

x2 <- data.frame(catastrophicFlag = 1, 
                 agentTenure=10, inPatientFlag=1, agentIsTOTFlag = 1, 
                 rescindRecordFlag = 1, abnormalLengthOfStayFlag = 1, 
                 LAIsNonOwnerFlag = 1, paidAmount=0)

predict(model, x2, type = 'response')
