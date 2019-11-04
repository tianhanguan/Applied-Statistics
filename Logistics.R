setwd('D:/CHL7001/Final Project')
load('data.rda')
require(foreign) 
library(survey)
library(dplyr)

##Univariate analysis
#1. Screen_time
model11 = svyglm(as.numeric(Depression==2) ~
                        Screen_time, design = design1, family=quasibinomial())
summary(model11)

#Calculate the unadjusted OR
m11 = model11$coefficients[2]
se11 = 0.1739
OR11 = exp(m11)
OR11

#Construct the 95% CI for the unadjusted OR
cat("  95% C.I.:  (", exp(m11-1.96*se11), ",", exp(m11+1.96*se11), ")\n", sep='')
###################
#2. Age
model12 = svyglm(as.numeric(Depression==2) ~
                   Age, design = design1, family=quasibinomial())
summary(model12) 
#Noted that age is not significant 
###################
#3. Gender
model13 = svyglm(as.numeric(Depression==2) ~
                   Gender, design = design1, family=quasibinomial())
summary(model13)

#Calculate the unadjusted OR
m13 = model13$coefficients[2]
se13 = 0.1240
OR13 = exp(m13)
OR13

#Construct the 95% CI for the unadjusted OR
cat("  95% C.I.:  (", exp(m13-1.96*se13), ",", exp(m13+1.96*se13), ")\n", sep='')
###################
#4. Education level
model14 = svyglm(as.numeric(Depression==2) ~
                   Education, design = design1, family=quasibinomial())
summary(model14)

#Calculate the unadjusted OR
m14 = model14$coefficients[2]
se14 = 0.2380
OR14 = exp(m14)
OR14

#Construct the 95% CI for the unadjusted OR
cat("  95% C.I.:  (", exp(m14-1.96*se14), ",", exp(m14+1.96*se14), ")\n", sep='')
#############################################
#5. Poverty level
model15 = svyglm(as.numeric(Depression==2) ~
                   Poverty, design = design1, family=quasibinomial())
summary(model15)

#Calculate the unadjusted OR
m15 = model15$coefficients[2]
se15 = 0.1514
OR15 = exp(m15)
OR15

#Construct the 95% CI for the unadjusted OR
cat("  95% C.I.:  (", exp(m15-1.96*se15), ",", exp(m15+1.96*se15), ")\n", sep='')
###################
#6. BMI
#Note: for BMI, need to modify the dataset so that normal weight
#represents the reference group
BMI2 = NULL
BMI1 = as.numeric(dat$BMI)
dat2 = dat
for (i in 1:length(BMI1)) {
  if (BMI1[i] == 1) {
    BMI2[i] = 2
  }
  else if (BMI1[i] == 2) {
    BMI2[i] = 1
  }
  else {
    BMI2[i] = BMI1[i]
  }
}
dat2$BMI = as.factor(BMI2)
dat2 %>% group_by(BMI) %>% count() #check the modification
design11 = svydesign(id=~SDMVPSU,strata=~SDMVSTRA,weights=~WTMEC2YR,nest=TRUE,data=dat2) 
model16 = svyglm(as.numeric(Depression==2) ~
                   BMI, design = design11, family=quasibinomial())
summary(model16)

#Unadjusted OR for Underweight group
m161 = model16$coefficients[2]
se161 = 0.39877
OR161 = exp(m161)
OR161
#Construct the 95% CI for the unadjusted OR
cat("  95% C.I.:  (", exp(m161-1.96*se161), ",", exp(m161+1.96*se161), ")\n", sep='')

#Noted that only the underweight group is significant
#############################################
##Multivariate analysis
#Build the full model
model1 = svyglm(as.numeric(Depression==2)~
                  Gender+Education+Age+Race+
                  BMI+Poverty+Screen_time, design = design1, family=quasibinomial())
summary(model1)

#Build the reduced model
model2 = svyglm(as.numeric(Depression==2)~
                  Gender+Education+Poverty+Screen_time, design = design1, family=quasibinomial())
summary(model2)

#1. Gender
#Calculate the adjusted OR
m21 = model2$coefficients[2]
se21 = 0.1395
OR21 = exp(m21)
OR21

#Construct the 95% CI for the adjusted OR
cat("  95% C.I.:  (", exp(m21-1.96*se21), ",", exp(m21+1.96*se21), ")\n", sep='')

#2. Education level
#Calculate the adjusted OR
m22 = model2$coefficients[3]
se22 = 0.2677
OR22 = exp(m22)
OR22

#Construct the 95% CI for the adjusted OR
cat("  95% C.I.:  (", exp(m22-1.96*se22), ",", exp(m22+1.96*se22), ")\n", sep='')

#3. Poverty level
#Calculate the adjusted OR
m23 = model2$coefficients[4]
se23 = 0.1903
OR23 = exp(m23)
OR23

#Construct the 95% CI for the adjusted OR
cat("  95% C.I.:  (", exp(m23-1.96*se23), ",", exp(m23+1.96*se23), ")\n", sep='')

#4. Screen_time
#Calculate the adjusted OR
m24 = model2$coefficients[5]
se24 = 0.1840
OR24 = exp(m24)
OR24

#Construct the 95% CI for the adjusted OR
cat("  95% C.I.:  (", exp(m24-1.96*se24), ",", exp(m24+1.96*se24), ")\n", sep='')
