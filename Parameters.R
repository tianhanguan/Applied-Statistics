setwd('D:/CHL7001/Final Project')
load('data.rda')
require(foreign) 
library(survey)
library(dplyr)

#Summarize the sample size grouped by study variables
dat %>% group_by(Depression) %>% count()
dat %>% group_by(Gender) %>% count()
dat %>% group_by(Education) %>% count()
dat %>% group_by(Race) %>% count()
dat %>% group_by(Age) %>% count()
dat %>% group_by(BMI) %>% count()
dat %>% group_by(Poverty) %>% count()
dat %>% group_by(Screen_time) %>% count()

###Use the survey package to estimate population parameters
##Model 1: Weighted analysis on the sampling design
#1) Build the svydesign object using the sampling design
design1 = svydesign(id=~SDMVPSU,strata=~SDMVSTRA,weights=~WTMEC2YR,nest=TRUE,data=dat) #weighted analysis

#2) Use the design object to estimate the population the based on
#the study variables as well as testing the association

#1. Gender
#estimate the parameters and make it into a nice-looking table format
var1 = svymean(~interaction(Depression,Gender), design=design1) 
table1 = ftable(var1, rownames=list(Depression=c('No or mild','Moderate or severe'),
                           Gender=c("Male",'Female')))
round(100*table1,2)

#Use Rao-Scott Chi-square test to the association between depression and the study variable
svychisq(~interaction(Depression,Gender),design1,statistic="F")

#2. Education level
var2 = svymean(~interaction(Depression,Education), design=design1) 
table2 = ftable(var2, rownames=list(Depression=c('No or mild','Moderate or severe'),Education=c("Less than high school/GED",'High school/GED equivalent or higher')))
round(100*table2,2)
svychisq(~interaction(Depression,Education),design1,statistic="F")

#3. Race
var3 = svymean(~interaction(Depression,Race), design=design1) 
table3 = ftable(var3, rownames=list(Depression=c('No or mild','Moderate or severe'),Race=c("Non-Hispanic White",'Non-Hispanic Black','Hispanic','Others')))
round(100*table3,2)
svychisq(~interaction(Depression,Race),design1,statistic="F")

#4. Age 
var4 = svymean(~interaction(Depression,Age), design=design1) 
table4 = ftable(var4, rownames=list(Depression=c('No or mild','Moderate or severe'),Age=c("20-35 years",'36-50 years','51-65 years','> 65 years')))
round(100*table4,2)
svychisq(~interaction(Depression,Age),design1,statistic="F")

#5. BMI 
var5 = svymean(~interaction(Depression,BMI), design=design1) 
table5 = ftable(var5, rownames=list(Depression=c('No or mild','Moderate or severe'),BMI=c("Underweight",'Normal','Overweight','Obese')))
round(100*table5,2)
svychisq(~interaction(Depression,BMI),design1,statistic="F")

#6. Poverty
var6 = svymean(~interaction(Depression,Poverty), design=design1) 
table6 = ftable(var6, rownames=list(Depression=c('No or mild','Moderate or severe'),Poverty=c('Yes','No')))
round(100*table6,2)
svychisq(~interaction(Depression,Poverty),design1,statistic="F")

#7. Screen_time
var7 = svymean(~interaction(Depression,Screen_time), design=design1) 
table7 = ftable(var7, rownames=list(Depression=c('No or mild','Moderate or severe'),Poverty=c('<=4hours','>4hours')))
round(100*table7,2)
svychisq(~interaction(Depression,Screen_time),design1,statistic="F")
############################################################
##Model 2: unWeighted analysis on the sampling design
#1) Build the svydesign object using the sampling design
design2 = svydesign(id=~SDMVPSU,strata=~SDMVSTRA,nest=TRUE,data=dat) #unweighted analysis: assuming equal probability

#2) Use the design object to estimate the population the based on
#the study variables as well as testing the association

#1. Gender
var1 = svymean(~interaction(Depression,Gender), design=design2) 
table1 = ftable(var1, rownames=list(Depression=c('No or mild','Moderate or severe'),
                                    Gender=c("Male",'Female')))
round(100*table1,2)
svychisq(~interaction(Depression,Gender),design2,statistic="F")

#2. Education level
var2 = svymean(~interaction(Depression,Education), design=design2) 
table2 = ftable(var2, rownames=list(Depression=c('No or mild','Moderate or severe'),Education=c("Less than high school/GED",'High school/GED equivalent or higher')))
round(100*table2,2)
svychisq(~interaction(Depression,Education),design2,statistic="F")

#3. Race
var3 = svymean(~interaction(Depression,Race), design=design2) 
table3 = ftable(var3, rownames=list(Depression=c('No or mild','Moderate or severe'),Race=c("Non-Hispanic White",'Non-Hispanic Black','Hispanic','Others')))
round(100*table3,2)
svychisq(~interaction(Depression,Race),design2,statistic="F")

#4. Age 
var4 = svymean(~interaction(Depression,Age), design=design2) 
table4 = ftable(var4, rownames=list(Depression=c('No or mild','Moderate or severe'),Age=c("20-35 years",'36-50 years','51-65 years','> 65 years')))
round(100*table4,2)
svychisq(~interaction(Depression,Age),design2,statistic="F")

#5. BMI 
var5 = svymean(~interaction(Depression,BMI), design=design2) 
table5 = ftable(var5, rownames=list(Depression=c('No or mild','Moderate or severe'),BMI=c("Underweight",'Normal','Overweight','Obese')))
round(100*table5,2)
svychisq(~interaction(Depression,BMI),design2,statistic="F")

#6. Poverty
var6 = svymean(~interaction(Depression,Poverty), design=design2) 
table6 = ftable(var6, rownames=list(Depression=c('No or mild','Moderate or severe'),Poverty=c('Yes','No')))
round(100*table6,2)
svychisq(~interaction(Depression,Poverty),design2,statistic="F")

#7. Screen_time
var7 = svymean(~interaction(Depression,Screen_time), design=design2) 
table7 = ftable(var7, rownames=list(Depression=c('No or mild','Moderate or severe'),Poverty=c('<=4hours','>4hours')))
round(100*table7,2)
svychisq(~interaction(Depression,Screen_time),design2,statistic="F")