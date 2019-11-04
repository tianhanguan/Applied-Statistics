rm(list=ls(all=TRUE))
require(foreign)  
setwd('D:/CHL7001/Final Project')
################################################################################3
###Section 1: Create and aggregate the required datasets
##Questionnaire data (PHQ-9) - Mental Health - Depression Screener (DPQ_G)
DPQ1112=read.xport("DPQ1112.xpt") 

dim(DPQ1112) #dimension of the data
colnames(DPQ1112) #obtain the column names
str(DPQ1112) #check the class/data type of each column/feature

#Ignore the response of the 10th question
DPQ1112 = subset(DPQ1112,select=-DPQ100)

#Remove rows which have SEQN missing
DPQ1112 = DPQ1112[is.na(DPQ1112[,1]) != 1, ]

#If the response is refused or don't know, treat it as NA
DPQ1112[DPQ1112==7 | DPQ1112==9] <- NA 

#Remove rows which have all NAs in the response
DPQ1112 = DPQ1112[rowSums(is.na(DPQ1112[,2:10])) != 9, ]

#PHQ9 score = sum of the scores of the first 9 questions
PHQ9 = rowSums(DPQ1112[2:10], na.rm = TRUE)

#Categorize each person into No or mild depression OR Moderate to severe depression
#based on the PHQ9 score
Depression = NULL
for (i in 1:length(PHQ9)){
  if (PHQ9[i] <= 9) {
    Depression[i] = 1 #No or mild
  }
  else {
    Depression[i] = 2 #Moderate to severe
  }
}

#The final dataset will only be composed of the Sequence Number and Depression status
DPQ = as.data.frame(cbind(DPQ1112[,1],Depression))
colnames(DPQ) = c('SEQN','Depression')
#######################
##Demographics data - Demographic Variables & Sample Weights (DEMO_G)
DEMO1112=read.xport("DEMO1112.xpt") 

dim(DEMO1112) #dimension of the data:9107 21
colnames(DEMO1112) #obtain the column names: important ones are SEQN, RIDAGEYR(age),RIAGENDR(gender), DMDEDUC2(education
                   #level of age20+), RIDRETH1(race), INDFMPIR(income-poverty ratio)
str(DEMO1112) #check the class/data type of each column/feature

#First, filter out individuals with age<20 years
DEMO1112 = DEMO1112[which(DEMO1112$RIDAGEYR>=20),]

#Select only the columns needed 
DEMO1112 = subset(DEMO1112,select=c(SEQN,RIDAGEYR,RIAGENDR,DMDEDUC2,RIDRETH1,INDFMPIR))

#Create the age variable based on different categories
age = NULL
for (i in 1:dim(DEMO1112)[1]){
  if (DEMO1112$RIDAGEYR[i] <= 35) {
    age[i] = 1 #20-35 years
  }
  else if (DEMO1112$RIDAGEYR[i] <= 50) {
    age[i] = 2 #36-50 years
  }
  else if (DEMO1112$RIDAGEYR[i] <= 65) {
    age[i] = 3 #51-65 years
  }
  else if (DEMO1112$RIDAGEYR[i] > 65) {
    age[i] = 4 #>65 years
  }
  else {
    age[i] =  NA
  }
}

#Create the educationa variable
education = NULL
for (i in 1:dim(DEMO1112)[1]){
  if (DEMO1112$DMDEDUC2[i] == 1 | DEMO1112$DMDEDUC2[i] == 2) {
    education[i] = 1 #Less than high school/GED
  }
  else if (DEMO1112$DMDEDUC2[i] == 3 | DEMO1112$DMDEDUC2[i] == 4 | DEMO1112$DMDEDUC2[i] == 5) {
    education[i] = 2 #High school graduate or more
  }
  else {
    education[i] = NA
  }
}

#Create the race variable
race = NULL
for (i in 1:dim(DEMO1112)[1]){
  if (DEMO1112$RIDRETH1[i] == 3) {
    race[i] = 1 #Non-Hispanic White
  }
  else if (DEMO1112$RIDRETH1[i] == 4) {
    race[i] = 2 #Non-Hispanic Black
  }
  else if (DEMO1112$RIDRETH1[i] == 2) {
    race[i] = 3 #Hispanic
  }
  else if (DEMO1112$RIDRETH1[i] == 1 | DEMO1112$RIDRETH1[i] == 5) {
    race[i] = 4 #Other race
  }
  else {
    race[i] = NA
  }
}

#Create the poverty variable 
poverty = NULL
for (i in 1:dim(DEMO1112)[1]) {
  if (is.na(DEMO1112$INDFMPIR[i]) ==  TRUE) {
    poverty[i] = NA
  }
  else if (DEMO1112$INDFMPIR[i] < 1) {
    poverty[i] = 1 #Yes
  }
  else {
    poverty[i] = 2 #No
  }
}

#Combine the above datasets into a single dataset
DEMO = as.data.frame(cbind(DEMO1112$SEQN,DEMO1112$RIAGENDR,age,education,race,poverty))
colnames(DEMO) =  c('SEQN','Gender','Age','Education','Race','Poverty')

#Filter out individuals without complete information (i.e. one or more NAs)
DEMO = DEMO[complete.cases(DEMO),]
#######################
##Examination data - Body Measures (BMX_G)
BMX1112=read.xport("BMX1112.xpt") ### bmi 

dim(BMX1112) #dimension of the data:9107 21
colnames(BMX1112) #obtain the column names: important ones are SEQN, PAQ710, PAQ715
str(BMX1112) #check the class/data type of each column/feature

#Select only the SEQN and BMXBMI columns 
BMX1112 = subset(BMX1112,select=c(SEQN,BMXBMI))

#Exclude individuals who do not have complete responses 
BMX1112 = BMX1112[complete.cases(BMX1112), ]

#Categorize each individual into 1 of the 4 BMI categories
BMI_Group = NULL
for (i in 1:dim(BMX1112)[1]){
  if (BMX1112[i,2] < 18.5) {
    BMI_Group[i] = 1 #Underweight
  }
  else if (BMX1112[i,2] < 24.9) {
    BMI_Group[i] = 2 #Normal weight
  }
  else if (BMX1112[i,2] < 29.9) {
    BMI_Group[i] = 3 #Overweight
  }
  else {
    BMI_Group[i] = 4 #Obese
  }
}

#The final dataset will only be composed of the Sequence Number and BMI_Group
BMI = as.data.frame(cbind(BMX1112$SEQN,BMI_Group))
colnames(BMI) = c('SEQN','BMI')
#######################
##Questionnaire data - Physical Activity (PAQ_G)
PAQ1112=read.xport("PAQ1112.xpt") ### tv watching/computer use

dim(PAQ1112) #dimension of the data:9107 21
colnames(PAQ1112) #obtain the column names: important ones are SEQN, PAQ710, PAQ715
str(PAQ1112) #check the class/data type of each column/feature

#Select only the SEQN, PAQ710, and PAQ715 columns 
PAQ1112 = subset(PAQ1112,select=c(SEQN,PAQ710,PAQ715))

#If the response is don't watch TV/computer or refused or don't know, treat it as NA
PAQ1112[PAQ1112==8 | PAQ1112==77 | PAQ1112==99] <- NA 

#Exclude individuals who do not have complete responses for the 2 questions (i.e. have one or more NAs)
PAQ1112 = PAQ1112[complete.cases(PAQ1112), ]

#calculate the screen score = sum of the scores of the 2 questions
screen_score = rowSums(PAQ1112[2:3])

#Categorize each person into Low screen time OR High screen time based on the screen score
screen_time = NULL
for (i in 1:length(screen_score)) {
  if (screen_score[i] <= 4) {
    screen_time[i] = 1 #Low
  }
  else {
    screen_time[i] = 2 #High
  }
}

#The final dataset will only be composed of the Sequence Number and Screen_time
PAQ = as.data.frame(cbind(PAQ1112$SEQN,screen_time))
colnames(PAQ) = c('SEQN','Screen_time')
############################################
#Now, merge all datasets together by SEQN
dat1 = merge(DPQ,DEMO, by="SEQN") 
dat2 = merge(dat1,BMI, by="SEQN")
dat = merge(dat2,PAQ, by="SEQN")
dim(dat) #2970 data, 8 variables

#Obtain the survey sampling weights for each of the individual 
#of the selected sample dataset
DEMO1112=read.xport("DEMO1112.xpt") 
weights = subset(DEMO1112,select=c(SEQN,SDMVPSU,SDMVSTRA,WTINT2YR,WTMEC2YR))

#Append the sampling weights to the created dataset
dat = merge(dat,weights, by='SEQN')

#convert all 8 variables from numeric type to factor type
for (i in 2:11) {
  dat[,i] = as.factor(dat[,i])
}
str(dat)

save(dat,file="data.Rda") #save the merged dataset in the local drive
#############################################################################
#############################################################################

