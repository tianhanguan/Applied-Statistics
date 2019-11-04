setwd('D:/CHL7001/Final Project')
load('data.rda')
library(ggplot2)

### Exploratory Data Analysis of the dataset
#Selection 1: Relationship between explanatory and response variable
#Barplots showing the distribution of factors within each variable
colour_plate = c('skyblue','goldenrod','dodgerblue','coral',
                 'chartreuse','brown','cyan','cornsilk')
ggplot(dat, aes(x=Depression)) +
    geom_bar(fill=colour_plate[1],colour='black') +
    ggtitle('Depression status') 

ggplot(dat, aes(x=Age)) +
  geom_bar(fill=colour_plate[2],colour='black') +
  ggtitle('Age group') 

ggplot(dat, aes(x=Gender)) +
  geom_bar(fill=colour_plate[3],colour='black') +
  ggtitle('Gender') 

ggplot(dat, aes(x=Education)) +
  geom_bar(fill=colour_plate[4],colour='black') +
  ggtitle('Education group') 

ggplot(dat, aes(x=Race)) +
  geom_bar(fill=colour_plate[5],colour='black') +
  ggtitle('Race category') 

ggplot(dat, aes(x=Poverty)) +
  geom_bar(fill=colour_plate[6],colour='black') +
  ggtitle('Poverty') 

ggplot(dat, aes(x=BMI)) +
  geom_bar(fill=colour_plate[7],colour='black') +
  ggtitle('BMI category') 

ggplot(dat, aes(x=Screen_time)) +
  geom_bar(fill=colour_plate[8],colour='black') +
  ggtitle('Screen time') 

#Stacked barplots showing the association between each of the explanatory variable 
#and the response variable (Depression):
ggplot(dat, aes(x=Depression,y=Gender,fill=Gender)) +
  geom_bar(stat='identity') +
  ggtitle('Gender vs Depression') 

ggplot(dat, aes(x=Depression,y=Age,fill=Age)) +
  geom_bar(stat='identity') +
  ggtitle('Age vs Depression') 

ggplot(dat, aes(x=Depression,y=Education,fill=Education)) +
  geom_bar(stat='identity') +
  ggtitle('Education vs Depression') 

ggplot(dat, aes(x=Depression,y=Race,fill=Race)) +
  geom_bar(stat='identity') +
  ggtitle('Race vs Depression') 

ggplot(dat, aes(x=Depression,y=Poverty,fill=Poverty)) +
  geom_bar(stat='identity') +
  ggtitle('Poverty vs Depression')

ggplot(dat, aes(x=Depression,y=BMI,fill=BMI)) +
  geom_bar(stat='identity') +
  ggtitle('BMI vs Depression')

ggplot(dat, aes(x=Depression,y=Screen_time,fill=Screen_time)) +
  geom_bar(stat='identity') +
  ggtitle('Screen_time vs Depression')

#Relationship between each variable and screen time
#create a second dataset that also includes total screen hours
screen_hours = as.data.frame(cbind(PAQ1112$SEQN,screen_score))
colnames(screen_hours) = c('SEQN','screen_hours')
dat2 =  merge(dat,screen_hours, by='SEQN')

ggplot(dat2, aes(x = screen_hours, fill = Gender)) +
  geom_density(alpha = .3) + ggtitle('Density of screen hours per gender')

ggplot(dat2, aes(x = screen_hours, fill = Education)) +
  geom_density(alpha = .3) + ggtitle('Density of screen hours per education level')

ggplot(dat2, aes(x = screen_hours, fill = Age)) +
  geom_density(alpha = .3) + ggtitle('Density of screen hours per age group')

ggplot(dat2, aes(x = screen_hours, fill = Race)) +
  geom_density(alpha = .3) + ggtitle('Density of screen hours per race group')

ggplot(dat2, aes(x = screen_hours, fill = BMI)) +
  geom_density(alpha = .3) + ggtitle('Density of screen hours per BMI group')

ggplot(dat2, aes(x = screen_hours, fill = Poverty)) +
  geom_density(alpha = .3) + ggtitle('Density of screen hours per poverty group')