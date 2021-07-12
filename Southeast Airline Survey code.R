#####--------#####|#####|\######|###########/\############|#########################
#####|############|#####|#\#####|##########/##\###########|#########################
#####|############|#####|##\####|#########/####\##########|#########################
#####|-------#####|#####|###\###|########/------\#########|#########################
#####|############|#####|####\##|#######/########\########|#########################
#####|############|#####|#####\#|######/##########\#######|#########################
#####|############|#####|######\|#####/############\######|___________##############
####################################################################################
dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!
#The original data stores in JSON file which is human readalbe but not able to be processed in R studio.
#At first, transformation from JSON to dataframe is necessory for which jsonlite package is libraried and 
#jsonlite::fromJSON() function is called.
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(caret)
library(kernlab)
library(e1071)
library(arules)
library(arulesViz)
library(ggmap)
library(ggrepel)
############################### Get to know data set #####################################
#######################################summary############################################
Airlinesurvey <- jsonlite::fromJSON('survey.json', flatten=TRUE)
#After importing, we need to know about our data, how does it look like and what is the structure .
View(Airlinesurvey)
str(Airlinesurvey)
summary(Airlinesurvey)
dim(Airlinesurvey)
############################### Data cleaning ############################################
##########################################################################################
#Results demonstrate that 26 characteristics are captured some of which are numerical and rest of them are catergorical.
#In this data set, lots of data are missing, column names, row names are appropriate so that it is not necessory to revise. 

#Now start with dealing with missing data.
#For catergorical variables such as Class, Type.of.Travel, it is difficult to be replaced with certain value. So I remove them directly
#Data cleaning process.
#1.Create a vector contains all rows that having missing catergorical values.
anyNA(Airlinesurvey$Destination.City)
anyNA(Airlinesurvey[,1:31])
Narow <- c(which(is.na(Airlinesurvey$Destination.City)),
           which(is.na(Airlinesurvey$Origin.City)),
           which(is.na(Airlinesurvey$Airline.Status)),
           which(is.na(Airlinesurvey$Gender)),
           which(is.na(Airlinesurvey$Type.of.Travel)),
           which(is.na(Airlinesurvey$Class)),
           which(is.na(Airlinesurvey$Flight.date)),
           which(is.na(Airlinesurvey$Partner.Code)),
           which(is.na(Airlinesurvey$Partner.Name)),
           which(is.na(Airlinesurvey$Origin.State)),
           which(is.na(Airlinesurvey$Destination.State)),
           which(is.na(Airlinesurvey$Flight.cancelled))
)
Narow<-unique(Narow)
#Airlinesurvey2<-Airlinesurvey[-duplicated(Airlinesurvey),]
#2. Remove certain rows.
Airlinesurvey <- Airlinesurvey[-Narow,]
#3.For some numerical data such as loyalty,Age,Pricesensitivy, based on observation of numerical columns in data frame, We can figure out a feasible replacement to NA.
############################### Data visualizing ############################################
#1################################histogram##################################################
hist(Airlinesurvey$Loyalty)
hist(Airlinesurvey$Flights.Per.Year)
hist(Airlinesurvey$Eating.and.Drinking.at.Airport)
hist(Airlinesurvey$Shopping.Amount.at.Airport)
hist(Airlinesurvey$Total.Freq.Flyer.Accts)
hist(Airlinesurvey$Departure.Delay.in.Minutes)
hist(Airlinesurvey$Arrival.Delay.in.Minutes)
#These histogram is right-skewed, to alleviate impact of ouliers,median is a better replacement to NA than mean
Airlinesurvey$Year.of.First.Flight[which(is.na(Airlinesurvey$Year.of.First.Flight))] <- median(Airlinesurvey$Year.of.First.Flight,na.rm = TRUE)
Airlinesurvey$Airline.Status[which(is.na(Airlinesurvey$Airline.Status))] <- median(Airlinesurvey$Loyalty,na.rm = TRUE)
Airlinesurvey$Eating.and.Drinking.at.Airport[which(is.na(Airlinesurvey$Eating.and.Drinking.at.Airport))] <- median(Airlinesurvey$Eating.and.Drinking.at.Airport,na.rm = TRUE)
Airlinesurvey$Shopping.Amount.at.Airport[which(is.na(Airlinesurvey$Shopping.Amount.at.Airport))] <- median(Airlinesurvey$Shopping.Amount.at.Airport,na.rm = TRUE)
Airlinesurvey$Total.Freq.Flyer.Accts[which(is.na(Airlinesurvey$Total.Freq.Flyer.Accts))] <- median(Airlinesurvey$Total.Freq.Flyer.Accts,na.rm = TRUE)
Airlinesurvey$Departure.Delay.in.Minutes[which(is.na(Airlinesurvey$Departure.Delay.in.Minutes))] <- median(Airlinesurvey$Departure.Delay.in.Minutes,na.rm = TRUE)
Airlinesurvey$Arrival.Delay.in.Minutes[which(is.na(Airlinesurvey$Arrival.Delay.in.Minutes))] <- median(Airlinesurvey$Arrival.Delay.in.Minutes,na.rm = TRUE)

hist(Airlinesurvey$Age)
#This histogram is symmetric so we can also use median to replace na.
Airlinesurvey$Age[which(is.na(Airlinesurvey$Age))] <- median(Airlinesurvey$Age,na.rm = TRUE)

hist(Airlinesurvey$Price.Sensitivity)
#The histogram demonstrates that about 70% people have 1, so we replace na with 1, also the mode.
Airlinesurvey$Price.Sensitivity[which(is.na(Airlinesurvey$Price.Sensitivity))] <- 1

#4. For the rest of numerical columns:likelihood, variables related to time and position like latitude ,day of month,It is difficult to find an appropriate replacement to NA out of 
#the reason that the data only relate to the trip, but can be concluded from experience.
Narow2 <- c(which(is.na(Airlinesurvey$Day.of.Month)),
            which(is.na(Airlinesurvey$Year.of.First.Flight)),
           which(is.na(Airlinesurvey$Scheduled.Departure.Hour)),
           which(is.na(Airlinesurvey$Flight.Distance)),
           which(is.na(Airlinesurvey$Likelihood.to.recommend)),
           which(is.na(Airlinesurvey$dlong)),
           which(is.na(Airlinesurvey$dlat)),
           which(is.na(Airlinesurvey$olat)),
           which(is.na(Airlinesurvey$Flight.time.in.minutes)),
           which(is.na(Airlinesurvey$olong))
)
Narow2 <- unique(Narow2)
Airlinesurvey <- Airlinesurvey[-Narow2,]
#Test if data set is cleaned up.
anyNA(Airlinesurvey[,1:31])
#Set row names to default to make our data frame tidy.
rownames(Airlinesurvey) <- NULL
########################### Demongraphical analysis ######################################
##########################################################################################
#1.histogram of airline status filled with satisfaction score.
cstatus <- Airlinesurvey %>%
  group_by(Airline.Status)%>%
  summarize(count =n(), avgscore = mean(Likelihood.to.recommend))
View(cstatus)
statusplot <- cstatus %>% ggplot() +
  aes(x=reorder(Airline.Status,avgscore), y = count) +
  geom_col(aes(fill = avgscore))
statusplot
#histogram of gender filled with satisfaction score
cgender <- Airlinesurvey %>%
  group_by(Gender)%>%
  summarize(count =n(), avgscore = mean(Likelihood.to.recommend))
View(cgender)
genderplot <- cgender %>% ggplot() +
  aes(x=reorder(Gender,avgscore), y = count) +
  geom_col(aes(fill = avgscore)) + labs( x = 'Gender plot')
  ggtitle('Gender plot')
genderplot  
#scatter plot of flights per year and satisfaction score
tsplot <- Airlinesurvey %>% ggplot() +
  aes(x = Flights.Per.Year,y = Likelihood.to.recommend) +
  geom_point() +
  geom_smooth(method = "lm")+
  ggtitle("")
tsplot
#Histogram of airline partners filled with satisfaction score
#1 prepare data
apdata <- Airlinesurvey %>%
  group_by(Partner.Name)%>%
  summarize(count =n(), avgscore = mean(Likelihood.to.recommend))
View(apdata)
#1 prob table of class
Cheapseatal<-Airlinesurvey %>%
  filter(str_trim(Partner.Code)=='WN')
table(Cheapseatal$Class)
table(Cheapseatal$Class) %>% prop.table()

  aes()
#2.1 
aphist <- apdata %>% ggplot() +
  aes(x=reorder(Partner.Name,avgscore), y = count) +
  geom_col(aes(fill = avgscore)) + labs( x = 'partner information')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_gradient(low = 'yellow', high = 'red')
aphist
#2.2
aphist2 <- apdata %>% ggplot() +
  aes(x=reorder(Partner.Name,count), y = count) +
  geom_col(aes(fill = avgscore)) + labs( x = 'partner information2')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_gradient(low = 'yellow', high = 'red')
aphist2

OCity <- Airlinesurvey %>%
  group_by(Origin.City) %>%
  count()
View(OCity)
#destination city
DCity <- Airlinesurvey %>%
  group_by(Destination.City) %>%
  count()
View(DCity)
ggplot() +
  aes(x = Airlinesurvey$Age)+
  geom_boxplot()



###############################Select my data set#########################################
#1########################################################################################
Southeast_surveypop <- Airlinesurvey %>%
  filter(str_trim(Partner.Code)=='WN'|str_trim(Partner.Code)=='DL'|
           str_trim(Partner.Code)=='FL'|str_trim(Partner.Code)=='VX')
View(Southeast_surveypop)
sampleList <- createDataPartition(y=Southeast_surveypop$Partner.Name ,p = .25,list = FALSE)
Southeast_survey <- Southeast_surveypop[sampleList,]
Subset2 <- Southeast_survey 
View(Southeast_survey)
###############################Predictive model###########################################
#1########################################################################################
#Linear model-- backward regression for only Southeast airline.
#1 input all of numerical variables
##########################################################################################
Southeast_lm <- lm(data = Southeast_survey ,Likelihood.to.recommend~
                     Loyalty + Eating.and.Drinking.at.Airport
                   +Flights.Per.Year + Shopping.Amount.at.Airport
                   +Total.Freq.Flyer.Accts + Departure.Delay.in.Minutes
                   +Arrival.Delay.in.Minutes + Age
                   +Flight.Distance + Flight.time.in.minutes
                   +Year.of.First.Flight + Scheduled.Departure.Hour
                   +dlong + dlat
                   +olat + olong)
summary(Southeast_lm)
#Ajusted R-squared:0.07794 remove variable Arrival.Delay.in.Minutes with the highest p-value.
Southeast_lm <- lm(data = Southeast_survey ,Likelihood.to.recommend~
                     Loyalty + Eating.and.Drinking.at.Airport
                   +Flights.Per.Year + Shopping.Amount.at.Airport
                   +Total.Freq.Flyer.Accts  + Departure.Delay.in.Minutes + Age
                   +Flight.Distance + Flight.time.in.minutes
                   +Year.of.First.Flight + Scheduled.Departure.Hour
                   +dlong + dlat
                   +olat + olong)
summary(Southeast_lm)
#Adjusted R-squared:  0.07885. remove variable Total.Freq.Flyer.Accts
Southeast_lm <- lm(data = Southeast_survey ,Likelihood.to.recommend~
                     Loyalty + Eating.and.Drinking.at.Airport
                   +Flights.Per.Year + Shopping.Amount.at.Airport
                  + Departure.Delay.in.Minutes + Age
                   +Flight.Distance + Flight.time.in.minutes
                   +Year.of.First.Flight + Scheduled.Departure.Hour
                   +dlong + dlat
                   +olat + olong)
summary(Southeast_lm)
#Adjusted R-squared:  0.07957, remove variable dlat
Southeast_lm <- lm(data = Southeast_survey ,Likelihood.to.recommend~
                     Loyalty + Eating.and.Drinking.at.Airport
                   +Flights.Per.Year + Shopping.Amount.at.Airport
                   + Departure.Delay.in.Minutes + Age
                   +Flight.Distance + Flight.time.in.minutes
                   +Year.of.First.Flight + Scheduled.Departure.Hour
                   +dlong
                   +olat + olong)
summary(Southeast_lm)
#Adjusted R-squared:  0.0802 , remove Scheduled.Departure.Hour  
Southeast_lm <- lm(data = Southeast_survey ,Likelihood.to.recommend~
                     Loyalty + Eating.and.Drinking.at.Airport
                   +Flights.Per.Year + Shopping.Amount.at.Airport
                   + Departure.Delay.in.Minutes + Age
                   +Flight.Distance + Flight.time.in.minutes
                   +Year.of.First.Flight +dlong
                   +olat + olong)
summary(Southeast_lm)
#Adjusted R-squared:  0.08042 , remove dlong
Southeast_lm <- lm(data = Southeast_survey ,Likelihood.to.recommend~
                     Loyalty + Eating.and.Drinking.at.Airport
                   +Flights.Per.Year + Shopping.Amount.at.Airport
                   + Departure.Delay.in.Minutes + Age
                   +Flight.Distance + Flight.time.in.minutes
                   +Year.of.First.Flight +olat + olong)
summary(Southeast_lm)
#Adjusted R-squared:  0.08055, remove Flight.time.in.minutes  
Southeast_lm <- lm(data = Southeast_survey ,Likelihood.to.recommend~
                     Loyalty + Eating.and.Drinking.at.Airport
                   +Flights.Per.Year
                   +Total.Freq.Flyer.Accts 
                   +Arrival.Delay.in.Minutes + Age
                   +Flight.Distance+Scheduled.Departure.Hour
                   + olong)
summary(Southeast_lm)
#Adjusted R-squared:  0.07834, keep Flight.time.in.minutes and remove Fly. Distance
Southeast_lm <- lm(data = Southeast_survey ,Likelihood.to.recommend~
                     Loyalty + Eating.and.Drinking.at.Airport
                   +Flights.Per.Year + Shopping.Amount.at.Airport
                   + Departure.Delay.in.Minutes + Age
                  + Flight.time.in.minutes
                   +Year.of.First.Flight +olat + olong)
summary(Southeast_lm)
#Adjusted R-squared:  0.0812, remove Flight.time.in.minutes 
Southeast_lm <- lm(data = Southeast_survey ,
                   Likelihood.to.recommend~
                     Loyalty + Eating.and.Drinking.at.Airport
                   +Flights.Per.Year + Shopping.Amount.at.Airport
                   + Departure.Delay.in.Minutes + Age
                   +Year.of.First.Flight +olat + olong)
summary(Southeast_lm)
#Ajusted R-squared: 0.08209 , remove olong
Southeast_lm <- lm(data = Southeast_survey , Likelihood.to.recommend~
                     Loyalty + Eating.and.Drinking.at.Airport
                   +Flights.Per.Year + Shopping.Amount.at.Airport
                   + Departure.Delay.in.Minutes + Age
                   +Year.of.First.Flight +olat)
summary(Southeast_lm)
#Ajusted R-squared: 0.08209 , remove Years of first fligh
##############################################################################################
Southeast_lm <- lm(data = Southeast_survey , Likelihood.to.recommend~
                     Loyalty + Eating.and.Drinking.at.Airport
                   +Flights.Per.Year + Shopping.Amount.at.Airport
                   + Departure.Delay.in.Minutes + Age
                   +olat)
summary(Southeast_lm)
################################################################################################
#Ajusted R-squared: 0.08231 , remove shopping amount
Southeast_lm <- lm(data = Southeast_survey , Likelihood.to.recommend~
                     Loyalty + Eating.and.Drinking.at.Airport
                   +Flights.Per.Year
                   + Departure.Delay.in.Minutes + Age
                   +olat)
summary(Southeast_lm)
#Ajusted R-squared: 0.08166 , keep shopping amount and remove eating and drinking
Southeast_lm <- lm(data = Southeast_survey , Likelihood.to.recommend~
                     Loyalty
                   +Flights.Per.Year + Shopping.Amount.at.Airport
                   + Departure.Delay.in.Minutes + Age
                   +olat)
summary(Southeast_lm)
#Adjusted R-squared:  0.0817 , keep eating and drinking and remove olat
Southeast_lm <- lm(data = Southeast_survey , Likelihood.to.recommend~
                     Loyalty + Eating.and.Drinking.at.Airport
                   +Flights.Per.Year + Shopping.Amount.at.Airport
                   + Departure.Delay.in.Minutes + Age)
summary(Southeast_lm)
#Adjusted R-squared:  0.08075 , all the insignificant varables have been tested
#The formula is ......
###############################Predictive model###########################################
#2########################################################################################
#Association rules.
#data preparation: data types to factor.
####################################################################################
############          function transformer  for numeric data            ############
####################################################################################
transformer<- function(example,tags){
  cutpoint<-quantile(example,c(.25,.5,.75))
  cutpoint <- as.numeric(cutpoint)
  cutpoint <- c(cutpoint,max(example),min(example)-1)
  cutpoint<-sort(cutpoint)
  group_tags <- cut(example, 
                    breaks=cutpoint, 
                    include.lowest=FALSE, 
                    right=TRUE, 
                    labels=tags)
  groups_factor <- factor(group_tags, levels = tags)
  return(groups_factor)
}
#########################transformer with cut point#########################
transformercut<- function(example,cutpoint,tags){
  cutpoint <- as.numeric(cutpoint)
  cutpoint<-sort(cutpoint)
  group_tags <- cut(example, 
                    breaks=cutpoint, 
                    include.lowest=FALSE, 
                    right=TRUE, 
                    labels=tags)
  groups_factor <- factor(group_tags, levels = tags)
  return(groups_factor)
}
###############################Select my data set#########################################
#1########################################################################################
Southeast_surveypop <- Airlinesurvey %>%
  filter(str_trim(Partner.Code)=='WN'|str_trim(Partner.Code)=='DL'|
           str_trim(Partner.Code)=='FL'|str_trim(Partner.Code)=='VX')
View(Southeast_surveypop)
sampleList <- createDataPartition(y=Southeast_surveypop$Partner.Name ,p = .25,list = FALSE)
Southeast_survey <- Southeast_surveypop[sampleList,]
uselesscolmun <- c(-1,-2,-6,-11,-15:-21,-28:-32)
Southeast_survey <- Southeast_survey[,uselesscolmun]
##########################################################################################
#prepare right format for likelihood to recommend
cutpoint <- c(1,7,9,11)
tags <- c('low','med','high')
group_tags <- cut(Southeast_survey$Likelihood.to.recommend,
                  breaks = cutpoint,
                  include.lowest = TRUE,
                  right = FALSE,
                  labels = tags)
Southeast_survey$Likelihood.to.recommend <- factor(group_tags, levels = tags)
Subset2 <- Southeast_survey
####################################################################################
#transform Airline status
Southeast_survey$Airline.Status <- as.factor(Southeast_survey$Airline.Status)
#transform gender
Southeast_survey$Gender <- as.factor(Southeast_survey$Gender)
#transfrom types of travel
Southeast_survey$Type.of.Travel <- as.factor(Southeast_survey$Type.of.Travel)
#transform class
Southeast_survey$Class <- as.factor(Southeast_survey$Class)
#transform flight canceled
Southeast_survey$Flight.cancelled <- as.factor(Southeast_survey$Flight.cancelled)
Subset3 <- Southeast_survey 
####################################################################################
tags <- c('less','normal','usual','frequent')
Southeast_survey$Flights.Per.Year <- transformer(Southeast_survey$Flights.Per.Year,tags)
####################################################################################
####################################################################################
#transform Flights Per Year column
tags <- c('old','new')
cutpoint <- c(0,2006,3000)
Southeast_survey$Year.of.First.Flight <- transformercut(Southeast_survey$Year.of.First.Flight,cutpoint,tags)
####################################################################################
#transform loyalty
tags <- c('casual','loyal')
cutpoint <- c(-2,0,2)
Southeast_survey$Loyalty <- transformercut(Southeast_survey$Loyalty,cutpoint,tags)
####################################################################################
####################################################################################
#transform Age column
tags <- c('youth','middleage','elder','eldest')
Southeast_survey$Age <- transformer(Southeast_survey$Age,tags)
####################################################################################
####################################################################################
#transform shopping amount
tags <- c('normal','shopper')
cutpoint <- c(-1,50,500)
Southeast_survey$Shopping.Amount.at.Airport <- transformercut(Southeast_survey$Shopping.Amount.at.Airport,cutpoint,tags)
####################################################################################
####################################################################################
#transform eating amount
tags <- c('normal','foodie')
cutpoint <- c(-1,50,500)
Southeast_survey$Eating.and.Drinking.at.Airport <- transformercut(Southeast_survey$Eating.and.Drinking.at.Airport,cutpoint,tags)
####################################################################################
#transform departure delay
tags <- c('on time','delay')
cutpoint <- c(-1,6,500)
Southeast_survey$Departure.Delay.in.Minutes<- transformercut(Southeast_survey$Departure.Delay.in.Minutes,cutpoint,tags)
####################################################################################
#transform arrival delay
tags <- c('on time','delay')
cutpoint <- c(-1,6,500)
Southeast_survey$Arrival.Delay.in.Minutes<- transformercut(Southeast_survey$Arrival.Delay.in.Minutes,cutpoint,tags)
####################################################################################
#transform flight distance
tags <- c('short-haul','mid-hual','long-haul')
cutpoint <- c(0,701,2201,5000)
Southeast_survey$Flight.Distance<- transformercut(Southeast_survey$Flight.Distance,cutpoint,tags)
####################################################################################
#transform flight distance
tags <- c('short-trip','long-trip')
cutpoint <- c(0,180,560)
Southeast_survey$Flight.time.in.minutes<- transformercut(Southeast_survey$Flight.time.in.minutes,cutpoint,tags)
View(Southeast_survey)
####################################################################################
newlist <- c(1:6,8,9,16)
newlist2 <-c(1:6,8,9)
customer <- Southeast_survey[,newlist]
flights <- Southeast_survey[,-newlist2]
View(customer)
View(flights)
####################################################################################
#ARA on customer
####################################################################################
#transform it into transactions
SurveyX <- as(customer,"transactions")
inspect(SurveyX)
itemFrequency(SurveyX)
itemFrequencyPlot(SurveyX)
#apply ARA on it
ruleset <- apriori(SurveyX,
                   parameter=list(support=0.005,confidence=0.5),
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=high")))
inspect(ruleset)
inspectDT(ruleset)
plot(ruleset, method = "grouped matrix", engine ="interactive")
####################################################################################
####################################################################################
#ARA on flights
####################################################################################
#transform it into transactions
SurveyX <- as(flights,"transactions")
inspect(SurveyX)
itemFrequency(SurveyX)
itemFrequencyPlot(SurveyX)
#apply ARA on it
ruleset <- apriori(SurveyX,
                   parameter=list(support=0.005,confidence=0.5),
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=high")))
inspect(ruleset)
inspectDT(ruleset)
plot(ruleset, method = "grouped matrix", engine ="interactive")
####################################################################################
###############################Predictive model###########################################
#3########################################################################################
#Support vector machine.
#data preparation: data types to factor.
###############################Select my data set#########################################
#1########################################################################################
Southeast_surveypop <- Airlinesurvey %>%
  filter(str_trim(Partner.Code)=='WN'|str_trim(Partner.Code)=='DL'|
           str_trim(Partner.Code)=='FL'|str_trim(Partner.Code)=='VX')
View(Southeast_surveypop)
sampleList <- createDataPartition(y=Southeast_surveypop$Partner.Name ,p = .20,list = FALSE)
Southeast_survey <- Southeast_surveypop[sampleList,]
#prepare right format for likelihood to recommend
cutpoint <- c(1,7,9,11)
tags <- c('low','med','high')
group_tags <- cut(Southeast_survey$Likelihood.to.recommend,
                  breaks = cutpoint,
                  include.lowest = TRUE,
                  right = FALSE,
                  labels = tags)
Southeast_survey$Likelihood.to.recommend <- factor(group_tags, levels = tags)
##########################################################################################
#pull out data columns we are going to use

SVMset <- Subset3[,-13]
str(SVMset)
colnames(SVMset)
##########################################################################################
#creatiing training set and test set
trainList <- createDataPartition(y=SVMset$Likelihood.to.recommend,p = .70,list = FALSE)
trainData <- SVMset[trainList,]
testData <- SVMset[-trainList,]
##########################################################################################
#conduct svm
svmOutput<- ksvm(Likelihood.to.recommend ~., data = trainData, kernel = "rbfdot",
                 kpar = "automatic", C = 5, cross = 3 , prob.model = TRUE)
svmOutput
svmPred <- predict(svmOutput, testData)
svmPred
plot(svmOutput, data = testData)
confusionMatrix(testData$Likelihood.to.recommend ,svmPred)
#accuracy canculation
#           Reference
#Prediction low med high
#low        27  21   24
#med        16  27   35
#high       9  23   82
########################################4#################################################
###############################Low satisfactory###########################################
#################################visualization############################################
#preparation data
Southeast_surveypop <- Airlinesurvey %>%
  filter(str_trim(Partner.Code)=='WN'|str_trim(Partner.Code)=='DL'|
           str_trim(Partner.Code)=='FL'|str_trim(Partner.Code)=='VX')
View(Southeast_surveypop)
##########################################################################################
#prepare right format for likelihood to recommend
cutpoint <- c(1,7,9,11)
tags <- c('low','med','high')
group_tags <- cut(Southeast_surveypop$Likelihood.to.recommend,
                  breaks = cutpoint,
                  include.lowest = TRUE,
                  right = FALSE,
                  labels = tags)
Southeast_surveypop$Likelihood.to.recommend <- factor(group_tags, levels = tags)
Subset2 <- Southeast_surveypop
Subset2$Likelihood.to.recommend <- as.character(Subset2$Likelihood.to.recommend)

City <- Subset2 %>%
  group_by(Origin.City)%>%
  summarize(count=n())
View(City)
#find the top 3 bad trips cities
Badcity <- Subset2 %>%
  filter(Likelihood.to.recommend=='low')%>%
  group_by(Origin.City)%>%
  summarize(bad=n())
View(Badcity)
#find the top2 good trips cities
Goodcity <- Subset2 %>%
  filter(Likelihood.to.recommend=='high')%>%
  group_by(Origin.City)%>%
  summarize(good=n())
View(Goodcity)
Cityrate <- merge(Badcity,Goodcity,by = "Origin.City")
Cityrate <- merge(Cityrate,City,by = "Origin.City")
Cityrate <- data.frame(Cityrate, "bad rate"=Cityrate$bad/Cityrate$count
                       , "good rate"=Cityrate$good/Cityrate$count)
View(Cityrate)
#create bad location set
Badcitylocation <- Subset2 %>% 
  filter(Likelihood.to.recommend=='low') %>%
  filter(Origin.City=='Atlanta, GA'|Origin.City=='Las Vegas, NV')
Badcitylocation <- Badcitylocation[,28:31]
View(Badcitylocation)
#U.S map visualization
USmap <- borders("state" , colour = "grey" , fill = "white")
USmap<-ggplot() + USmap +
  geom_curve(data = Badcitylocation, aes(x=olong, y = olat, xend = dlong, yend = dlat),
             col = "red", size = .3,
             curvature =  0.2,arrow = arrow(length = unit(0.2, "cm")))+
  geom_point(data = Badcitylocation, 
             aes(x = olong, y = olat), color = 'yellow', size = 3) +
  geom_point(data = Badcitylocation, 
             aes(x = dlong, y = dlat), color = 'blue', size = 0.3) + 
  ggtitle("Bad trip visualization")
USmap
#create comparing location set
Goodcitylocation <- Subset2 %>% 
  filter(Likelihood.to.recommend=='high') %>%
  filter(Origin.City=='Flint, MI'|Origin.City=='Wichita, KS'|
           Origin.City=='Albany, NY'|Origin.City=='Oklahoma City, OK'|
           Origin.City=='San Juan, PR')
Goodcitylocation <- Goodcitylocation[,28:31]
View(Goodcitylocation)
Badcitylocation2 <- Subset2 %>% 
  filter(Likelihood.to.recommend=='low') %>%
  filter(Origin.City=='Flint, MI'|Origin.City=='Wichita, KS'|
           Origin.City=='Albany, NY'|Origin.City=='Oklahoma City, OK'|
           Origin.City=='San Juan, PR')
View(Badcitylocation2)
Badcitylocation2 <- Badcitylocation2[,28:31]
#create comparing map
Comparingmap <- borders("state" , colour = "grey" , fill = "white")
Comparingmap<-ggplot() + Comparingmap +
  geom_curve(data = Goodcitylocation, aes(x=olong, y = olat, xend = dlong, yend = dlat),
             col = "green", size = .3,
             curvature =  0.2,arrow = arrow(length = unit(0.2, "cm")))+
  geom_curve(data = Badcitylocation2, aes(x=olong, y = olat, xend = dlong, yend = dlat),
             col = "red", size = .3,
             curvature =  0.4,arrow = arrow(length = unit(0.2, "cm")))+
  geom_point(data = Goodcitylocation, 
             aes(x = olong, y = olat), color = 'yellow', size = 3) +
  geom_point(data = Badcitylocation2, 
             aes(x = dlong, y = dlat), color = 'blue', size = 0.3) + 
  geom_point(data = Goodcitylocation, 
             aes(x = dlong, y = dlat), color = 'blue', size = .3) +
  geom_point(data = Badcitylocation2, 
             aes(x = olong, y = olat), color = 'yellow', size = 3) + 
  ggtitle("Comparing trip visualization")
Comparingmap
#connection map with curve
USmap <- borders("state" , colour = "grey" , fill = "white")
USmap<-ggplot() + USmap +
  geom_curve(data = Lowset, aes(x=olong, y = olat, xend = dlong, yend = dlat),
             col = "red",
             size = .3,
             curvature =  0.2)
#2
########################################4#################################################
########################################4#################################################
mean(Airlinesurvey$Likelihood.to.recommend,na.rm = TRUE)
table(Airlinesurvey$Gender)
table(Airlinesurvey$Partner.Name)
str(Airlinesurvey$Partner.Name)
#plots for numerical data. to see the distribution of them.
ggplot() +
  aes(x = Airlinesurvey$Age)+
  geom_boxplot()
