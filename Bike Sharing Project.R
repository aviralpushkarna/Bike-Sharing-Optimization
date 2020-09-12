########################################


##########################################


#### Read the data file ##### 
BD <- read.csv('MetroQ1.csv')

#############################

#### Exploring data to understand how to proceed with data cleaning ####

sum(BD$bike_type != "standard")

type_count <- ggplot(data = BD, aes(x=bike_type, fill= bike_type)) + geom_bar()
type_count

sum(BD$bike_type != "standard" & BD$bike_type != "smart" & BD$bike_type != "electric")

sum(BD$passholder_type != "Walk-up" & BD$passholder_type != "Monthly Pass" & BD$passholder_type != "Annual Pass")
sum(BD$passholder_type == "Testing")

head(BD$passholder_type)


pass_count <- ggplot(data = BD, aes(x=passholder_type, fill= passholder_type)) + 
  geom_bar() + theme(axis.text.x=element_blank(),
                     axis.ticks.x=element_blank()
  )
pass_count


sum(BD$passholder_type == "Flex Pass")


plan_dur_count <- ggplot(data = BD, aes(x= as.factor(BD$plan_duration), fill= BD$plan_duration)) + 
  geom_bar() + theme(axis.ticks.x=element_blank())
plan_dur_count

subset(BD, BD$plan_duration != 365 & BD$plan_duration != 30 & BD$plan_duration != 1)

sum(BD$plan_duration == 1 & BD$passholder_type == "Walk-up")

head(BD$trip_route_category)

sum(BD$trip_route_category != "One Way" & BD$trip_route_category != "Round Trip")

sum(BD$trip_route_category != "Round Trip")

#### PLOT TRIP TYPE
trip_type_count <- ggplot(data = BD, aes(x= trip_route_category, fill= trip_route_category)) + 
  geom_bar() + theme(axis.ticks.x=element_blank(),
                     axis.title.x=element_blank())
trip_type_count




#############################################################################




######### DROP FLEX PASS AND TESTING PASS TO CLEAM DATA #####################

BDnew <- subset(BD, BD$passholder_type== "Walk-up" | 
                  BD$passholder_type== "One Day Pass" |
                  BD$passholder_type== "Monthly Pass" |
                  BD$passholder_type== "Annual Pass")


testgroup <-  subset(BDnew, BDnew$passholder_type == "Walk-up" & BDnew$plan_duration != 1)


############################################################################


######### CHECK DISTRIBUTION OF MISCODED VALUES ############################

test.pass_count <- ggplot(data = testgroup, aes(x=plan_duration, fill= plan_duration)) + 
  geom_bar() 
test.pass_count

###########################################################################

########## CHECK FOR DISCREPANCIES IN THE COLUMNS #########################

sum(BDnew$passholder_type == "Walk-up" & BDnew$plan_duration != 1)
sum(BDnew$passholder_type == "Walk-up" & BDnew$plan_duration == 1)

##########################################################################


########## CHECK FOR DISCREPANCIES IN THE COLUMNS ########################
sum(BDnew$passholder_type == "One Day Pass" & BDnew$plan_duration != 1)
sum(BDnew$passholder_type == "Monthly Pass" & BDnew$plan_duration != 30)
sum(BDnew$passholder_type == "Annual Pass" & BDnew$plan_duration != 365)

##########################################################################

######### INstall and load packages that may be required in analysis #######

library(ggplot2)
install.packages('usmap')
library(usmap)

library(MASS)
library(ggplot2)
library(tidyverse)

install.packages('urbnmapr')
install.packages('devtools')

library(urbnmapr)
library(tidyverse)

library(sf)
install.packages('tmap')
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)

#########################################################################

names(BDnew)

summary(BDnew)

############# Some Exploratory Analysis #################################


gt1 <- ggplot(BDnew, aes(duration, fill = passholder_type)) + geom_histogram(binwidth  = 50)
gt1

gt2 <- ggplot(BDnew, aes(BDnew$trip_route_category, fill = passholder_type, xlab = "Trip Route Category")) + geom_bar() + theme_bw() + labs(x="Trip Route Category")
gt2

gt3 <- ggplot(BDnew, aes(BDnew$start_station,BDnew$end_station, color = passholder_type)) + geom_point()
gt3

gt4 <- ggplot(BDnew, aes(BDnew$start_station, fill = passholder_type)) + geom_bar()
gt4


######################################################################

## PLEASE NOTE: THIS SECTION OF CODE REQUIRES A GOOGLE API KEY IN ORDER TO
## RUN, THE KEY HAS NOT BEEN INCLUDED IN THE SUBMITTED CODE AS IT IS CONFIDENTIAL

## PLEAE ENTER YOUR GOOGLE API KEY IN THE LINE OF CODE BELOW
## THANK YOU FOR YOUR COOPERATION

######################################################################

ggmap::register_google(key = "Please enter your google api key")



########## Exploring and analysing data using maps ############### 

p <- ggmap(get_googlemap(center = c(lon = -118.335167, lat = 34.05287),
                         zoom = 11, scale = 2,
                         maptype ='terrain',
                         color = 'color'))

mp1 <- p + geom_point(aes(x = start_lon, y = start_lat,  colour = passholder_type), data = BDnew, size = 3.5) + 
  theme(legend.position="bottom")

mp2 <- mp1 +
  geom_line(aes(x = start_lon, y = start_lat), col = "orange", data = BDnew)

mp2

summary(BDnew$trip_route_category)



p2 <- ggmap(get_googlemap(center = c(lon = -118.335167, lat = 34.05287),
                         zoom = 11, scale = 2,
                         maptype ='terrain',
                         color = 'color'))

mp3 <- p2 + geom_point(aes(x = start_lon, y = start_lat,  colour = trip_route_category), data = BDnew, size = 3.5) + 
  theme(legend.position="bottom")

mp4 <- mp3 +
  geom_line(aes(x = start_lon, y = start_lat), col = "green", data = BDnew)

mp4

###########################################################################

summary(BDnew)



####### Creating new colums to store each time variable separately #########

BDnew$sdate <- sapply(strsplit(as.character(BDnew$start_time), " "), "[", 1)
BDnew$stime <- sapply(strsplit(as.character(BDnew$start_time), " "), "[", 2)
BDnew$syear <- sapply(strsplit(as.character(BDnew$sdate), "-"), "[", 1)
BDnew$smonth <- sapply(strsplit(as.character(BDnew$sdate), "-"), "[", 2)
BDnew$sday <- sapply(strsplit(as.character(BDnew$sdate), "-"), "[", 3)
BDnew$shour <- sapply(strsplit(as.character(BDnew$stime), ":"), "[", 1)
BDnew$sminiute <- sapply(strsplit(as.character(BDnew$stime), ":"), "[", 2)
BDnew$ssecond <- sapply(strsplit(as.character(BDnew$stime), ":"), "[", 3)

#############################################################################


######### Change hour, minute, second to numeric value ######################

BDnew$shour<-as.numeric(BDnew$shour)
BDnew$sminiute<-as.numeric(BDnew$sminiute)
BDnew$ssecond<-as.numeric(BDnew$ssecond)

#############################################################################


########## Using time values to explore customer behaviour ##################

gt5 <- ggplot(BDnew, aes(BDnew$shour, fill = "red")) + geom_bar() + theme_bw() + labs(x = "Start Time of the Trip (HH)") + theme(legend.position = "none")
gt5

############################################################################



##############################################################################

## DESIGNING AND IMPLEMENTING MODELS ##

##############################################################################


###### import and clean data ##############################
cb<-read.csv("MetroQ1.csv")

#clean columns-end station
cb[is.na(cb$end_station),]
cb[is.null(cb$end_station),]
cb[is.nan(cb$end_station),]
typeof(cb$end_station)
cb$end_station_factor<-as.factor(cb$end_station)

##drop flex pass and testing pass

cb <- subset(cb, cb$passholder_type== "Walk-up" | 
               cb$passholder_type== "One Day Pass" |
               cb$passholder_type== "Monthly Pass" |
               cb$passholder_type== "Annual Pass")

cb$passholder_type<-as.character(cb$passholder_type)
cb$passholder_type<-as.factor(cb$passholder_type)


#clean columns-end_lat & end_lon
cb[is.na(cb$end_lat),]
cb[is.null(cb$end_lat),]
cb[is.nan(cb$end_lat),]
typeof(cb$end_lat)

summary(cb[is.na(cb$end_lat),])
summary(cb[is.na(cb$end_lon),])

summary(cb[cb$end_station==4285,])
summary(cb[cb$end_station==4286,])

#clean columns-bike_id
cb[is.na(cb$bike_id),]
cb[is.null(cb$bike_id),]
cb[is.nan(cb$bike_id),]
typeof(cb$bike_id)

###########################################################################


############### convert date and time to separate columns ##################

##### change start time #####

cb$sdate <- sapply(strsplit(as.character(cb$start_time), " "), "[", 1)
cb$stime <- sapply(strsplit(as.character(cb$start_time), " "), "[", 2)
cb$smonth <- sapply(strsplit(as.character(cb$sdate), "/"), "[", 1)
cb$sday <- sapply(strsplit(as.character(cb$sdate), "/"), "[", 2)
cb$syear <- sapply(strsplit(as.character(cb$sdate), "/"), "[", 3)
cb$shour <- sapply(strsplit(as.character(cb$stime), ":"), "[", 1)
cb$sminiute <- sapply(strsplit(as.character(cb$stime), ":"), "[", 2)


#### change end time #####

cb$edate <- sapply(strsplit(as.character(cb$end_time), " "), "[", 1)
cb$etime <- sapply(strsplit(as.character(cb$end_time), " "), "[", 2)
cb$emonth <- sapply(strsplit(as.character(cb$edate), "/"), "[", 1)
cb$eday <- sapply(strsplit(as.character(cb$edate), "/"), "[", 2)
cb$eyear <- sapply(strsplit(as.character(cb$edate), "/"), "[", 3)
cb$ehour <- sapply(strsplit(as.character(cb$etime), ":"), "[", 1)
cb$eminiute <- sapply(strsplit(as.character(cb$etime), ":"), "[", 2)

#### change hour, minute, second to numeric value ####

cb$shour<-as.numeric(cb$shour)
cb$sminiute<-as.numeric(cb$sminiute)

cb$ehour<-as.numeric(cb$ehour)
cb$eminiute<-as.numeric(cb$eminiute)

##########################################################################


################# CLASSIFICATION MODEL BUILDING ##########################

#splitting the training set and test set 

smp_siz<-floor(0.75*nrow(cb))
smp_siz
set.seed(123)
train_ind=sample(seq_len(nrow(cb)),size=smp_siz)
train=cb[train_ind,]
test=cb[-train_ind,]
nrow(train)
nrow(test)
nrow(cb)

##svm

install.packages("e1071")
install.packages("rpart")
library(e1071)
library(rpart)

#the svm model code successfully ran on an Mac air, Rstudio 1.1.453, R 3.6.1.
#you might run into errors like "contrasts can be applied only to factors with two or more levels" if you are using other devices or other versions of R/Rstudio.
#svm usually takes an extremely long time to run (~15 mins) and please be patient
svm_result <- svm(passholder_type~duration+emonth+eday+ehour+eminiute+smonth+sday+shour+sminiute+start_station+end_station+trip_route_category+bike_type, data=train, type="C-classification", kernel="linear")
summary(svm_result)

prediction_svm<-predict(svm_result,test,type='class')

# svm table and evaluate accuracy
xtab_svm <- table(na.omit(test)$passholder_type, prediction_svm)
xtab_svm
(6566+2+2447)/nrow(na.omit(test))

summary(cb)

# plot the separating hyperplane
plot(start_station~duration, data=test, main="Passholder Type Prediction via SVM", xlab='duration', ylab='Start_station', col=ifelse(passholder_type=="Annual Pass",'red',ifelse(passholder_type=="Monthly Pass", 'blue',ifelse(passholder_type=="One Day Pass",'yellow','green' ))))

## Classification Tree

# install the required packages some packages
install.packages("tree")
install.packages("partykit")
library(partykit)
library(tree)

passholder_tree <- tree(passholder_type~duration+eyear+emonth+eday+ehour+eminiute+syear+smonth+sday+shour+sminiute+start_station+end_station+trip_route_category+bike_type, data=train) 

summary(passholder_tree)

# description of the whole tree by 
passholder_tree
# plot the tree
plot(passholder_tree) 
text(passholder_tree, label="yprob")

#check test sample
prediction_tree<-predict(passholder_tree,test, type='class')
xtab_tree <- table(test$passholder_type, prediction_tree)
xtab_tree

#out of sample accuracy
(6883+2207)/nrow(test)

## multinomial logistic regression

install.packages('nnet')
library(nnet)
log_train<-multinom(passholder_type~duration+emonth+eday+ehour+eminiute+smonth+sday+shour+sminiute+start_station+end_station+trip_route_category+bike_type, data=train)
prediction_log<-predict(log_train, test[complete.cases(test),], "class")
xtab_log <- table(na.omit(test)$passholder_type, prediction_log)
xtab_log

#out of sample accuracy
(1+6460+1+2578)/nrow(na.omit(test))

## random forest
#install packages
install.packages('randomForest')
library(randomForest)

rforest_train<-randomForest(passholder_type~duration+emonth+eday+ehour+eminiute+smonth+sday+shour+sminiute+start_station+end_station+trip_route_category+bike_type, data=train, nodesize=5, nC = 500, mtry = 4)
prediction_rf<-predict(rforest_train, na.omit(test), "class")
xtab_rf <- table(na.omit(test)$passholder_type, prediction_rf)
xtab_rf

#out of sample accuracy
(191+6700+89+3429)/nrow(na.omit(test))

##evaluation

#svm out of sample accuracy
xtab_svm
(6566+2+2447)/nrow(na.omit(test))
#classification tree sample accuracy
xtab_tree
(6883+2207)/nrow(test)
#multinomial out of sample accuracy
xtab_log
(1+6460+1+2578)/nrow(na.omit(test))
#random forest out of sample accuracy
xtab_rf
(191+6700+89+3429)/nrow(na.omit(test))
##the number might be slightly off because of different randomization results.