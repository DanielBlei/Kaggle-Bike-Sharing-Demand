dir = "C:\\sers\\danie\\Desktop\\CA2"

setwd(dir)
library(Metrics)
library(RColorBrewer)
library(rattle)
library(rpart)
library(rpart.plot)
library(caret)
library(xgboost)

Biketrain = read.csv("train_bike.csv", header = T)
Biketest = read.csv("test_bike.csv", header = T)
sample = read.csv("sampleSubmission.csv", header = T)




#Matching the test and train columns

Biketest$casual = 0
Biketest$registered = 0
Biketest$count = 0

Biketest$count = as.integer(Biketest$count)
Biketest$registered = as.integer(Biketest$registered)
Biketest$casual = as.integer(Biketest$casual)
#----------------------------------------------------------------------------------------------------------------------
#Understanding the data
#----------------------------------------------------------------------------------------------------------------------
datacombined = rbind(Biketrain,Biketest)

hist(datacombined$season)
hist(datacombined$weather)
hist(datacombined$humidity)
hist(datacombined$holiday)
hist(datacombined$workingday)
hist(datacombined$temp)
hist(datacombined$atemp)
hist(datacombined$windspeed)

#Splitting the day, month, year..

Biketrain$month=substr(Biketrain$datetime,4,5)
Biketrain$month=as.integer(Biketrain$month)
Biketrain$year=substr(Biketrain$datetime,7,10)
Biketrain$year=as.integer(Biketrain$year)
Biketrain$hour=substr(Biketrain$datetime,12,13)
Biketrain$hour=as.factor(Biketrain$hour)
date=substr(Biketrain$datetime,1,10)
days<-weekdays(as.Date(date))
Biketrain$week_D=days

Biketest$month=substr(Biketest$datetime,4,5)
Biketest$month=as.integer(Biketest$month)
Biketest$year=substr(Biketest$datetime,7,10)
Biketest$year=as.integer(Biketest$year)
Biketest$hour=substr(Biketest$datetime,12,13)
Biketest$hour=as.factor(Biketest$hour)
date=substr(Biketest$datetime,1,10)
days<-weekdays(as.Date(date))
Biketest$week_D=days

# 2 part

#Plotting boxplot to see demand.

boxplot(Biketrain$count~Biketrain$hour,xlcab="hour", ylab="count of users")

#Use the same code to plot registered and casual separated. 
#Treating outliers
boxplot(log(Biketrain$count)~Biketrain$hour,xlab="hour",ylab="log(count)")
boxplot(Biketrain$registered~Biketrain$week_D,xlab="day", ylab="registered users") #Plotting to analyse
boxplot(Biketrain$casual~Biketrain$week_D,xlab="day", ylab="casual users")

#Plot the weather variable to analyse.

boxplot(Biketrain$registered~Biketrain$weather,xlab="Weather", ylab="registered users") #Plotting to analyse
boxplot(Biketrain$casual~Biketrain$weather,xlab="Weather", ylab="casual users")

#identify any strong correlations continuous variables.
sub=data.frame(Biketrain$registered,Biketrain$casual,Biketrain$count,Biketrain$temp,Biketrain$humidity,Biketrain$atemp,Biketrain$windspeed)
M <- cor(sub)[1:7,1:7]
colnames(M) <- c("Registered", "Casual", "Count", "Temp", "Humidity", "atemp", "Windspeed")
rownames(M) <- c("Registered", "Casual", "Count", "Temp", "Humidity", "atemp", "Windspeed")
corrplot(M,method="number")

#--------------------------------------------------------------------------------------------------------------------
#Feature Engineering
#--------------------------------------------------------------------------------------------------------------------

#Converting the variables 

Biketrain$weather[Biketrain$weather ==4] <- 3
Biketest$weather[Biketest$weather ==4] <- 3

Biketrain$hour <- as.integer(Biketrain$hour)
Biketrain$date <- as.integer(Biketrain$date)
Biketrain$season= as.factor(Biketrain$season)
Biketrain$weather<- as.factor(Biketrain$weather)
Biketrain$holiday<- as.factor(Biketrain$holiday)
Biketrain$workingday <- as.factor(Biketrain$workingday)
Biketrain$month <- as.factor(Biketrain$month)

Biketest$hour <- as.integer(Biketest$hour)
Biketest$date <- as.integer(Biketest$date)
Biketest$season= as.factor(Biketest$season)
Biketest$weather<- as.factor(Biketest$weather)
Biketest$holiday<- as.factor(Biketest$holiday)
Biketest$workingday <- as.factor(Biketest$workingday)
Biketest$month <- as.factor(Biketest$month)


#Creating a variable splitting the hours of the day
Biketrain$daypart <- "4"
Biketest$daypart <-"4"

Biketrain$daypart[(Biketrain$hour < 10) & (Biketrain$hour > 3)] <- 1
Biketest$daypart[(Biketest$hour < 10) & (Biketest$hour > 3)] <- 1
Biketrain$daypart[(Biketrain$hour < 16) & (Biketrain$hour > 9)] <- 2
Biketest$daypart[(Biketest$hour < 16) & (Biketest$hour > 9)] <- 2
Biketrain$daypart[(Biketrain$hour < 22) & (Biketrain$hour > 15)] <- 3
Biketest$daypart[(Biketest$hour < 22) & (Biketest$hour > 15)] <- 3

#Creating a variable separating weekdays,holiday and weekend

Biketrain$day_type=""
Biketrain$day_type[Biketrain$holiday==0 & Biketrain$workingday==0]="weekend"
Biketrain$day_type[Biketrain$holiday==1]="holiday"
Biketrain$day_type[Biketrain$holiday==0 & Biketrain$workingday==1]="working day"

Biketest$day_type=""
Biketest$day_type[Biketest$holiday==0 & Biketest$workingday==0]="weekend"
Biketest$day_type[Biketest$holiday==1]="holiday"
Biketest$day_type[Biketest$holiday==0 & Biketest$workingday==1]="working day"

Biketest$day_type = as.factor(Biketest$day_type)
Biketrain$day_type = as.factor(Biketrain$day_type)


#Extracting days of week from datetime
Biketrain$week_D<-as.factor(weekdays(as.Date(Biketrain$date, origin = "2011-01-01")))
Biketest$week_D<-as.factor(weekdays(as.Date(Biketest$date, origin = "2011-01-01")))

#spliting the hours for casual and registered

Biketrain$dp_reg=0
Biketrain$dp_reg[Biketrain$hour<8]=1
Biketrain$dp_reg[Biketrain$hour>=22]=2
Biketrain$dp_reg[Biketrain$hour>9 & Biketrain$hour<18]=3
Biketrain$dp_reg[Biketrain$hour==8]=4
Biketrain$dp_reg[Biketrain$hour==9]=5
Biketrain$dp_reg[Biketrain$hour==20 | Biketrain$hour==21]=6
Biketrain$dp_reg[Biketrain$hour==19 | Biketrain$hour==18]=7

Biketrain$dp_cas=0
Biketrain$dp_cas[Biketrain$hour<=8]=1
Biketrain$dp_cas[Biketrain$hour==9]=2
Biketrain$dp_cas[Biketrain$hour>=10 & Biketrain$hour<=19]=3
Biketrain$dp_cas[Biketrain$hour>19]=4

Biketest$dp_reg=0
Biketest$dp_reg[Biketest$hour<8]=1
Biketest$dp_reg[Biketest$hour>=22]=2
Biketest$dp_reg[Biketest$hour>9 & Biketest$hour<18]=3
Biketest$dp_reg[Biketest$hour==8]=4
Biketest$dp_reg[Biketest$hour==9]=5
Biketest$dp_reg[Biketest$hour==20 | Biketest$hour==21]=6
Biketest$dp_reg[Biketest$hour==19 | Biketest$hour==18]=7

Biketest$dp_cas=0
Biketest$dp_cas[Biketest$hour<=8]=1
Biketest$dp_cas[Biketest$hour==9]=2
Biketest$dp_cas[Biketest$hour>=10 & Biketest$hour<=19]=3
Biketest$dp_cas[Biketest$hour>19]=4

#splitting the temperature for casual and registed

Biketrain$temp_reg=0
Biketrain$temp_reg[Biketrain$temp<13]=1
Biketrain$temp_reg[Biketrain$temp>=13 & Biketrain$temp<23]=2
Biketrain$temp_reg[Biketrain$temp>=23 & Biketrain$temp<30]=3
Biketrain$temp_reg[Biketrain$temp>=30]=4

Biketrain$temp_cas=0
Biketrain$temp_cas[Biketrain$temp<15]=1
Biketrain$temp_cas[Biketrain$temp>=15 & Biketrain$temp<23]=2
Biketrain$temp_cas[Biketrain$temp>=23 & Biketrain$temp<30]=3
Biketrain$temp_cas[Biketrain$temp>=30]=4

Biketest$temp_reg=0
Biketest$temp_reg[Biketest$temp<13]=1
Biketest$temp_reg[Biketest$temp>=13 & Biketest$temp<23]=2
Biketest$temp_reg[Biketest$temp>=23 & Biketest$temp<30]=3
Biketest$temp_reg[Biketest$temp>=30]=4

Biketest$temp_cas=0
Biketest$temp_cas[Biketest$temp<15]=1
Biketest$temp_cas[Biketest$temp>=15 & Biketest$temp<23]=2
Biketest$temp_cas[Biketest$temp>=23 & Biketest$temp<30]=3
Biketest$temp_cas[Biketest$temp>=30]=4

#Creating bins for years
Biketrain$year_bin = 0
Biketrain$year_bin[Biketrain$year=='2011']=1
Biketrain$year_bin[Biketrain$year=='2011' & Biketrain$month>3]=2
Biketrain$year_bin[Biketrain$year=='2011' & Biketrain$month>6]=3
Biketrain$year_bin[Biketrain$year=='2011' & Biketrain$month>9]=4
Biketrain$year_bin[Biketrain$year=='2012']=5
Biketrain$year_bin[Biketrain$year=='2012' & Biketrain$month>3]=6
Biketrain$year_bin[Biketrain$year=='2012' & Biketrain$month>6]=7
Biketrain$year_bin[Biketrain$year=='2012' & Biketrain$month>9]=8

Biketest$year_bin = 0
Biketest$year_bin[Biketest$year=='2011']=1
Biketest$year_bin[Biketest$year=='2011' & Biketest$month>3]=2
Biketest$year_bin[Biketest$year=='2011' & Biketest$month>6]=3
Biketest$year_bin[Biketest$year=='2011' & Biketest$month>9]=4
Biketest$year_bin[Biketest$year=='2012']=5
Biketest$year_bin[Biketest$year=='2012' & Biketest$month>3]=6
Biketest$year_bin[Biketest$year=='2012' & Biketest$month>6]=7
Biketest$year_bin[Biketest$year=='2012' & Biketest$month>9]=8

#Converting the variables to build the models

Biketrain$daypart <- as.factor(Biketrain$daypart)
Biketrain$dp_cas <- as.factor(Biketrain$dp_cas)
Biketrain$dp_reg <- as.factor(Biketrain$dp_reg)
Biketrain$year <- as.factor(Biketrain$year)
Biketrain$hour <- as.factor(Biketrain$hour)
Biketrain$temp_cas <- as.factor(Biketrain$temp_cas)
Biketrain$temp_reg <- as.factor(Biketrain$temp_reg)

Biketest$temp_reg <- as.factor(Biketest$temp_reg)
Biketest$temp_cas <- as.factor(Biketest$temp_cas)
Biketest$dp_cas <- as.factor(Biketest$dp_cas)
Biketest$dp_reg <- as.factor(Biketest$dp_reg)
Biketest$daypart <- as.factor(Biketest$daypart)
Biketest$year <- as.factor(Biketest$year)
Biketest$hour <- as.factor(Biketest$hour)

#Separating the windspeed with 0 values

Biketrain_w <- subset(Biketrain,Biketrain$windspeed== 0)
Biketrain_nw <- subset(Biketrain,!Biketrain$windspeed== 0)

#Combining the dataset and using index to separate the train and test

Wind_combine <- rbind(Biketrain_nw, Biketrain_w)

Wind_train <- Wind_combine[1:9573,]
Wind_test1 <- Wind_combine[9574:10886,]
Wind_Test_P <- subset(Biketest,Biketest$windspeed == 0)

#Predicting the windspeed for the main train and test dataset.


cv.ctrl1 <- trainControl(method = "cv",number = 20,
                        verboseIter=T,
                        classProbs= F)

xgb.grid1 <- expand.grid(nrounds = seq(1,600, by =5),
                        max_depth = c(5),
                        eta = c(0.1),
                        gamma = c(0),
                        colsample_bytree = 0.75,
                        min_child_weight = c(0),
                        subsample = c(0.9))

Wind_M <- train(log1p(windspeed) ~ date + month + year + season +
                   + weather + temp +atemp + humidity + 
                   daypart + day_type + week_D, data = Biketrain,
                  method="xgbTree",
                  stratified=TRUE,
                  verbose = 1,
                  eval_metric="rmse",
                preProc = c("center", "scale"),
                  trControl = cv.ctrl1,
                  tuneGrid = xgb.grid1)

Wind_P = expm1(predict(Wind_M,newdata = Wind_test1))
Wind_P2 = expm1(predict(Wind_M,newdata = Wind_Test_P))
Biketrain$windspeed[Biketrain$windspeed == 0] <- Wind_P
Biketest$windspeed[Biketest$windspeed == 0] <- Wind_P2

#----------------------------------------------------------------------------------------------
# Extreme Gradient Boosting
#----------------------------------------------------------------------------------------------

cv.ctrl <- trainControl(method = "repeatedcv",number = 5, repeats = 2,
                       verboseIter=T,classProbs= F)

xgb.grid <- expand.grid(nrounds = seq(1,600,by = 5),
                       max_depth = c(6),
                       eta = c(0.1),
                       gamma = c(0),
                       colsample_bytree = (0.45),
                       min_child_weight = c(0),
                       subsample = c(0.9))

Model_R <- train(log1p(registered) ~  month + hour + year + season +
        holiday + workingday + weather + temp +atemp + humidity +
        windspeed +daypart + day_type + week_D +dp_reg + temp_reg
      , data = Biketrain,method="xgbTree",stratified=TRUE,
        verbose = 1, eval_metric = "rmse", preProcess = c('center', 'scale'),
        trControl = cv.ctrl,tuneGrid = xgb.grid)

xgb.grid2 <- expand.grid(nrounds = seq(1,600,by = 5),
                        max_depth = c(5),
                        eta = 0.1,
                        gamma = 0,
                        colsample_bytree = (0.6),
                        min_child_weight = c(0),
                        subsample = c(0.7))

Model_C <- train(log1p(casual) ~  month + year + hour + season +
        holiday + workingday + weather + temp +atemp + humidity +
        windspeed +daypart + day_type + week_D + dp_cas + temp_cas
      , data = Biketrain,method="xgbTree",stratified=TRUE,
        verbose = 1,eval_metric="rmse",preProcess = c('center', 'scale'),
        rControl = cv.ctrl,tuneGrid = xgb.grid2)

#-----------------------------------------------------------------------------------------------------
# Evaluation
#-----------------------------------------------------------------------------------------------------

Eval_R<-ggplot(data = Model_R$results, aes(x = Model_R$results$nrounds )) +
  geom_line(aes(y=Model_R$results$RMSE,color='red'), size=1.25,linetype = "solid") +
  geom_line(aes(y=Model_R$results$MAE,color="steelblue"), size=1.25,linetype = "solid")+
  labs(y ="Score", x = "Boosting Iterations(nrounds)", colour = "Metric") +
  ggtitle(label = "Registered Model Evaluation") + theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(0, 600, by = 75))+
  scale_y_continuous(breaks = seq(0, 4, by = 0.5))+
  scale_color_manual(labels = c("RMSE", "MAE"), values = c("red", "steelblue"))


Eval_C <-ggplot(data = Model_C$results, aes(x = Model_C$results$nrounds )) +
  geom_line(aes(y=Model_C$results$RMSE,color='red'), size=1.25,linetype = "solid") +
  geom_line(aes(y=Model_C$results$MAE,color="steelblue"), size=1.25,linetype = "solid")+
 labs(y ="Score", x = "Boosting Iterations(nrounds)", colour = "Metric") +
  ggtitle(label = "Casual Model Evaluation") +theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(0, 600, by = 75))+
  scale_y_continuous(breaks = seq(0, 4, by = 0.25))+
  scale_color_manual(labels = c("RMSE", "MAE"), values = c("red", "steelblue"))
                
               
#Predicting the numbers of Casual and Registered in the test data

Biketest$casual <-  expm1(predict(Model_C,newdata = Biketest))
Biketest$registered <- expm1(predict(Model_R,newdata = Biketest))
Biketest$count <- (Biketest$casual + Biketest$registered)

#Writing the file

Submit <-data.frame(datetime= sample$datetime,count=Biketest$count)
write.csv(Submit,file="Submit.csv",row.names=FALSE)

