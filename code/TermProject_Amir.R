dirname(rstudioapi::getSourceEditorContext()$path)
setwd("/Users/Amirsam/Documents/GitHub/")
getwd()
#rm(list=ls())




###########################################################
library(ggplot2)
library(dplyr)
library(readr)
#install.packages("corrplot")
library(corrplot)
library(lattice)
library(caret)
#install.packages("ROCR")
library(gplots)
library(ROCR)
library(tree)
library(randomForest)
#install.packages("rstanarm")
#install.packages("ggridges")
library(Rcpp)
library(rstanarm)
library(pROC)


library(data.table)
install.packages("R.utils")
library(R.oo)
library(R.methodsS3)
library(R.utils) 
install.packages("downloader")
library(downloader)
library(lubridate)
library(plyr)
library(stats)
library(graphics)
library(lattice)
library(ggplot2)


if(!file.exists("repdata-data-StormData.csv.bz2")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "repdata-data-StormData.csv.bz2", method = "curl")
}

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
bz2.data <- "repdata_data_StormData.csv.bz2"
download(url, bz2.data, mode = "wb")  

bunzip2(bz2.data, "repdata-data-StormData.csv", overwrite=TRUE, remove = FALSE)

storm <- read.table("repdata-data-StormData.csv", sep = ",", header = TRUE)


storm1 = storm



storm1$Begin.Time <- as.POSIXct(strptime(storm1$BGN_DATE, format ="%m/%d/%Y %H:%M:%S"))

##storm1$End.Time <- as.POSIXct(strptime(storm1$END_DATE, format="%m/%d/%Y %H:%M:%S"))

storm1$Year.Begin <- year(as.Date(storm1$Begin.Time))



## Changing to AVALANCHE
storm1 <- transform(storm1,
                    EVTYPE = revalue(storm1$EVTYPE, c("AVALANCE" = "Avalanche", "AVALANCHE" = "AVALANCHE")))

## Changing to Thunderstorm Wind"

storm1 <- transform(storm1,
                    EVTYPE = revalue(storm1$EVTYPE, c("TSTM WIND" = "THUNDERSTORM WIND")))

## Changing to Rip Current

storm1 <- transform(storm1,
                    EVTYPE = revalue(storm1$EVTYPE, c("RIP CURRENTS" = "RIP CURRENT")))

## Changing to Extreme Cold/Wind Chill

storm1 <- transform(storm1,
                    EVTYPE = revalue(storm1$EVTYPE, c("EXTREME COLD" = "EXTREME COLD/WIND CHILL", "EXTREME WINDCHILL" = "EXTREME COLD/WIND CHILL")))

## Changing to Excessive Heat

storm1 <- transform(storm1,
                    EVTYPE = revalue(storm1$EVTYPE, c("EXTREME HEAT" = "EXCESSIVE HEAT", "RECORD/EXCESSIVE HEAT" = "EXCESSIVE HEAT")))

## Changing to Hurricane/Typhoon cathegory

storm1 <- transform(storm1,
                    EVTYPE = revalue(storm1$EVTYPE, c("HURRICANE" = "HURRICANE/TYPHOON")))

## Changing to High Surf

storm1 <- transform(storm1,
                    EVTYPE = revalue(storm1$EVTYPE, c("HEAVY SURF/HIGH SURF" = "HIGH SURF")))

## Changing to the new name "Debris Flow"

storm1 <- transform(storm1,
                    EVTYPE = revalue(storm1$EVTYPE, c("LANDSLIDE" = "DEBRIS FLOW")))

## Changing to Cold/Wind Chill cathegory

storm1 <- transform(storm1,
                    EVTYPE = revalue(storm1$EVTYPE, c("COLD" = "COLD/WIND CHILL")))

## Changing to High Wind

storm1 <- transform(storm1,
                    EVTYPE = revalue(storm1$EVTYPE, c("HIGH WINDS" = "HIGH WIND")))

## Changing to Heat

storm1 <- transform(storm1,
                    EVTYPE = revalue(storm1$EVTYPE, c("UNSEASONABLY WARM AND DRY" = "HEAT")))

## Changing to Heavy Rain, according to the document

storm1 <- transform(storm1,
                    EVTYPE = revalue(storm1$EVTYPE, c("URBAN/SML STREAM FLD" = "HEAVY RAIN")))

## Changing to Winter Weather

storm1 <- transform(storm1,
                    EVTYPE = revalue(storm1$EVTYPE, c("WINTER WEATHER/MIX" = "WINTER WEATHER")))



## storm2 <- data.frame(storm1$EVTYPE, storm1$FATALITIES, storm1$INJURIES, storm1$PROPDMG, storm1$PROPDMGEXP, storm1$CROPDMG, storm1$CROPDMGEXP, storm1$Year.Begin)
## colnames(storm2) <- c("Event", "Fatalities", "Injuries", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP", "Year" )

storm2 <- storm1

head(storm2)






pima = read_csv("WeatherEvents.csv",)
str(pima)
summary(pima)
head(pima,2)

colnames(pima) = c("Pregnant","Plasma_Glucose","Dias_BP","Triceps_Skin","Serum_Insulin","BMI","DPF","Age","Diabetes")

#Lets count the no. of NA values
sapply(pima, function(x) sum(is.na(x)))

#Lets look at a basic summary for the variables
summary(pima)


#Convert the Diabetes variable into a factor
pima$Diabetes <- as.factor(pima$Diabetes)

#Remove entry if Glucose,BP or BMI == 0
pima <- pima[apply(pima[,c(2,3,6)],1,function(x) !any(x==0)),]




#####################################################
#Data Visualization

gluc_mean <- pima %>% group_by(Diabetes) %>% summarise(Plas = round(mean(Plasma_Glucose),2))

#Relationship between Plasma Glucose & Diabetes
ggplot(data=pima,aes(Diabetes,Plasma_Glucose)) + 
  geom_boxplot(aes(fill=Diabetes)) + stat_boxplot(geom = "errorbar") + 
  ggtitle("Diabetes rates against Plasma Glucose Levels") + 
  xlab("Diabetes") + ylab("Plasma Glucose") + guides(fill=F) + 
  geom_text(data = gluc_mean, aes(x=Diabetes,y=Plas,label=Plas),
            hjust = -1.5,vjust=-0.5)

ins_mean <- pima %>% group_by(Diabetes) %>% summarise(Plas = round(mean(Serum_Insulin),2))

#Relationship between Diabetes & Serum Insulin levels
ggplot(data=pima,aes(Diabetes,Serum_Insulin)) + 
  geom_boxplot(aes(fill=Diabetes)) + stat_boxplot(geom = "errorbar") + 
  ggtitle("Diabetes rates against Serum Insulin Levels") + 
  xlab("Diabetes") + ylab("Serum Insulin") + guides(fill=F) + 
  geom_text(data = ins_mean, aes(x=Diabetes,y=Plas,label=Plas),
            hjust = -1.5,vjust=-0.5)


#Difference in Blood Pressure & BMI for Diabetics
ggplot(data = pima,aes(Dias_BP,BMI)) + geom_point(aes(colour=Diabetes),alpha=0.6) +
  xlab("Diastolic Blood Pressure") + ylab("Body Mass Index") + 
  ggtitle("Interaction between Blood Pressure & BMI for Diabetics") + 
  labs(colour="Diabetes Status") + 
  scale_colour_manual(values = c("#D55E00", "#009E73"))

#Relationship between pregnancy and diabetes
ggplot(pima, aes(Pregnant, fill = Diabetes)) +
  geom_density() + ylab("Distribution of Pregnancy") + 
  ggtitle("Pregnant Women vs. the threat of Diabetes")

#Correlelogram to help remove variables which may be correlated to one another
corrplot(cor(pima[,-9]),type = "lower", method = "number")


######################################################
#Logistic Regression

set.seed(15689)
index <- createDataPartition(pima$Diabetes,p = 0.7,list = F)
train <- pima[index,]
test  <- pima[-index,]

#Logistic Regression

m1 <- glm(Diabetes ~ ., data = train, family = binomial(link = "logit"))
summary(m1)

anova(m1,test = "Chisq")


#Remodel using only the significant variables
mod_fin <- glm(Diabetes ~ Pregnant + Plasma_Glucose + Triceps_Skin + BMI + DPF,
               data = train, family = binomial(link = "logit"))
summary(mod_fin)


#Residuals 
summary(residuals(mod_fin))


par(mfrow=c(2,2))
plot(mod_fin)


#Apply the model to the testing sample
test_pred <- predict(mod_fin,test, type = "response")
pred_test <- as.data.frame(cbind(test$Diabetes,test_pred))
colnames(pred_test) <- c("Original","Test_pred")
pred_test$outcome <- ifelse(pred_test$Test_pred > 0.5, 1, 0)
error <- mean(pred_test$outcome != test$Diabetes)
print(paste('Test Data Accuracy', round(1-error,2)*100,'%'))

confusionMatrix(test$Diabetes,pred_test$outcome)



acc_lg <- confusionMatrix(test$Diabetes,pred_test$outcome)$overall['Accuracy']

# Get the ROC curve and the AUC
par(mfrow=c(1,1))
plot.roc(test$Diabetes,test_pred,percent=TRUE,col="#1c61b6",print.auc=TRUE,
         main = "Area under the curve for Logistic Regression")



#####################################################
#Bayesian Logistic Regression

#Bayesian Logistic Regression
prior_dist <- student_t(df = 7, location = 0, scale = 2.5)
bayes_mod  <- stan_glm(Diabetes ~ ., data = train,
                       family = binomial(link = "logit"), 
                       prior = prior_dist, prior_intercept = prior_dist,
                       seed = 15689)


#Confidence Intervals for the predictors
posterior_interval(bayes_mod, prob = 0.95)

#Residuals for the Bayesian Model
summary(residuals(bayes_mod))


bayes_res <- data.frame(residuals(bayes_mod))
bayes_res$index <- seq.int(nrow(bayes_res)) 
colnames(bayes_res) <- "Residuals"

#Plotting the residuals
ggplot(data = bayes_res,aes(index,Residuals)) + geom_point() + ggtitle("Representation of randomness amongst Residuals")


ggplot(data = bayes_res,aes(Residuals)) + geom_density(aes(fill=Residuals)) + 
  ylab("Density") + ggtitle("Distribution of Residuals")


#Predicting Probabilities for the test data
pred <- posterior_linpred(bayes_mod, newdata = test, transform=TRUE)
fin_pred <- colMeans(pred)
test_prediction <- as.integer(fin_pred >= 0.5)

confusionMatrix(test$Diabetes,test_prediction)


acc_bayes <- confusionMatrix(test$Diabetes,test_prediction)$overall['Accuracy']

plot.roc(test$Diabetes,fin_pred,percent=TRUE,col="#1c61b6", print.auc=TRUE,
         main = "Area under the curve for Bayesian Logistic Regression")



#####################################################
#Decision Trees & Random Forests

#Decision Trees
set.seed(15689)
m_dt <- tree(Diabetes ~ ., data = train)
pred_dt <- predict(m_dt, train, type = "class")
confusionMatrix(train$Diabetes,pred_dt)[2:3]


plot(m_dt)
text(m_dt, pretty = 0)


pred_dt_test <- predict(m_dt, test, type = "class")
confusionMatrix(test$Diabetes,pred_dt_test)


acc_dt <- confusionMatrix(pred_dt_test,test$Diabetes)$overall['Accuracy']

#Random Forest
set.seed(15689)

opt_mod <- tuneRF(train[-as.numeric(ncol(train))],train$Diabetes,ntreeTry = 150, 
                  stepFactor = 2, improve = 0.05,trace = T, plot = T, doBest = F)



mtry_fin <- opt_mod[as.numeric(which.min(opt_mod[,"OOBError"])),"mtry"]

rf_fin <- randomForest(Diabetes~.,data=train, mtry=mtry_fin, ntree=101, 
                       keep.forest=TRUE, proximity=TRUE, importance=TRUE,test=test)

pred_test <- predict(rf_fin, newdata = test)
confusionMatrix(test$Diabetes,pred_test)



acc_rf <- confusionMatrix(test$Diabetes,pred_test)$overall['Accuracy']

par(mfrow=c(1,2))
varImpPlot(rf_fin,type = 2,main = "Variable Importance",col = 'black')
plot(rf_fin,main = "Error vs no. of trees grown")


#####################################################
#Model Comparison

accuracy <- data.frame(Model=c("Logistic","Bayesian Logistic","Decision Tree","Random Forest"),Accuracy=c(acc_lg,acc_bayes,acc_dt,acc_rf))

ggplot(accuracy,aes(x=Model,y=Accuracy))+geom_bar(stat='identity')+theme_bw()+
  ggtitle('Comparison of Model Accuracy')




