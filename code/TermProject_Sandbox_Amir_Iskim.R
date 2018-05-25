#dirname(rstudioapi::getSourceEditorContext()$path)
setwd("D:/Rdata")
getwd()
#rm(list=ls())




###########################################################
library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(lattice)
library(caret)
library(gplots)
library(ROCR)
library(tree)
library(randomForest)
library(Rcpp)
library(rstanarm)
library(pROC)
library(data.table)
library(R.oo)
library(R.methodsS3)
library(R.utils) 
library(downloader)
library(lubridate)
library(plyr)
library(stats)
library(graphics)


if(!file.exists("repdata-data-StormData.csv.bz2")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "repdata-data-StormData.csv.bz2", method = "libcurl")
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

str(storm1)
storm2 <- storm1


#storm2 <- data.frame(storm1$EVTYPE, storm1$FATALITIES, storm1$INJURIES, storm1$PROPDMG, storm1$PROPDMGEXP, storm1$CROPDMG, storm1$CROPDMGEXP, storm1$Year.Begin)
#colnames(storm2) <- c("Event", "Fatalities", "Injuries", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP", "Year" )



head(storm2)
str(storm2)


## PROPDMG is:  "Property damage in whole numbers and hundredths"

## PROPDMGEXP is:  "A multiplier where Hundred (H), Thousand (K), Million (M), Billion (B)

## CROPDMG is: "Crop damage in whole numbers and hundredths"

## CROPDMGEXP is : "A multiplier where Hundred (H), Thousand (K), Million (M), Billion (B)"

## Exploring the variables levels, it is found: 

levels(storm2$PROPDMGEXP)

## According to the Code Book, this variable is an exponential, and K, M, and B are recognized. "" "-" "? "+" are not recognized and will be deleted.

## I will do this transformations on a new variable

missings <- c("-", "?", "+", "")

storm2$exp.property <- storm2$PROPDMGEXP

storm2 <- storm2[!storm2$exp.property %in% missings,]
storm2$exp.property <- droplevels(storm2$exp.property)


levels(storm2$CROPDMGEXP)

## According to the Code Book, "" "?" are not recognized and will be deleted. I will do this on a new variable for Crop

storm2$exp.crop <- storm2$CROPDMGEXP


storm2 <- storm2[!storm2$exp.crop %in% missings,]
storm2$exp.crop <- droplevels(storm2$exp.crop)


## to confirm the procedure, I explore again levels

levels(storm2$exp.property)

levels(storm2$exp.crop)

## Now, I will proceed to transform the exponentials, to numbers


storm2 <- transform(storm2,
                    exp.property = revalue(storm2$exp.property, c("K" = 3, "m" = 6, "M" = 6, "B" = 9, "h" = 2, "H" = 2)))



storm2 <- transform(storm2,
                    exp.crop = revalue(storm2$exp.crop, c("K" = 3, "k" = 3, "m" = 6, "M" = 6, "B" = 9)))


## Now, I will check if the process was finally done

levels(storm2$exp.property)

levels(storm2$exp.crop)

# In the process of cleaning economic consequences variables, I proceed to build a new variable, combinig variables with their exponentials.
storm2$exp.property <- as.numeric(storm2$exp.property)

storm2$exp.crop <- as.numeric(storm2$exp.crop)

storm2$property.costs <- storm2$PROPDMG^storm2$exp.property

storm2$crop.costs <- storm2$CROPDMG^storm2$exp.crop

storm2$total.costs <- storm2$property.costs + storm2$crop.costs

#Note: Checking if variables were well created
summary(storm2$property.costs)
summary(storm2$crop.costs)
summary(storm2$total.costs)

#First Question: Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
  
#For resolving this question, I show first, how Fatalities and Injuries happened per year, during the period, and after, I show how Fatalities and Injuries happened across United States, per Event.
#1. Estimating total fatalities and injuries during the period, per year
library(data.table)
fatalities.year <- aggregate(storm1$FATALITIES, by = list(storm1$Year.Begin), FUN = sum)
colnames(fatalities.year) <- c("Year", "Fatalities")
head(fatalities.year)

injuries.year <- aggregate(storm1$INJURIES, by = list(storm1$Year.Begin), FUN = sum)
colnames(injuries.year) <- c("Year", "Injuries")
head(injuries.year)

morbimortality.year <- data.frame(cbind(fatalities.year, injuries.year))




## total cases of mortality and morbidity per year

morbimortality.year$Total <- as.data.table(morbimortality.year$Fatalities + morbimortality.year$Injuries)
head(morbimortality.year)

# 2. Estimating total fatalities and injuries, per event type

fatalities.event <- aggregate(storm1$FATALITIES, by = list(storm1$EVTYPE), FUN = sum)
colnames(fatalities.event) <- c("Event", "Fatalities")
fatalities.event.sorted <- fatalities.event[order(-fatalities.event$Fatalities, na.last=TRUE), ][1:30, ]
head(fatalities.event.sorted)

injuries.event <- aggregate(storm1$INJURIES, by = list(storm1$EVTYPE), FUN = sum)
colnames(injuries.event) <- c("Event", "Injuries")
injuries.event.ordered <- injuries.event[order(-injuries.event$Injuries, na.last=TRUE), ][1:30, ]
injuries.event.ordered


# Now the plot with fatalities and injuries per year

# Figure 1. “Weather events most harmful and Fatalities/Injuries Trend, per year, USA, 1950 -2011”

##par = c(mfrow = c(1,1), margin = c(50,2,2,1))
par(mfrow = c(2, 2), mar = c(11.5, 5, 4, 2), las = 3, cex = 0.5, cex.main = 1.4, cex.lab = 1.2)

with(fatalities.event.sorted, barplot(Fatalities, names.arg = fatalities.event.sorted$Event, col = "red", ylab = "Fatalities", main = "Total Fatalities"))

with(injuries.event.ordered, barplot(Injuries, names.arg = injuries.event.ordered$Event, col = "green", ylab = "Injuries", main = "Total Injuries"))

## Fatalities related with Weather events, USA, 1950-2011
reg <- lm(Fatalities ~ Year, data = morbimortality.year)
with(morbimortality.year, plot(Year, Fatalities, ylab = "Fatalities", xlim = c(1945, 2015), ylim = c(0,1200), pch = 19, col ="red", main = "Total Fatalities per year"))
par(xpd = FALSE)
abline(reg, col="black")

## Injuries related with Weather events, USA, 1950-2011
reg1 <- lm(Injuries ~ Year, data = morbimortality.year)
with(morbimortality.year, plot(Year, Injuries, ylab = "Injuries", xlim = c(1945, 2015), ylim = c(0,9000),  pch = 19, col = "green", main = "Total Injuries per year"))
par(xpd = FALSE)
abline(reg1, col="black")

# Estimation of total amount of fatalities and injuries during the whole period

totalfatalities <- sum(morbimortality.year$Fatalities)
totalinjuries <- sum(morbimortality.year$Injuries)
ratioif <-  totalinjuries/totalfatalities


# Second Question: Across the United States, which types of events have the greatest economic consequences?

# 1. Related total costs of weather events per year
costs.year <- aggregate(storm2$total.costs, by = list(storm2$Year.Begin), FUN = sum)
colnames(costs.year) <- c("Year", "Costs")
head(costs.year)

# 2. Related total costs per event
total.costs.event <- aggregate(storm2$total.costs, by = list(storm2$EVTYPE), FUN = sum)
colnames(total.costs.event) <- c("Event", "Costs")
head(total.costs.event) 

costs.event.ordered <- total.costs.event[order(-total.costs.event$Costs, na.last=TRUE), ][1:30, ]
head(costs.event.ordered)

#Finally, plot of the relationship between total costs with total morbimortality
morbimortality.year$Total <- morbimortality.year$Fatalities + morbimortality.year$Injuries

total.sum <- sum(morbimortality.year$Total)

head(morbimortality.year[,-3])


costs.sum <- sum(total.costs.event$Costs)

sum.costs <- sum(costs.event.ordered$Costs[1:5])

cost5ratio <- (sum.costs/costs.sum)*100 

# Now, I merge both files (morbimortality and costs per year) in a data.frame, to facilitate plotting them together in the next figure
morbimortality.year <-  as.data.table(morbimortality.year)

total.costs.year <- as.data.table(costs.year)

df <- merge(morbimortality.year, costs.year, by = "Year")

head(df[,-3])

# Figure 2. Economic consequences of weather events, per year and per event, USA, 1950-2011

par(mfrow = c(2, 2), mar = c(11.5, 5, 4, 2), las = 3, cex = 0.5, cex.main = 1.4, cex.lab = 1.2)

reg2 <- lm(log10(Costs) ~ Year, data = df)
with(df, plot(log10(Costs) ~ Year, pch = 19, col = "red", ylab = "Total Log(Costs)", main = "Log10(Costs) per Year"))
abline(reg2, col = "black")

## log10(Total costs) per event 

with(costs.event.ordered, barplot(log10(Costs), names.arg = costs.event.ordered$Event, col = "red", ylab = "log10(Costs)", main = "Total Costs per event type"))

## Log(Costs) of Fatalities

with(df, scatter.smooth(Fatalities, log10(Costs), pch = 20, col = "red", main = "Log10(Costs) of Fatalities"))
?scatter.smooth
## Log(Costs) of Injuries

with(df, scatter.smooth(Injuries, log10(Costs), pch = 20, col = "green", main = "Log10(Costs) of Injuries"))

head(df)
head(storm2)

df2 = df[,-3]
head(df2)

rf = randomForest(Costs~., data=df2, importance =T)

print(rf)  # it is normal to have a different value because each computer has diferent random value 
plot(rf, main="Random Forest Regression for Boston")

length(rf$mse)
min(rf$mse)
mean(rf$mse)
head(rf$mse)
tail(rf$mse) 

n <- which.min(rf$mse)  # give me the index which mse error is minimum
n
rf2 <- randomForest(Costs~., data=df2, 
                    ntree=n, importance=TRUE, na.action=na.omit)  # now you can say the number of trees is the optimal one we found
?randomForest
print(rf2)
# Importance of variables: higher value mean more important
#  this importance will give you which feature is important in deciding this tree, the features at the top are important because bigger dimension/ which features come to the top ? (important) /   IncMSE: if you don't use this feature how much error will increase in your dataset / rm has the highest values, number of rooms is so important (most people use the once) / IncNodePurity (how much subtree really about that tree for example rm has the most sub trees and so important)
importance(rf2)    
varImpPlot(rf2, scale=T, main = "Variable Importance Plot")

head(df2)
par(mfrow=c(1,1))
par(fig=c(0.1,0.7,0.3,0.9))
par(cex=0.7, mai=c(0.1,0.1,0.2,0.1))
quartz("decision tree", 10,10)
dev.off()
?quartz
head(df2)
library(rpart)
rfit = rpart(Year~., data=df2, method="anova", control = rpart.control(minsplit=10))
#rfit
# predictors age maiscreat, .... 
#png("tree.png", width=1000, height=800, antialias="cleartype")
plot(rfit, uniform=T, main="reg", ,margin=0.2)
text(rfit,   use.n=TRUE, all= T, cex=0.8) 


