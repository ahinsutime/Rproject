rm(list=ls())
dev.off()
#setwd("C:/Rdata")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
curdir=dirname(rstudioapi::getActiveDocumentContext()$path)
#install.packages("githubinstall")
#library(githubinstall)
#githubinstall("https://github.com/awesomedata/awesome-public-datasets")

############1.Preparing data###################
#**(1)Install and load packages----------------
#install & load
#if you have already installed the packages only run line 12 and skip up to line 20
packages <- c("R.utils", "data.table", "downloader", "lubridate", "plyr","dplyr")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)
lapply(packages, require,character.only=T)
#**(2)Download data----------------
#fread is much much much faster than read. Let's use fread to read csv datasets
if(!file.exists("repdata-data-StormData.csv.bz2")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "repdata-data-StormData.csv.bz2", method = "curl")
}
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
bz2.data <- "repdata_data_StormData.csv.bz2"
download(url, bz2.data, mode = "wb")  
bunzip2(bz2.data, "repdata-data-StormData.csv", overwrite=TRUE, remove = FALSE)
storm <- fread("repdata-data-StormData.csv", sep = ",", header = TRUE)
storm1 = storm
#**(3)Process data----------------
#subset relevant columns
#remove county data, remarks, refnum
storm2 <- storm1[,-c(5:6,14:15,31,36:37)]
#**(5)change "","-","?" to NA & "+" to 1 ---------------------------
storm2[ storm2 == "" ] <- NA
storm3=storm2
storm3[ storm3 == "+" ] <- "O"
storm4=storm3
storm4[ storm4 == "-" ] <- NA
storm5=storm4
storm5[ storm5 == "?" ] <- NA
na_count <-sapply(storm2, function(y) sum(length(which(is.na(y)))))
na_count2 <-sapply(storm3, function(y) sum(length(which(is.na(y)))))
na_count3 <-sapply(storm4, function(y) sum(length(which(is.na(y)))))
na_count4 <-sapply(storm5, function(y) sum(length(which(is.na(y)))))
na_count_tab <- data.frame(na_count,na_count2,na_count3,na_count4)
na_count_tab
############2.Preparing data###################
##resources:
#http://rstudio-pubs-static.s3.amazonaws.com/25710_7f98ce257ba941a3a9bd5e73186dc730.html
#https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html
#**(1)Basic statistics
storm5
#**(2)Which events where the most fatals
storm5<-transform(storm5, EVTYPE = factor(EVTYPE)) #EVTYPE as factors
storm5$EVTYPE <- toupper(storm5$EVTYPE) #upper case all characters for consistency
unique(storm5$EVTYPE)#total of 898 events
#Group event types by contents containing keywords
storm5$EVTYPE[grep("*TORNADO*", storm5$EVTYPE)] <- "TORNADO"
storm5$EVTYPE[grep("*HURRICANE*|*TYPHOON*", storm5$EVTYPE)] <- "HURRICANE"
storm5$EVTYPE[grep("*WIND*", storm5$EVTYPE)] <- "WIND"
storm5$EVTYPE[grep("*FIRE*", storm5$EVTYPE)] <- "FIRE"
storm5$EVTYPE[grep("*STORM*|*GLAZE*|*HAIL*|*WETNESS*|*LIGHTNING*|*RAIN*|*BLIZZARD*", storm5$EVTYPE)] <- "STORM"
storm5$EVTYPE[grep("*COLD*|*LOW TEMPERATURE*|*WINTRY*|*FREEZE*", storm5$EVTYPE)] <- "COLD"
storm5$EVTYPE[grep("*SNOW*", storm5$EVTYPE)] <- "SNOW"
storm5$EVTYPE[grep("*FLOOD*|*STREAM*|*HIGH WATER*", storm5$EVTYPE)] <- "FLOOD"
storm5$EVTYPE[grep("*HEAT*|*HOT*", storm5$EVTYPE)] <- "HEAT"
storm5$EVTYPE[grep("*SURF*|*SEAS*|*MARINE*|*CURRENT*|*TSUNAMI", storm5$EVTYPE)] <- "SURF"
storm5$EVTYPE[grep("*FOG*", storm5$EVTYPE)] <- "FOG"
storm5$EVTYPE[grep("*DRY*|*DROUGHT*", storm5$EVTYPE)] <- "DRY"
storm5$EVTYPE[grep("*LANDSLIDE*|*LAND*|*AVALANCHE*|*SLIDE*", storm5$EVTYPE)] <- "LANDSLIDE"
storm5$EVTYPE[grep("*ICE*|*ICY*|*FROST*", storm5$EVTYPE)] <- "ICE"
#**(3)Unite propdmg with propdmgexp and corpdm with cropdmgexp
unique(storm5$CROPDMGEXP) #check unique values
unique(storm5$PROPDMGEXP) #check unique values
#**(4)transform characters to numeric values
#first, transform all characters to uppercase for consistency
storm5$CROPDMGEXP <- toupper(storm5$CROPDMGEXP)
storm5$PROPDMGEXP <- toupper(storm5$PROPDMGEXP)
#second, unite columns
storm6 =storm5
unique(storm6$PROPDMGEXP)
storm6 <- transform(storm6,
                    PROPDMGEXP = revalue(storm6$PROPDMGEXP, c("0" = "T","1" = "T", "2" = "T", "3" = "T", "4" = "T", "5" = "T", "6" = "T", "7" ="T", "8"="T")))
storm6$PROPDMG_t <- ifelse(storm6$PROPDMGEXP == "T", 1e1*storm6$PROPDMG,
                           ifelse(storm6$PROPDMGEXP== "H", 1e2*storm6$PROPDMG,
                                  ifelse(storm6$PROPDMGEXP == "K", 1e3*storm6$PROPDMG,
                                         ifelse(storm6$PROPDMGEXP == "M", 1e6*storm6$PROPDMG,
                                                ifelse(storm6$PROPDMGEXP == "B", 1e9*storm6$PROPDMG,
                                                       ifelse(storm6$PROPDMGEXP == "O", 1*storm6$PROPDMG,0))))))
unique(storm6$CROPDMGEXP)
storm6 <- transform(storm6,
                    CROPDMGEXP = revalue(storm6$CROPDMGEXP, c("0" = "T", "2" = "T")))
storm6$CROPDMG_t <- ifelse(storm6$CROPDMGEXP == "T", 1e1*storm6$CROPDMG,
                           ifelse(storm6$CROPDMGEXP== "H", 1e2*storm6$CROPDMG,
                                  ifelse(storm6$CROPDMGEXP == "K", 1e3*storm6$CROPDMG,
                                         ifelse(storm6$CROPDMGEXP == "M", 1e6*storm6$CROPDMG,
                                                ifelse(storm6$CROPDMGEXP == "B", 1e9*storm6$CROPDMG,
                                                       ifelse(storm6$CROPDMGEXP == "O", 1*storm6$CROPDMG,0))))))
storm6$DMG_t <- storm6$CROPDMG_t + storm6$PROPDMG_t
View(storm6)
storm7 <- storm6[,-c(21:24)]
View(storm7)
#**(5)Extract Year--------------------------------
#Begin data as the criterion for year extraction
storm7$Begin.Time <- as.POSIXct(strptime(storm7$BGN_DATE, format ="%m/%d/%Y %H:%M:%S"))
storm7$Year.Begin <- year(as.Date(storm7$Begin.Time))
storm8 <- storm7[,-c(1:4,7:14,20,22,30)]
View(storm8)


#**(6)Merge with population data--------------------------------
curdir=dirname(rstudioapi::getActiveDocumentContext()$path)
curdir
projectdir=(dirname(curdir))
projectdir
datadir=paste(projectdir, "/additionalData", sep="")
#load new data
?read.csv
datadir
addi_data<-read.csv(paste(datadir,"/state level/statedata.csv", sep=""), sep = ",", header = T)
addi_data_2 <- addi_data[,-1]
#change column names of both datas
colnames(addi_data_2) <- c("STATE","YEAR","rgdp","pop")
addi_data_2
colnames(storm8)[1] <- "STATE"
colnames(storm8)[16] <- "YEAR"
storm9<-merge(storm8, addi_data_2, by=c("STATE","YEAR"), all.x = T) 
View(storm9)
#add annual weather data
annual_temp <- read.csv(paste(datadir,"/weather/annual_temp.csv", sep=""), sep = ",", header = T)
annual_temp_2 <- annual_temp[,-1]
colnames(annual_temp_2) <- c("STATE","YEAR","tavg","tmax","tmin","pcp")
head(storm9)
head(annual_temp_2)
storm_f<-merge(storm9, annual_temp_2, by=c("STATE","YEAR"), all.x = T) 
#change data structure
storm_f$STATE<-as.factor(storm_f$STATE)
storm_f$YEAR<-as.factor(storm_f$YEAR)
storm_f$EVTYPE<-as.factor(storm_f$EVTYPE)
storm_f$F<-as.factor(storm_f$F)
storm_f$WFO<-as.factor(storm_f$WFO)
str(storm_f)
write.csv(
  storm_f,              
  file="storm_f.csv",        
  row.names=TRUE  
)
###############################################################################################


####Actual Code from Iskim

library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(caret)
library(ROCR)
library(tree)
library(randomForest)
library(rstanarm)
library(pROC)

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

# Figure 1. ?€œWeather events most harmful and Fatalities/Injuries Trend, per year, USA, 1950 -2011?€?

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

plot(rfit, uniform=T, main="reg", ,margin=0.2)
text(rfit,   use.n=TRUE, all= T, cex=0.8) 

library(bnlearn)
#learning a beysian network from data
bnhc <- hc(df2, score="bic-g")# based on gausian 
#bnhc <- hc(marks, score="bic")# not work because it is for factor data
bnhc
bnhc$nodes
bnhc$arcs

library(igraph)
edges=arcs(bnhc)
nodes=nodes(bnhc)
net <- graph.data.frame(edges,directed=T,vertices=nodes)
plot(net,vertex.label=V(net)$name,vertex.size=40,
     edge.arrow.size=0.3,vertex.color="cyan",
     edge.color="black")


library(PerformanceAnalytics) # look at how they are related, they are kind of related by with correlation you cannot find which one is the reason for the other one
chart.Correlation(marks,pch=21,histogram=TRUE)

str(storm2)

colIndex=c( #which(colnames(storm2) == "LATITUDE"),
  #which(colnames(storm2) == "LONGITUDE"),
  which(colnames(storm2) == "CROPDMG"),
  which(colnames(storm2) == "INJURIES"),
  which(colnames(storm2) == "FATALITIES"),
  #which(colnames(storm2) == "PROPDMG"),
  #which(colnames(storm2) == "STATE"),
  which(colnames(storm2) == "EVTYPE"))


storm3 = storm2[,colIndex]
str(storm3)
storm3
# having continues and factor, we have to use different set of functions
library(deal) # package for having combination factor and continues values
str(storm3)
storm3.nw <- network(storm3)          # specify prior network

storm3.prior <- jointprior(storm3.nw) # make joint prior distribution
# learn estimate parameters
storm3.nw <- learn(storm3.nw,storm3,storm3.prior)$nw
storm3.nw

result <- heuristic(initnw=storm3.nw, data=storm3, prior=storm3.prior,
                    restart=2, degree=10, trace=FALSE)
win.graph(width=7,height=7)
plot(getnetwork(result))

# # why some nodes are black, blacks are factors, and white ones are continues
# # look at BMI effected by gender, kol (kolestrol)
# #page 18
# library(igraph)  # get adjency matrix and plot it again
# mx <- matrix(c(0,0,1,0,0,0,0,0,0, 0,0,0,1,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,
#                0,0,1,0,0,0,0,0,0, 1,0,0,0,0,1,0,0,0, 0,0,0,0,0,0,1,0,0,
#                0,0,0,0,0,0,0,1,0, 1,1,0,1,1,1,0,0,0, 1,1,0,0,1,1,1,0,0),
#              nrow=9, byrow=T, dimnames=list(NULL,names(ksl)))
# knet <- graph.adjacency(mx)
# V(knet)$color <- c(rep("cornsilk",4),rep("cyan",5))
# plot(knet,edge.arrow.size=0.4,edge.color="gray47",vertex.label.color="black",
#      vertex.label.cex=1.2,vertex.size=30,layout=layout.circle)

