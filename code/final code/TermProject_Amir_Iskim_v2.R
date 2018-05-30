
setwd('C:/Users/Bazinjc/Desktop/GitHub')
getwd()
rm(list=ls())
dev.off()

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
storm8 <- storm7[,-c(1:4,7:14,21,22,30)] # I changed 20 to 21 to have Injuries in our columns
str(storm8)
View(storm8)
#**(6)Merge with population data--------------------------------
#load new data
addi_data<-read.csv("statedata.csv", sep = ",", header = T)
addi_data_2 <- addi_data[,-1]
#change column names of both datas
colnames(addi_data_2) <- c("STATE","YEAR","rgdp","pop")
addi_data_2
colnames(storm8)[1] <- "STATE"
colnames(storm8)[16] <- "YEAR"
storm9<-merge(storm8, addi_data_2, by=c("STATE","YEAR"), all.x = T) 
View(storm9)
###############################################################################################
#Find correlation
#t test
#Linear regression
#ANOVA
#MANOVA
#tree, random forest
#Clustering
#Association Rule
#Visualization, geo locations (map)

##############################start the code from here #######################
# we = Amirsaman and Insu
# we fixed missing Injuries in the dataset, and we saved a new Rda file in the folder
# we added new columns in the data like Morbimortality + some columns based on our needs during the code
# we did Random forest, barplot, variable importance plot
# we defined 5 research question and the answers are provided below questions


rm(list=ls())
#install & load
#if you have already installed the packages only run line 12 and skip up to line 20
packages <- c("R.utils", "data.table", "downloader", "lubridate", "plyr","dplyr","rstudioapi","randomForest","tree","party")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)
lapply(packages, require,character.only=T)

# set directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
curdir=dirname(rstudioapi::getActiveDocumentContext()$path)
datadir <- paste(curdir, "/final code", sep="")
setwd(datadir)
storm_f=readRDS('storm_f2.Rda')
str(storm_f)

#storm_f$INJURIES = storm8$INJURIES
# saveRDS(storm_f,file = 'storm_f2.Rda')
# write('storm_f.Rda')
# ?write

##########Rresearch Questions #######

#** (1) Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful (most Fatalities and Injuries)? --------
#  we can answer this question by sorting fatalities and injuries in decreasing manner for each type of event
#** Answer: TORNADO caused most number of fatalities, WIND caused most number of INJURIES and TOTAL (FATALITIES+INJURIES) 

#--------- preprosesing data for our purpose

## storm8 <- storm7[,-c(1:4,7:14,21,22,30)] # I changed 20 to 21 to have Injuries in our columns in line # 113 of code
## str(storm9)
## storm9<-transform(storm9, YEAR = as.factor(YEAR)) #YEAR as factors
## storm9<-transform(storm9, EVTYPE = as.factor(EVTYPE)) #Event type as factors
## columns which are  used in analysis

#** required columns in dataset -----
required_columns = c("STATE","YEAR","EVTYPE","LENGTH","WIDTH","F",
                     "MAG","FATALITIES","LATITUDE","LONGITUDE", "PROPDMG_t",
                     "CROPDMG_t","DMG_t","rgdp","pop",
                     "tavg","tmax","tmin","pcp", "INJURIES")

Indx = c()
for (coln in required_columns)
{
   Indx = c (Indx, which( colnames(storm_f) == coln) )
}
Indx
str(storm_f)

storm = storm_f[,c(1,2,3,4,5,6,7,8,10,11,14,15,16,17,18,19,20,21,22,23)] # taking useful columns in our analysis
str(storm)


#**  Estimating total fatalities and injuries per event type ----

# Aggregate Fatalities by event type
fatalities.event <- aggregate(storm$FATALITIES, by = list(storm$EVTYPE), FUN = sum)
colnames(fatalities.event) <- c("Event", "Fatalities")
fatalities.event.sorted <- fatalities.event[order(-fatalities.event$Fatalities, na.last=TRUE), ]
head(fatalities.event.sorted)

# Aggregate Injuries by event type
injuries.event <- aggregate(storm$INJURIES, by = list(storm$EVTYPE), FUN = sum)
colnames(injuries.event) <- c("Event", "Injuries")
injuries.event.sorted <- injuries.event[order(-injuries.event$Injuries, na.last=TRUE), ]
head(injuries.event.sorted)

# Combine Injuries and Fatalities 
morbimortality.event <- data.frame(cbind(fatalities.event, injuries.event))[,-3]

str(morbimortality.event)
morbimortality.event$Total <- as.data.table( morbimortality.event$Fatalities + morbimortality.event$Injuries )
morbimortality.event$Total <- apply( morbimortality.event[,2:3],1,sum )
colnames(morbimortality.event)[4] = "Total"
morbimortality.event.sorted <- morbimortality.event[order(-morbimortality.event$Total, na.last=TRUE), ]
head(morbimortality.event.sorted)


# barplot of Fatalities and Injuries by event type
nr = 15 # just show 15 top values
par(mfrow = c(3, 1), mar = c(11.5, 5, 4, 2), las = 3, cex = 0.5, cex.main = 1.4, cex.lab = 1.2)
with(fatalities.event.sorted[1:nr,], barplot(Fatalities, names.arg = fatalities.event.sorted$Event[1:nr], col = "red", ylab = "Fatalities", main = "Fatalities - Event"))
with(injuries.event.sorted[1:nr,], barplot(Injuries, names.arg = injuries.event.sorted$Event[1:nr], col = "green", ylab = "Injuries", main = "Injuries - Event"))
with(morbimortality.event.sorted[1:nr,], barplot(Total, names.arg = injuries.event.sorted$Event[1:nr], col = "blue", ylab = "Total", main = "Total (Fatalities + Injuries) - Event"))



#** (2) Across the United States, which year was the most harmful year regarding the number of Fatalities and Injuries?--------
#   we can answer this question by sorting fatalities and injuries in decreasing manner for each year
#** Answer: People of USA at 1995 experienced most number of fatalities from natural events, and at 2011 experienced most number of INJURIES and TOTAL (FATALITIES+INJURIES) 
#**  Estimating total fatalities and injuries per year ----

# Aggregate Fatalities by year
fatalities.year <- aggregate(storm$FATALITIES, by = list(storm$YEAR), FUN = sum)
colnames(fatalities.year) <- c("Year", "Fatalities")
fatalities.year.sorted <- fatalities.year[order(-fatalities.year$Fatalities, na.last=TRUE), ]
head(fatalities.year.sorted)
# Aggregate Injuries by year
injuries.year <- aggregate(storm$INJURIES, by = list(storm$YEAR), FUN = sum)
colnames(injuries.year) <- c("Year", "Injuries")
injuries.year.sorted <- injuries.year[order(-injuries.year$Injuries, na.last=TRUE), ]
head(injuries.year.sorted)

# Combine Injuries and Fatalities by year
morbimortality.year <- data.frame(cbind(fatalities.year, injuries.year))[,-3]
morbimortality.year$Total <- as.data.table( morbimortality.year$Fatalities + morbimortality.year$Injuries )
morbimortality.year$Total <- apply( morbimortality.year[,2:3],1,sum )
colnames(morbimortality.year)[4] = "Total"
morbimortality.year.sorted <- morbimortality.year[order(-morbimortality.year$Total, na.last=TRUE), ]
head(morbimortality.year.sorted)


# barplot of Fatalities and Injuries by event type
nr = 15 # just show 15 top values
par(mfrow = c(3, 1), mar = c(11.5, 5, 4, 2), las = 3, cex = 0.5, cex.main = 1.4, cex.lab = 1.2)
with(fatalities.year.sorted[1:nr,], barplot(Fatalities, names.arg = fatalities.year.sorted$Year[1:nr], col = "red", ylab = "Fatalities", main = "Fatalities - Year"))
with(injuries.year.sorted[1:nr,], barplot(Injuries, names.arg = injuries.year.sorted$Year[1:nr], col = "green", ylab = "Injuries", main = "Injuries - Year"))
with(morbimortality.year.sorted[1:nr,], barplot(Total, names.arg = morbimortality.year.sorted$Year[1:nr], col = "blue", ylab = "Injuries", main = "Total (Fatalities + Injuries) - Year"))


# # Estimation of total amount of fatalities and injuries during the whole period
# totalfatalities <- sum(morbimortality.year$Fatalities)
# totalinjuries <- sum(morbimortality.year$Injuries)
# ratioif <-  totalinjuries/totalfatalities


#** (3) Across the United States, in which state people experienced the most number of Fatalities and Injuries?--------
#   we can answer this question by sorting fatalities and injuries in decreasing manner for each state
#** Answer: People at IL state, experienced most number of fatalities from natural events, and at AR state, experienced most number of INJURIES and TOTAL (FATALITIES+INJURIES) 
#**  Estimating total fatalities and injuries per state ----

# Aggregate Fatalities by state
fatalities.state <- aggregate(storm$FATALITIES, by = list(storm$STATE), FUN = sum)
colnames(fatalities.state) <- c("State", "Fatalities")
fatalities.state.sorted <- fatalities.state[order(-fatalities.state$Fatalities, na.last=TRUE), ]
head(fatalities.state.sorted)
# Aggregate Injuries by state
injuries.state <- aggregate(storm$INJURIES, by = list(storm$STATE), FUN = sum)
colnames(injuries.state) <- c("State", "Injuries")
injuries.state.sorted <- injuries.state[order(-injuries.state$Injuries, na.last=TRUE), ]
head(injuries.state.sorted)

# Combine Injuries and Fatalities by state
morbimortality.state <- data.frame(cbind(fatalities.state, injuries.state))[,-3]
morbimortality.state$Total <- as.data.table( morbimortality.state$Fatalities + morbimortality.state$Injuries )
morbimortality.state$Total <- apply( morbimortality.state[,2:3],1,sum )
colnames(morbimortality.state)[4] = "Total"
morbimortality.state.sorted <- morbimortality.state[order(-morbimortality.state$Total, na.last=TRUE), ]
head(morbimortality.state.sorted)


# barplot of Fatalities and Injuries by event type
nr = 15 # just show 15 top values
par(mfrow = c(3, 1), mar = c(11.5, 5, 4, 2), las = 3, cex = 0.5, cex.main = 1.4, cex.lab = 1.2)
with(fatalities.state.sorted[1:nr,], barplot(Fatalities, names.arg = fatalities.state.sorted$State[1:nr], col = "red", ylab = "Fatalities", main = "Fatalities - State"))
with(injuries.state.sorted[1:nr,], barplot(Injuries, names.arg = injuries.state.sorted$State[1:nr], col = "green", ylab = "Injuries", main = "Injuries - State"))
with(morbimortality.state.sorted[1:nr,], barplot(Total, names.arg = morbimortality.state.sorted$State[1:nr], col = "blue", ylab = "Injuries", main = "Total (Fatalities + Injuries) - State"))

# event type

############ Random Forest ###################

# set random seed
set.seed(123)

#** (4) question: which variables(colums) are more influencial in the estimation of total damage (crop + property damage)? ------
#   Answer: As it is shown in important variables plot, the most influencial variables are:
#   FATALITIES, LATITUDE, Population, Magnitude of hail, min tempreture, longitude, average tempratue, max tempreture, fujita scale (tornado power), economical condition for each state(rgdp), raining rate (pcp), width of turnado, ...
#   which makes sense because if you have more fatalities it is obvious that you have more property + crop damage
#   in some specific latitudes , natural events occur more times and that places are more likely to experience damages
#   having more population means that more property and crops are existed in that area, so they are more likely to suffer from natural events, and so on ...

#** Random forest for estimating total economical damage (DMG_t) ------------

# we removed crop damage (CROPDMG_t) and property damage (PROPDMG_t) because DMG_t is the sum of these two variables, and it is better to be removed to 
# distinguish what are the real variables influencing the prediction of total damage
# we also remove factors to run random forest

rf = randomForest(DMG_t~., data=storm[,c(-1,-2,-3,-11,-12)], importance =T,  na.action=na.omit)  
print(rf)  
par(mfrow = c(1, 1), mar = c(11.5, 5, 4, 2), las = 3, cex = 0.5, cex.main = 1.4, cex.lab = 1.2)

plot(rf, main="Random Forest Regression for ecomate (economic + climate + natural events) for estimating total damage (crop + property damage)")

# length(rf$mse)
# min(rf$mse)
# mean(rf$mse)
# head(rf$mse)
# tail(rf$mse) 


#--- optimal number of trees
n <- which.min(rf$mse); n   # give me the index which mse error of random forest is minimum

rf2 <- randomForest(DMG_t~., data=storm[,c(-1,-2,-3,-11,-12)], 
                    ntree=n, importance=TRUE, na.action=na.omit)  # now you can say the number of trees is the optimal one we found

#--- Importance of variables in estimating total damage (DMG_t): higher value mean more important
#  this importance will give you which feature is important in deciding this tree
par(mfrow = c(1, 1), mar = c(11.5, 5, 4, 2), las = 1, cex = 0.5, cex.main = 1.4, cex.lab = 1.2)
varImpPlot(rf2, scale=T, main = "Variable Importance Plot for estimating total damage (crop + property damage)")

#** (5) question: which variables(colums) are more influencial in the estimation of total morbimortality (injuries + fatalities)?
#   Answer: As it is shown in important variables plot, the most influencial variables are:
#   tmax, LONGITUDE, F, pcp, pop, ...
#   which makes sense, like our previous interpretation of result again LONGITUDE is influencial (place of living is important), 
#   population is important (more population leads to more morbi and Fujita(Thurnado power) are among most influencial features)
#   but in this case, morbi is not influenced by economical condition (rgdp), even if you are rich, if the natural even is strong, you will die!! :D

str(storm)
dim(storm)
storm$MORBI_t = storm$FATALITIES + storm$INJURIES 
str(storm)

# we removed DMG_t because it was the sum of crop damage (CROPDMG_t) and property damage (PROPDMG_t) to avoid repetitious features
# We removed FATALITIES and INJURIES because MORBI_T is sum of them to extract real influencial factors 
# we also remove factors to run random forest

rf = randomForest(MORBI_t~., data=storm[,c(-1,-2,-3,-13, -20,-8)], importance =T,  na.action=na.omit)  # remove factors to run random forest
print(rf)  
par(mfrow = c(1, 1), mar = c(11.5, 5, 4, 2), las = 3, cex = 0.5, cex.main = 1.4, cex.lab = 1.2)
plot(rf, main="Random Forest Regression for ecomate (economic + climate + natural events) for estimating Morbi (fatalities + injueirs)")

# length(rf$mse)
# min(rf$mse)
# mean(rf$mse)
# head(rf$mse)
# tail(rf$mse) 


#--- optimal number of trees
n <- which.min(rf$mse); n   # give me the index which mse error of random forest is minimum

rf2 <- randomForest(MORBI_t~., data=storm[,c(-1,-2,-3,-13, -20,-8)], 
                    ntree=n, importance=TRUE, na.action=na.omit)  # now you can say the number of trees is the optimal one we found

#--- Importance of variables in estimating total damage (DMG_t): higher value mean more important
#  this importance will give you which feature is important in deciding this tree
par(mfrow = c(1, 1), mar = c(11.5, 5, 4, 2), las = 1, cex = 0.5, cex.main = 1.4, cex.lab = 1.2)
varImpPlot(rf2, scale=T, main = "Variable Importance Plot for estimating morbi (Injuries + fatalities)")


#** (5) question: which variables(colums, features) are more influencial in the estimation of event type? ------
#   Answer: As it is shown in important variables plot, the most influencial variables are:
#   tmax, LONGITUDE, F, pcp, pop, ...
#   which makes sense, like our previous interpretation of result again LONGITUDE is influencial (place of living is important), 
#   population is important (more population leads to more morbi and Fujita(Thurnado power) are among most influencial features)
#   but in this case, morbi is not influenced by economical condition (rgdp), even if you are rich, if the natural even is strong, you will die!! :D

str(storm)

#** Random forest for estimating event type (EVTYPE) ------------
# we removed crop damage (CROPDMG_t) and property damage (PROPDMG_t) because DMG_t is the sum of these two variables, and it is better to be removed to 
# distinguish what are the real variables influencing the prediction of total damage
# we also remove factors to run random forest
str(storm[,c(-1,-2,-6,-11,-12)])
storm1 <- na.omit(storm) # omiting NA variables to make ctree function work
str(storm1)

ct = ctree(EVTYPE~., data=storm1)
ct
plot(ct, type='simple')
print(rf)  
par(mfrow = c(1, 1), mar = c(11.5, 5, 4, 2), las = 3, cex = 0.5, cex.main = 1.4, cex.lab = 1.2)

# 
# plot(rf, main="Random Forest Regression for ecomate (economic + climate + natural events)")
# 
# # length(rf$mse)
# # min(rf$mse)
# # mean(rf$mse)
# # head(rf$mse)
# # tail(rf$mse) 
# 
# 
# #--- optimal number of trees
# n <- which.min(rf$mse); n   # give me the index which mse error of random forest is minimum
# 
# rf2 <- randomForest(DMG_t~., data=storm[,c(-1,-2,-3,-11,-12)], 
#                     ntree=n, importance=TRUE, na.action=na.omit)  # now you can say the number of trees is the optimal one we found
# 
# #--- Importance of variables in estimating total damage (DMG_t): higher value mean more important
# #  this importance will give you which feature is important in deciding this tree
# par(mfrow = c(1, 1), mar = c(11.5, 5, 4, 2), las = 1, cex = 0.5, cex.main = 1.4, cex.lab = 1.2)
# varImpPlot(rf2, scale=T, main = "Variable Importance Plot")
# 
# 
# 
# 
# 
# 
# head(df2)
# par(mfrow=c(1,1))
# par(fig=c(0.1,0.7,0.3,0.9))
# par(cex=0.7, mai=c(0.1,0.1,0.2,0.1))
# quartz("decision tree", 10,10)
# dev.off()
# ?quartz
# head(df2)
# library(rpart)
# rfit = rpart(Year~., data=df2, method="anova", control = rpart.control(minsplit=10))
# #rfit
# 
# plot(rfit, uniform=T, main="reg", ,margin=0.2)
# text(rfit,   use.n=TRUE, all= T, cex=0.8) 
# 
# install.packages("bnlearn")
# library(bnlearn)
# #learning a beysian network from data
# bnhc <- hc(df2, score="bic-g")# based on gausian 
# #bnhc <- hc(marks, score="bic")# not work because it is for factor data
# bnhc
# bnhc$nodes
# bnhc$arcs
# 
# library(igraph)
# edges=arcs(bnhc)
# nodes=nodes(bnhc)
# net <- graph.data.frame(edges,directed=T,vertices=nodes)
# plot(net,vertex.label=V(net)$name,vertex.size=40,
#      edge.arrow.size=0.3,vertex.color="cyan",
#      edge.color="black")
# 
# 
# library(PerformanceAnalytics) # look at how they are related, they are kind of related by with correlation you cannot find which one is the reason for the other one
# chart.Correlation(marks,pch=21,histogram=TRUE)
