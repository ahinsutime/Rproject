rm(list=ls())
dev.off()

setwd("C:/Rdata")

#install.packages("githubinstall")
#library(githubinstall)
#githubinstall("https://github.com/awesomedata/awesome-public-datasets")


############1.Preparing data###################
#**(1)Install and load packages----------------
#install & load
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("R.utils", "data.table", "downloader", "lubridate", "plyr")
ipak(packages)
library(dplyr)
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
#remove state data, remarks, refnum
storm2 <- storm1[,-c(5:6,14:15,31,36:37)]
#**(5)change "", "+","-","?" to NA----------------
storm2[ storm2 == "" ] <- NA
storm3=storm2
storm3[ storm3 == "+" ] <- NA
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
#**(1)Basic statistics
storm5

#**(2)Which events where the most fatals
storm5<-transform(storm5, EVTYPE = factor(EVTYPE)) #EVTYPE as factors
storm5$EVTYPE <- toupper(storm5$EVTYPE)
unique(storm5$EVTYPE)#total of 898 events
#Group event types by contents
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







