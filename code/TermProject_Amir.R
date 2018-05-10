rm(list=ls())
dev.off()

setwd("/Users/Amirsam/Desktop/Rdata-Project")
getwd()
#install.packages("githubinstall")
#library(githubinstall)
#githubinstall("https://github.com/awesomedata/awesome-public-datasets")


###############################################################################################
library(data.table)

#fread is much much much faster than read. Let's use fread to read csv datasets

#Datasets of crime

globalTerrorism <- fread('globalterrorismdb_0617dist.csv', stringsAsFactors=TRUE)
gunViolence <- fread('gun-violence-data_01-2013_03-2018.csv', stringsAsFactors=TRUE)
head(gunViolence)
#globalTerrorism <- read.csv('globalterrorismdb_0617dist.csv', header=TRUE)
#gunViolence <- read.csv('gun-violence-data_01-2013_03-2018.csv', header=TRUE)

#Datasets of climate

AirPollution <- fread.csv('Facility Air Pollution Dataset - All Facilities.csv', stringsAsFactors=TRUE)
GlobalTemperature <- fread.csv('GlobalTemperatures.csv', stringsAsFactors=TRUE)
GlobalLandTemp_State <- fread.csv('GlobalLandTemperaturesByState.csv', stringsAsFactors=TRUE)
GlobalLandTemp_MajorCity <- fread.csv('GlobalLandTemperaturesByMajorCity.csv', stringsAsFactors=TRUE)
GlobalLandTemp_Country <- fread.csv('GlobalLandTemperaturesByCountry.csv', stringsAsFactors=TRUE)
GlobalLandTemp_City <- fread("GlobalLandTemperaturesByCity.csv", stringsAsFactors=TRUE)

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







