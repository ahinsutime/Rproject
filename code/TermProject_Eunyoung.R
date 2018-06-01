# I tried to make every possible and interesting data visualization in this file.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

# import Ecomate.Rda
curdir=dirname(rstudioapi::getActiveDocumentContext()$path)
stormdatapath=file.path(curdir,"final code", "Ecomate.Rda")
storm=readRDS(stormdatapath)

# we need state names, not just code, to use map feature 
statecodeweb=url(paste("http://eunyoungko.com/resources/rprojectdata/economic/","statecode.csv", sep=""))
statecode <- read.csv(statecodeweb,sep=",", header=TRUE)
colnames(statecode)<-c("STATENAME","STATE")
storm_v=merge(storm,statecode, by="STATE", all.x=TRUE)
  
# I added packages "fiftystater", "googleVis","wkhtmltopdf" and "plotly" in the below list 
packages <- c("fiftystater","plotly","googleVis","wkhtmltopdf","R.utils", "data.table", "downloader", "lubridate", "plyr","dplyr","rstudioapi","randomForest","tree","party","rpart","grid","libcoin","partykit","igraph","PerformanceAnalytics","deal", "bnlearn")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)
lapply(packages, require,character.only=T)



#**Plot maps for basic variables----------------

#***Raw variables for each states, for the whole period----------

library(data.table)
library(dplyr)

# total event count
eventcount=storm_v[, .N, by = .(STATENAME, EVTYPE)] %>% dcast(STATENAME ~ EVTYPE)
toteventcount=rowSums(eventcount[,2:20], na.rm=TRUE)
eventcount[,"totalevcount"]<-toteventcount
TotalEvent <- gvisGeoChart(eventcount, "STATENAME", "totalevcount",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))
g=plot(TotalEvent)
print(TotalEvent,file="mapPlots/totalevent.html")
#system("wkhtmltoimage --enable-plugins --javascript-delay 10000   totalevent.html totalevent.png")

# flood count by states 
TotalFlood <- gvisGeoChart(eventcount, "STATENAME", "FLOOD",
                           options=list(region="US", 
                                        displayMode="regions", 
                                        resolution="provinces",
                                        width=600, height=400))
print(TotalFlood,file="mapPlots/totalflood.html")

# storm count by states 
TotalStorm <- gvisGeoChart(eventcount, "STATENAME", "STORM",
                           options=list(region="US", 
                                        displayMode="regions", 
                                        resolution="provinces",
                                        width=600, height=400))
print(TotalStorm,file="mapPlots/totalstorm.html")

# wind count by states 
TotalWind <- gvisGeoChart(eventcount, "STATENAME", "WIND",
                           options=list(region="US", 
                                        displayMode="regions", 
                                        resolution="provinces",
                                        width=600, height=400))
print(TotalWind,file="mapPlots/totalwind.html")

# rgdp, population, rgdp per capita by states, at 1993 and 2011 (start year and end year)
urlecondata <- "http://eunyoungko.com/resources/rprojectdata/economic/"
statedataweb=url(paste(urlecondata,"statedata.csv", sep=""))
statedata <- read.csv(statedataweb,sep=",", header=TRUE)
statedata=statedata[,-c(1)]
colnames(statedata) <-c("STATE","YEAR","rgdp",'pop')
statedata_v=merge(statedata,statecode, by="STATE", all.x=TRUE)
rgdppc=statedata$rgdp/statedata$pop
statedata_v[,"rgdppc"]<-rgdppc

statedata_v_2011=subset(statedata_v, YEAR==2011)
statedata_v_1963=subset(statedata_v, YEAR==1963)

# rgdp
Rgdp63 <- gvisGeoChart(statedata_v_1963, "STATENAME", "rgdp",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))
print(Rgdp63,file="mapPlots/rgdp63.html")

Rgdp11 <- gvisGeoChart(statedata_v_2011, "STATENAME", "rgdp",
                       options=list(region="US", 
                                    displayMode="regions", 
                                    resolution="provinces",
                                    width=600, height=400))
print(Rgdp11,file="mapPlots/rgdp11.html")

# pop
Pop63 <- gvisGeoChart(statedata_v_1963, "STATENAME", "pop",
                       options=list(region="US", 
                                    displayMode="regions", 
                                    resolution="provinces",
                                    width=600, height=400))
print(Pop63,file="mapPlots/pop63.html")

Pop11 <- gvisGeoChart(statedata_v_2011, "STATENAME", "pop",
                       options=list(region="US", 
                                    displayMode="regions", 
                                    resolution="provinces",
                                    width=600, height=400))
print(Pop11,file="mapPlots/pop11.html")

# rgdp per capita
Rgdppc63 <- gvisGeoChart(statedata_v_1963, "STATENAME", "rgdppc",
                      options=list(region="US", 
                                   displayMode="regions", 
                                   resolution="provinces",
                                   width=600, height=400))
print(Rgdppc63,file="mapPlots/rgdppc63.html")

Rgdppc11 <- gvisGeoChart(statedata_v_2011, "STATENAME", "rgdppc",
                      options=list(region="US", 
                                   displayMode="regions", 
                                   resolution="provinces",
                                   width=600, height=400))
print(Rgdppc11,file="mapPlots/rgdppc11.html")

# weather data
urlweatherdata <- "http://eunyoungko.com/resources/rprojectdata/weather/"
tempweb=url(paste(urlweatherdata,"annual_temp.csv", sep=""))
tempdata <- read.csv(tempweb,sep=",", header=TRUE)
tempdata=tempdata[,-c(1)]
colnames(tempdata) <-c("STATENAME","YEAR","tavg",'tmax','tmin','pcp')

tempdata_v_2011=subset(tempdata, YEAR==2011)
tempdata_v_1963=subset(tempdata, YEAR==1963)

# tavg
tavg63 <- gvisGeoChart(tempdata_v_1963, "STATENAME", "tavg",
                         options=list(region="US", 
                                      displayMode="regions", 
                                      resolution="provinces",
                                      width=600, height=400))
print(tavg63,file="mapPlots/tavg63.html")

tavg11 <- gvisGeoChart(tempdata_v_2011, "STATENAME", "tavg",
                       options=list(region="US", 
                                    displayMode="regions", 
                                    resolution="provinces",
                                    width=600, height=400))
print(tavg11,file="mapPlots/tavg11.html")

# tmax
tmax63 <- gvisGeoChart(tempdata_v_1963, "STATENAME", "tmax",
                       options=list(region="US", 
                                    displayMode="regions", 
                                    resolution="provinces",
                                    width=600, height=400))
print(tmax63,file="mapPlots/tmax63.html")

tmax11 <- gvisGeoChart(tempdata_v_2011, "STATENAME", "tmax",
                       options=list(region="US", 
                                    displayMode="regions", 
                                    resolution="provinces",
                                    width=600, height=400))
print(tmax11,file="mapPlots/tmax11.html")

# tmin
tmin63 <- gvisGeoChart(tempdata_v_1963, "STATENAME", "tmin",
                       options=list(region="US", 
                                    displayMode="regions", 
                                    resolution="provinces",
                                    width=600, height=400))
print(tmin63,file="mapPlots/tmin63.html")

tmin11 <- gvisGeoChart(tempdata_v_2011, "STATENAME", "tmin",
                       options=list(region="US", 
                                    displayMode="regions", 
                                    resolution="provinces",
                                    width=600, height=400))
print(tmin11,file="mapPlots/tmin11.html")

# precipitation
pcp63 <- gvisGeoChart(tempdata_v_1963, "STATENAME", "pcp",
                       options=list(region="US", 
                                    displayMode="regions", 
                                    resolution="provinces",
                                    width=600, height=400))
print(pcp63,file="mapPlots/pcp63.html")

pcp11 <- gvisGeoChart(tempdata_v_2011, "STATENAME", "pcp",
                       options=list(region="US", 
                                    displayMode="regions", 
                                    resolution="provinces",
                                    width=600, height=400))
print(pcp11,file="mapPlots/pcp11.html")

#*** Detailed manage for disasters----------
# flood, wind, storm 
flood=subset(storm_v,EVTYPE=="FLOOD")
wind=subset(storm_v,EVTYPE=="WIND")
storm=subset(storm_v,EVTYPE=="STORM")

