setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

# State code 
statecode <- read.csv("statescode.csv",sep=",", header=TRUE)

# annual data
atavg<-read.csv("annual_tavg.csv", sep=",", header=TRUE)
atmax<-read.csv("annual_tmax.csv", sep=",", header=TRUE)
atmin<-read.csv("annual_tmin.csv", sep=",", header=TRUE)
apcp<-read.csv("annual_pcp.csv", sep=",", header=TRUE)

annual_temp=merge(atavg, atmax, by=c("state", "year"))
annual_temp=merge(annual_temp, atmin, by=c("state", "year"))
annual_temp=merge(annual_temp, apcp, by=c("state", "year"))

annual_temp=merge(statecode,annual_temp, by="state" )
annual_temp=annual_temp[,-c(1)]
write.csv(annual_temp,"annual_temp.csv")


# monthly data
mtavg<-read.csv("monthly_tavg.csv", sep=",", header=TRUE)
mtmax<-read.csv("monthly_tmax.csv", sep=",", header=TRUE)
mtmin<-read.csv("monthly_tmin.csv", sep=",", header=TRUE)
mpcp<-read.csv("monthly_pcp.csv", sep=",", header=TRUE)

monthly_temp=merge(mtavg, mtmax, by=c("state", "year","month"))
monthly_temp=merge(monthly_temp, mtmin, by=c("state", "year","month"))
monthly_temp=merge(monthly_temp, mpcp, by=c("state", "year","month"))

monthly_temp=merge(statecode,monthly_temp, by="state" )
monthly_temp=monthly_temp[,-c(1)]
write.csv(monthly_temp,"monthly_temp.csv")
