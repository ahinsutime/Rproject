setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
#storm <- read.table("repdata_data_StormData.csv", sep = ",", header = TRUE)


# Concatenating State level (1) GDP, (2) Population variables 

# State code 
statecode <- read.csv("statecode.csv",sep=",", header=TRUE)

# (1) GDP - connecting two series from https://www.bea.gov/regional/downloadzip.cfm
# from 1963 to 1996 - Annual GDP by State, SIC GDP
gdp63dat <- paste("gdpstate_sic_all_C.csv")
gdp63dat.in <- readLines(gdp63dat)
gdp63dat.in <- gdp63dat.in[grep("error",gdp63dat.in, invert=TRUE)]
gdp63raw=read.csv(textConnection(paste(gdp63dat.in, collapse="\n")), header=TRUE)
# only use aggregated GDP 
gdp63=subset(gdp63raw,IndustryId==1)
gdp63=gdp63[-c(1,10, 53:63), c(1,2,9:43)]

# from 1997 to 2017 - Annual GDP by State, NAICS GDP
gdp97dat <- paste("gdpstate_naics_all_C.csv")
gdp97dat.in <- readLines(gdp97dat)
gdp97dat.in <- gdp97dat.in[grep("error",gdp97dat.in, invert=TRUE)]
gdp97raw=read.csv(textConnection(paste(gdp97dat.in, collapse="\n")), header=TRUE)
# remove non-state rows
gdp97=subset(gdp97raw,IndustryId==1)
gdp97=gdp97[-c(1, 10, 53:63), c(1,2,10:29)]

# merge two disconnected datasets 
gdp=merge(gdp63, gdp97, by=c("GeoFIPS", "GeoName"))

# make it real GDP, by deflating by CPI index (downloaded from https://fred.stlouisfed.org/series/CPALTT01USA661S)
cpi=read.csv("cpi.csv",sep=",", header=TRUE)
cpi=cpi[c(5:59),]
deflator=cpi$cpi/100
rgdp=gdp
gdpval=gdp[,c(3:57)]
gdpnumeric=apply(as.matrix(gdpval[,1:55]),2,as.numeric)
rgdpnumeric=gdpnumeric*deflator
rgdp[,c(3:57)]=data.frame(rgdpnumeric)
state=statecode$Abbreviation
rgdp=cbind(state,rgdp)
rgdp=rgdp[,-c(2,3)]

# Population by State
# data downloaded from https://fred.stlouisfed.org/search?nasw=0&st=population&t=annual%3Bpopulation%3Bstate%3Busa&ob=sr&od=desc
pop=read.csv("popstate.csv",sep=",", header=TRUE)

library(reshape2)
rgdp=setNames(melt(rgdp), c('state', 'year', 'rgdp'))
pop=setNames(melt(pop),c('state','year','pop'))
statedata=merge(rgdp, pop, by=c("state", "year"))
statedata$year <- substring(statedata$year, 2)
write.csv(statedata,"statedata.csv")



## Proportion of primary industry of total GDP
primaryind=read.csv("primaryind.csv",sep=",", header=TRUE)
library(reshape2)
primind=setNames(melt(primaryind), c('state', 'year', 'primind'))
primind$year <- substring(primind$year, 2)

statedatawithpind=merge(statedata,primind,by=c("state","year"))
write.csv(statedatawithpind,"statedata_short.csv")
