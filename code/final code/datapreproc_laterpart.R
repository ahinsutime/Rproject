
#**(6) Merge with economic data--------------------------------
urlecondata <- "http://eunyoungko.com/resources/rprojectdata/economic/"

# State code 
statecodeweb=url(paste(urlecondata,"statecode.csv", sep=""))
statecode <- read.csv(statecodeweb,sep=",", header=TRUE)

# GDP - connecting two series from https://www.bea.gov/regional/downloadzip.cfm
# from 1963 to 1996 - Annual GDP by State, SIC GDP
gdp63web=url(paste(urlecondata,"gdpstate_sic_all_C.csv", sep=""))
gdp63dat.in <- readLines(gdp63web)
gdp63dat.in <- gdp63dat.in[grep("error",gdp63dat.in, invert=TRUE)]
gdp63raw=read.csv(textConnection(paste(gdp63dat.in, collapse="\n")), header=TRUE)
close(gdp63web)
# only use aggregated GDP 
gdp63=subset(gdp63raw,IndustryId==1)
gdp63=gdp63[-c(1,10, 53:63), c(1,2,9:43)]

# from 1997 to 2017 - Annual GDP by State, NAICS GDP
gdp97web=url(paste(urlecondata,"gdpstate_naics_all_C.csv", sep=""))
gdp97dat.in <- readLines(gdp97web)
gdp97dat.in <- gdp97dat.in[grep("error",gdp97dat.in, invert=TRUE)]
gdp97raw=read.csv(textConnection(paste(gdp97dat.in, collapse="\n")), header=TRUE)
close(gdp97web)
# remove non-state rows
gdp97=subset(gdp97raw,IndustryId==1)
gdp97=gdp97[-c(1, 10, 53:63), c(1,2,10:29)]

# merge two disconnected datasets 
gdp=merge(gdp63, gdp97, by=c("GeoFIPS", "GeoName"))

# make it real GDP, by deflating by CPI index (downloaded from https://fred.stlouisfed.org/series/CPALTT01USA661S)
cpiweb=url(paste(urlecondata,"cpi.csv", sep=""))
cpi=read.csv(cpiweb,sep=",", header=TRUE)
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
popweb=url(paste(urlecondata,"popstate.csv", sep=""))
pop=read.csv(popweb,sep=",", header=TRUE)

library(reshape2)
rgdp=setNames(melt(rgdp), c('state', 'year', 'rgdp'))
pop=setNames(melt(pop),c('state','year','pop'))
statedata=merge(rgdp, pop, by=c("state", "year"))
statedata$year <- substring(statedata$year, 2)
write.csv(statedata,"statedata.csv")

addi_data_2 <- statedata
#change column names of both datas
colnames(addi_data_2) <- c("STATE","YEAR","rgdp","pop")
addi_data_2
colnames(storm8)[1] <- "STATE"
colnames(storm8)[16] <- "YEAR"
storm9<-merge(storm8, addi_data_2, by=c("STATE","YEAR"), all.x = T) 
View(storm9)

#**(7) Merge with weather data--------------------------------
urlweatherdata <- "http://eunyoungko.com/resources/rprojectdata/weather/"
#add annual weather data

# State code 
statecodeweb=url(paste(urlweatherdata,"statecode.csv", sep=""))
statecode <- read.csv(statecodeweb,sep=",", header=TRUE)

# annual data
statecodeweb=url(paste(urlecondata,"statecode.csv", sep=""))
statecode <- read.csv(statecodeweb,sep=",", header=TRUE)

atavg<-read.csv(url(paste(urlweatherdata,"annual_tavg.csv", sep="")), sep=",", header=TRUE)
atmax<-read.csv(url(paste(urlweatherdata,"annual_tmax.csv", sep="")), sep=",", header=TRUE)
atmin<-read.csv(url(paste(urlweatherdata,"annual_tmin.csv", sep="")), sep=",", header=TRUE)
apcp<-read.csv(url(paste(urlweatherdata,"annual_pcp.csv", sep="")), sep=",", header=TRUE)

annual_temp=merge(atavg, atmax, by=c("state", "year"))
annual_temp=merge(annual_temp, atmin, by=c("state", "year"))
annual_temp=merge(annual_temp, apcp, by=c("state", "year"))

annual_temp=merge(statecode,annual_temp, by="state" )
annual_temp=annual_temp[,-c(1)]
colnames(annual_temp) = c("state","year","tavg","tmax","tmin","pcp")

annual_temp_2 <- annual_temp
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