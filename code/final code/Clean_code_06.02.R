####################################
rm(list=ls())
dev.off()
setwd("C:/Rdata")
#install.packages("githubinstall")
#library(githubinstall)
#githubinstall("https://github.com/awesomedata/awesome-public-datasets")
############1.Preparing data###################
#**(1)Install and load packages----------------
#install & load
#if you have already installed the packages only run line 12 and skip up to line 20
packages <- c("R.utils", "data.table", "downloader", "lubridate", "plyr","dplyr",
              "rstudioapi","randomForest","tree","party","tidyr","broom","datasets",
              "ggplot2","tabplot","PerformanceAnalytics","coefplot")
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
storm1<-storm[!storm$STATE %in% c("AS","DC","FM","GU","MH","MP",
                          "PW","PR","VI","AE","AA","AP", "AM", "AN",
                          "GM","LC","LE","LH","LM","LO","LS","PH","PK",
                          "PM","PZ","SL","ST","XX"), ]
#**(3)Process data----------------
#subset relevant columns
#remove county data, remarks, refnum
storm2 <- storm1[,-c(5:6,14:15,31,36:37)]
#**(4)change "","-","?" to NA & "+" to 1 ---------------------------
storm2[ storm2 == "" ] <- NA
storm3=storm2
storm3[ storm3 == "+" ] <- "O"
storm4=storm3
storm4[ storm4 == "-" ] <- NA
storm5=storm4
storm5[ storm5 == "?" ] <- NA
############2.Preparing data###################
##resources:
#http://rstudio-pubs-static.s3.amazonaws.com/25710_7f98ce257ba941a3a9bd5e73186dc730.html
#https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html
#**(1)Basic statistics-------
storm5
#**(2)Which events where the most fatals------
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
#**(3)Unite propdmg with propdmgexp and corpdm with cropdmgexp-----------
unique(storm5$CROPDMGEXP) #check unique values
unique(storm5$PROPDMGEXP) #check unique values
#**(4)transform characters to numeric values-----------
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
storm7 <- storm6[,-c(21:24)]
#**(5)Extract Year--------------------------------
#Begin data as the criterion for year extraction
storm7$Begin.Time <- as.POSIXct(strptime(storm7$BGN_DATE, format ="%m/%d/%Y %H:%M:%S"))
storm7$Year.Begin <- year(as.Date(storm7$Begin.Time))
storm8 <- storm7[,-c(1:4,7:14,21,22,30)]
#**(6) Merge with economic data--------------------------------
urlecondata <- "http://eunyoungko.com/resources/rprojectdata/economic/"
# State code 
statecodeweb=url(paste(urlecondata,"statecode.csv", sep=""))
statecode <- read.csv(statecodeweb,sep=",", header=TRUE)
head(statecode)
# GDP - connecting two series from https://www.bea.gov/regional/downloadzip.cfm
# from 1963 to 1996 - Annual GDP by State, SIC GDP
gdp63web=url(paste(urlecondata,"gdpstate_sic_all_C.csv", sep=""))
gdp63dat.in <- readLines(gdp63web)
gdp63dat.in <- gdp63dat.in[grep("error",gdp63dat.in, invert=TRUE)]
gdp63raw=read.csv(textConnection(paste(gdp63dat.in, collapse="\n")), header=TRUE)
close(gdp63web)
head(gdp63raw)
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
head(gdp)
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
head(rgdp)
# Population by State
# data downloaded from https://fred.stlouisfed.org/search?nasw=0&st=population&t=annual%3Bpopulation%3Bstate%3Busa&ob=sr&od=desc
popweb=url(paste(urlecondata,"popstate.csv", sep=""))
pop=read.csv(popweb,sep=",", header=TRUE)
head(pop)
library(reshape2)
rgdp=setNames(melt(rgdp), c('state', 'year', 'rgdp'))
pop=setNames(melt(pop),c('state','year','pop'))
statedata=merge(rgdp, pop, by=c("state", "year"))
statedata$year <- substring(statedata$year, 2)
write.csv(statedata,"statedata.csv")
addi_data_2 <- statedata
head(addi_data_2)
#change column names of both datas
colnames(addi_data_2) <- c("STATE","YEAR","rgdp","pop")
str(storm8)
str(addi_data_2)
addi_data_2$YEAR<-as.numeric(addi_data_2$YEAR)
addi_data_2$STATE<-as.character(addi_data_2$STATE)
colnames(storm8)[1] <- "STATE"
colnames(storm8)[16] <- "YEAR"
storm9<-merge(storm8, addi_data_2, by=c("STATE","YEAR"), all.x = T) 
#**(7) Merge with weather data--------------------------------
urlweatherdata <- "http://eunyoungko.com/resources/rprojectdata/weather/"
#add annual weather data
# State code 
statecodeweb=url(paste(urlecondata,"statecode.csv", sep=""))
statecode <- read.csv(statecodeweb,sep=",", header=TRUE)
head(statecode)
levels(statecode$Abbreviation)
colnames(statecode)[1]<-"state"
# annual data
atavg<-read.csv(url(paste(urlweatherdata,"annual_tavg.csv", sep="")), sep=",", header=TRUE)
atmax<-read.csv(url(paste(urlweatherdata,"annual_tmax.csv", sep="")), sep=",", header=TRUE)
atmin<-read.csv(url(paste(urlweatherdata,"annual_tmin.csv", sep="")), sep=",", header=TRUE)
apcp<-read.csv(url(paste(urlweatherdata,"annual_pcp.csv", sep="")), sep=",", header=TRUE)
annual_temp=merge(atavg, atmax, by=c("state", "year"),all.x = T)
annual_temp=merge(annual_temp, atmin, by=c("state", "year"),all.x = T)
annual_temp=merge(annual_temp, apcp, by=c("state", "year"),all.x = T)
annual_temp$state <- toupper(annual_temp$state)
annual_temp_f=merge(statecode,annual_temp, by="state",all.x = T )
annual_temp = annual_temp_f[,-1]
colnames(annual_temp) = c("state","year","tavg","tmax","tmin","pcp")
annual_temp_2 <- annual_temp
colnames(annual_temp_2) <- c("STATE","YEAR","tavg","tmax","tmin","pcp")
annual_temp_2$YEAR<-as.numeric(annual_temp_2$YEAR)
annual_temp_2$STATE<-as.character(annual_temp_2$STATE)
storm_f<-merge(storm9, annual_temp_2, by=c("STATE","YEAR"), all.x = T) 
na_count_an <-sapply(storm9, function(y) sum(length(which(is.na(y)))))
na_count_an
#change data structure
storm_f$STATE<-as.factor(storm_f$STATE)
storm_f$YEAR<-as.factor(storm_f$YEAR)
storm_f$EVTYPE<-as.factor(storm_f$EVTYPE)
storm_f$F<-as.factor(storm_f$F)
str(storm_f)
#remove WIDTH, F, MAG
final_storm <- storm_f[,-c(4:7,12,13)]
str(final_storm)
#**(7) exclude all NAs and check last data--------------------------------
dim(final_storm)
x<-na.exclude(final_storm)
dim(x)
save(x, file = "Cleandata.rda")
###############################################################################################
#################################################
#################################################
#3.Basic statistic analysis##########
#split data in factor variable and continuos
x_f <- x[,c(1:3)]
x_n <- x[,-c(1:3)]
#**(1)summary table of numeric variables--------
sumstat <- x_n %>%
  # Select and rename five variables 
  select("Number of people Killed"= FATALITIES,
         "Number of people Injured"=INJURIES,
         "Latitude where the storm event began"= LATITUDE,
         "Longitude where the storm event began"= LONGITUDE,
         "Property Damage"= PROPDMG_t,
         "Crop Damage"= CROPDMG_t,
         "Total damage sum"= DMG_t,
         "Real GDP"=rgdp,
         "Population"=pop,
         "Average Year Temperature"=tavg,
         "Maxmimun Year Temperature"=tmax,
         "Minimum Year Temperature"=tmin,
         "Year Precipitation"=pcp) %>%
# Find the mean, st. dev., min, and max for each variable 
  summarise_all(funs(mean, sd, min, max)) %>%
# Move summary stats to columns
  gather(key, value, everything()) %>% 
  separate(key, into = c("variable", "stat"), sep = "_") %>%
  spread(stat, value) %>%
# Set order of summary statistics 
  select(variable, mean, sd, min, max) %>%
# Round all numeric variables to one decimal point
  mutate_at(.funs=funs(round(., 1)),.vars = c("mean","sd","min","max"))
sumstat
#**(2)table plot----------
tableplot(x_f)
#**(3)Barplot of factor variables--------
#1.create ordering function
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}
#2. barplot each factors using ordering function
p1 <- ggplot(x_f, aes(x = reorder_size(x_f$STATE))) +
  geom_bar() +
  xlab("State or Province") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p2<- ggplot(x_f, aes(x = x_f$YEAR)) +
  geom_bar() +
  xlab("YEAR") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p3 <- ggplot(x_f, aes(x = reorder_size(x_f$EVTYPE))) +
  geom_bar() +
  xlab("Event Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#3. create Multiple plot function
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#4. Plot multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
multiplot(p1, p3, p2, cols=1)
#**(4)Which ??????
bx_x=x
bx_x$STATE <- as.character(x$STATE)
bx_x<-subset(bx_x, subset = STATE %in% c("TX","KS","IA","AL","MO"))
bx_x$STATE <- as.factor(bx_x$STATE)
str(bx_x)
str(bx_x)
bx_x<-filter(x, STATE== "")
ggplot(data = bx_x, 
       aes(x=STATE, y=FATALITIES)) + 
  geom_boxplot() + 
  facet_wrap(~YEAR,ncol = 8)
#**(5)Linear regression----------
chart.Correlation(x_n)

#Clustering

