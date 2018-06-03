####################################
rm(list=ls())
dev.off()
# set directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
curdir=dirname(rstudioapi::getActiveDocumentContext()$path)
datadir <- curdir
setwd(datadir)
getwd()
#install.packages("githubinstall")
#library(githubinstall)
#githubinstall("https://github.com/awesomedata/awesome-public-datasets")
############1.Preparing data###################
#**(1)Install and load packages----------------
#install & load
#if you have already installed the packages only run line 12 and skip up to line 20
packages <- c("R.utils", "data.table", "downloader", "lubridate", "plyr","dplyr",
              "rstudioapi","randomForest","tree","party","tidyr","broom","datasets",
              "ggplot2","tabplot","PerformanceAnalytics","coefplot","RColorBrewer",
              "data.table","igraph", "rpart","bnlearn","lattice", 
              "grid","partykit","deal","fiftystater","plotly","googleVis")

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
saveRDS(x, file = "Cleandata.rda")
###############################################################################################
#################################################
#################################################
# #3.Basic statistic analysis##########
# #split data in factor variable and continuos
# x_f <- x[,c(1:3)]
# x_n <- x[,-c(1:3)]
# #**(1)summary table of numeric variables--------
# sumstat <- x_n %>%
#   # Select and rename five variables 
#   select("Number of people Killed"= FATALITIES,
#          "Number of people Injured"=INJURIES,
#          "Latitude where the storm event began"= LATITUDE,
#          "Longitude where the storm event began"= LONGITUDE,
#          "Property Damage"= PROPDMG_t,
#          "Crop Damage"= CROPDMG_t,
#          "Total damage sum"= DMG_t,
#          "Real GDP"=rgdp,
#          "Population"=pop,
#          "Average Year Temperature"=tavg,
#          "Maxmimun Year Temperature"=tmax,
#          "Minimum Year Temperature"=tmin,
#          "Year Precipitation"=pcp) %>%
#   # Find the mean, st. dev., min, and max for each variable 
#   summarise_all(funs(mean, sd, min, max)) %>%
#   # Move summary stats to columns
#   gather(key, value, everything()) %>% 
#   separate(key, into = c("variable", "stat"), sep = "_") %>%
#   spread(stat, value) %>%
#   # Set order of summary statistics 
#   select(variable, mean, sd, min, max) %>%
#   # Round all numeric variables to one decimal point
#   mutate_at(.funs=funs(round(., 1)),.vars = c("mean","sd","min","max"))
# sumstat
# #**(2)table plot----------
# tableplot(x_f)
# #**(3)Barplot of factor variables--------
# #1.create ordering function
# reorder_size <- function(x) {
#   factor(x, levels = names(sort(table(x), decreasing = TRUE)))
# }
# #2. barplot each factors using ordering function
# p1 <- ggplot(x_f, aes(x = reorder_size(x_f$STATE))) +
#   geom_bar() +
#   xlab("State or Province") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# p2<- ggplot(x_f, aes(x = x_f$YEAR)) +
#   geom_bar() +
#   xlab("YEAR") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# p3 <- ggplot(x_f, aes(x = reorder_size(x_f$EVTYPE))) +
#   geom_bar() +
#   xlab("Event Type") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# #3. create Multiple plot function
# # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# # - cols:   Number of columns in layout
# # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
# # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# # then plot 1 will go in the upper left, 2 will go in the upper right, and
# # 3 will go all the way across the bottom.
# #4. Plot multiplot
# multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#   library(grid)
#   # Make a list from the ... arguments and plotlist
#   plots <- c(list(...), plotlist)
#   numPlots = length(plots)
#   # If layout is NULL, then use 'cols' to determine layout
#   if (is.null(layout)) {
#     # Make the panel
#     # ncol: Number of columns of plots
#     # nrow: Number of rows needed, calculated from # of cols
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#   if (numPlots==1) {
#     print(plots[[1]])
#   } else {
#     # Set up the page
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#     # Make each plot, in the correct location
#     for (i in 1:numPlots) {
#       # Get the i,j matrix positions of the regions that contain this subplot
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
# }
# multiplot(p1, p3, p2, cols=1)
# #**(4)Which ??????
# bx_x=x
# bx_x$STATE <- as.character(x$STATE)
# bx_x<-subset(bx_x, subset = STATE %in% c("TX","KS","IA","AL","MO"))
# bx_x$STATE <- as.factor(bx_x$STATE)
# str(bx_x)
# str(bx_x)
# bx_x<-filter(x, STATE== "")
# ggplot(data = bx_x, 
#        aes(x=STATE, y=FATALITIES)) + 
#   geom_boxplot() + 
#   facet_wrap(~YEAR,ncol = 8)
# #**(5)Linear regression----------
# chart.Correlation(x_n)
# 
# #Clustering
# 



##############################start the code from here #######################
# we = Amirsaman and Insu
# we fixed missing Injuries in the dataset, and we saved a new Rda file in the folder
# we added new columns in the data like Morbimortality + some columns based on our needs during the code
# we did Random forest, barplot, variable importance plot
# we defined 5 research question and the answers are provided below questions

storm=readRDS('Cleandata.Rda')

#--------- adding total Morbimortality to data columns
str(storm)
storm$MORBI_t = storm$FATALITIES + storm$INJURIES 
storm$MORBI_t <- apply( storm[,4:5],1,sum )
#colnames(storm)[17] = "MORBI_t"
storm <-transform(storm, YEAR = as.numeric(as.character(YEAR)))
str(storm)

# delete factors with NAN
table(factor(storm$EVTYPE))
levels(storm$EVTYPE)
storm$EVTYPE <- factor(storm$EVTYPE);
table(factor(storm$EVTYPE))
str(storm)

##########Research Questions #######

#** (1) Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful (most Fatalities and Injuries)? --------
#  we can answer this question by sorting fatalities and injuries in decreasing manner for each type of event
#** Answer: TORNADO caused most number of fatalities, WIND caused most number of INJURIES and TOTAL (FATALITIES+INJURIES) 


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
#with(fatalities.event.sorted[1:nr,], barplot(Fatalities, names.arg = fatalities.event.sorted$Event[1:nr], col = "red", ylab = "Fatalities", main = "Fatalities - Event"))
#with(injuries.event.sorted[1:nr,], barplot(Injuries, names.arg = injuries.event.sorted$Event[1:nr], col = "green", ylab = "Injuries", main = "Injuries - Event"))
#with(morbimortality.event.sorted[1:nr,], barplot(Total, names.arg = injuries.event.sorted$Event[1:nr], col = "blue", ylab = "Total", main = "Total (Fatalities + Injuries) - Event"))
Fat<-fatalities.event.sorted[1:nr,]
Fat$Event <- factor(Fat$Event, levels = Fat$Event[order(-Fat$Fatalities)])

ggplot(Fat, aes(x=Event, y=Fatalities, fill=Event))+
  geom_bar(stat="identity", position="dodge")+
  xlab("Event")+
  ylab("Fatalities")+
  ggtitle("Fatalities vs Event")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

Inj<-injuries.event.sorted[1:nr,]
Inj$Event <- factor(Inj$Event, levels = Inj$Event[order(-Inj$Injuries)])

ggplot(Inj, aes(x=Event, y=Injuries, fill=Event))+
  geom_bar(stat="identity", position="dodge")+
  xlab("Event")+
  ylab("Injuries")+
  ggtitle("Injuries vs Event")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

Mor<-morbimortality.event.sorted[1:nr,]
Mor$Event <- factor(Mor$Event, levels = Mor$Event[order(-Mor$Total)])

ggplot(Mor, aes(x=Event, y=Total, fill=Event))+
  geom_bar(stat="identity", position="dodge")+
  xlab("Event")+
  ylab("Morbimortalities")+
  ggtitle("Morbimortalities vs Event")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

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
#with(fatalities.year.sorted[1:nr,], barplot(Fatalities, names.arg = fatalities.year.sorted$Year[1:nr], col = "red", ylab = "Fatalities", main = "Fatalities - Year"))
#with(injuries.year.sorted[1:nr,], barplot(Injuries, names.arg = injuries.year.sorted$Year[1:nr], col = "green", ylab = "Injuries", main = "Injuries - Year"))
#with(morbimortality.year.sorted[1:nr,], barplot(Total, names.arg = morbimortality.year.sorted$Year[1:nr], col = "blue", ylab = "Injuries", main = "Total (Fatalities + Injuries) - Year"))

Faty<-fatalities.year.sorted[1:nr,]
Faty$Year <- factor(Faty$Year, levels = Faty$Year[order(-Faty$Fatalities)])

ggplot(Faty, aes(x=Year, y=Fatalities, fill=Year))+
  geom_bar(stat="identity", position="dodge")+
  xlab("Year")+
  ylab("Fatalities")+
  ggtitle("Fatalities vs Year")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

Injy<-injuries.year.sorted[1:nr,]
Injy$Year <- factor(Injy$Year, levels = Injy$Year[order(-Injy$Injuries)])

ggplot(Injy, aes(x=Year, y=Injuries, fill=Year))+
  geom_bar(stat="identity", position="dodge")+
  xlab("Year")+
  ylab("Injuries")+
  ggtitle("Injuries vs Year")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

Mory<-morbimortality.year.sorted[1:nr,]
Mory$Year <- factor(Mory$Year, levels = Mory$Year[order(-Mory$Total)])

ggplot(Mory, aes(x=Year, y=Total, fill=Year))+
  geom_bar(stat="identity", position="dodge")+
  xlab("Year")+
  ylab("Morbimortalities")+
  ggtitle("Morbimortalities vs Year")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

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
#with(fatalities.state.sorted[1:nr,], barplot(Fatalities, names.arg = fatalities.state.sorted$State[1:nr], col = "red", ylab = "Fatalities", main = "Fatalities - State"))
#with(injuries.state.sorted[1:nr,], barplot(Injuries, names.arg = injuries.state.sorted$State[1:nr], col = "green", ylab = "Injuries", main = "Injuries - State"))
#with(morbimortality.state.sorted[1:nr,], barplot(Total, names.arg = morbimortality.state.sorted$State[1:nr], col = "blue", ylab = "Morbimortality", main = "Total (Fatalities + Injuries) - State"))

Fats<-fatalities.state.sorted[1:nr,]
Fats$State <- factor(Fats$State, levels = Fats$State[order(-Fats$Fatalities)])

ggplot(Fats, aes(x=State, y=Fatalities, fill=State))+
  geom_bar(stat="identity", position="dodge")+
  xlab("State")+
  ylab("Fatalities")+
  ggtitle("Fatalities vs State")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

Injs<-injuries.state.sorted[1:nr,]
Injs$State <- factor(Injs$State, levels = Injs$State[order(-Injy$Injuries)])

ggplot(Injs, aes(x=State, y=Injuries, fill=State))+
  geom_bar(stat="identity", position="dodge")+
  xlab("State")+
  ylab("Injuries")+
  ggtitle("Injuries vs State")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

Mors<-morbimortality.state.sorted[1:nr,]
Mors$State <- factor(Mors$State, levels = Mors$State[order(-Mory$Total)])

ggplot(Mors, aes(x=State, y=Total, fill=State))+
  geom_bar(stat="identity", position="dodge")+
  xlab("State")+
  ylab("Morbimortalities")+
  ggtitle("Morbimortalities vs State")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

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

str(storm)
storm_sample = storm[sample(nrow(storm),0.01*nrow(storm)),] # small sample data to do randonforest 

storm1 = as.data.frame(storm[,-c(1, 3, 8, 9, 17)]) # We removed EVTYPE, PROPDMG_t, CROPDMG_t
str(storm1)
storm1_s = storm_sample[,-c(1, 3, 8, 9, 17)] 
str(storm1_s)

rf = randomForest(DMG_t~., data=storm1_s, importance =T,  na.action=na.omit)  
print(rf)  
par(mfrow = c(1, 1), mar = c(11.5, 5, 4, 2), las = 3, cex = 0.5, cex.main = 1.4, cex.lab = 1.2)
plot(rf, main="Random Forest Regression for ecomate (economic + climate + natural events) for estimating total damage (crop + property damage)")
str(rf$test)
#?randomForest
length(rf$mse)

#nrow(rf$mse)
#rep(1:nrow(rf$mse),1)
x<-rep(1:length(rf$mse))
y<-rf$mse
rfd<-data.frame(x,y)

ggplot(rfd, aes(x, y))+
  #geom_point(col='blue')+
  geom_line(col='blue')+
  #stat_smooth(method='lm', col='red')+
  theme_bw()+
  xlab('Number of trees')+
  ylab('Error (MSE)')+
  ggtitle("Random Forest Regression of Ecomate for Estimating Total Damage")+
  theme(plot.title = element_text(hjust = 0.5))

#--- optimal number of trees
n <- which.min(rf$mse); n   # give me the index which mse error of random forest is minimum

rf2 <- randomForest(DMG_t~., data=storm1_s, 
                    ntree=n, importance=TRUE, na.action=na.omit)  # now you can say the number of trees is the optimal one we found

#--- Importance of variables in estimating total damage (DMG_t): higher value mean more important
#  this importance will give you which feature is important in deciding this tree
par(mfrow = c(1, 1), mar = c(11.5, 5, 4, 2), las = 1, cex = 0.5, cex.main = 1.4, cex.lab = 1.2)
varImpPlot(rf2, scale=T, main = "Variable Importance Plot for Estimating Total Damage (Crop + Property Damage)")
#** (5) question: which variables(colums) are more influencial in the estimation of total morbimortality (injuries + fatalities)?
#   Answer: As it is shown in important variables plot, the most influencial variables are:
#   tmax, LONGITUDE, F, pcp, pop, ...
#   which makes sense, like our previous interpretation of result again LONGITUDE is influencial (place of living is important), 
#   population is important (more population leads to more morbi and Fujita(Thurnado power) are among most influencial features)
#   but in this case, morbi is not influenced by economical condition (rgdp), even if you are rich, if the natural even is strong, you will die!! :D
str(storm)
storm2 <- storm[,-c(1,3, 4, 5, 10)]
str(storm2)
storm2_s <-  storm_sample[,-c(1,3, 4, 5, 10)]
str(storm2_s)
str(storm2)
# we removed DMG_t because it was the sum of crop damage (CROPDMG_t) and property damage (PROPDMG_t) to avoid repetitious features
# We removed FATALITIES and INJURIES because MORBI_T is sum of them to extract real influencial factors 
# we also remove factors to run random forest

rf = randomForest(MORBI_t~., data=storm2_s, importance =T,  na.action=na.omit)  # remove factors to run random forest
print(rf)  
par(mfrow = c(1, 1), mar = c(11.5, 5, 4, 2), las = 3, cex = 0.5, cex.main = 1.4, cex.lab = 1.2)
#plot(rf, main="Random Forest Regression for ecomate (economic + climate + natural events) for estimating Morbi (fatalities + injueirs)")

x<-rep(1:length(rf$mse))
y<-rf$mse
rfd<-data.frame(x,y)

ggplot(rfd, aes(x, y))+
  #geom_point(col='blue')+
  geom_line(col='blue')+
  #stat_smooth(method='lm', col='red')+
  theme_bw()+
  xlab('Number of trees')+
  ylab('Error (MSE)')+
  ggtitle("Random Forest Regression of Ecomate for Estimating Morbimortalities")+
  theme(plot.title = element_text(hjust = 0.5))

#--- optimal number of trees
n <- which.min(rf$mse); n   # give me the index which mse error of random forest is minimum

rf2 <- randomForest(MORBI_t~., data=storm2_s, 
                    ntree=n, importance=TRUE, na.action=na.omit)  # now you can say the number of trees is the optimal one we found

#--- Importance of variables in estimating total damage (DMG_t): higher value mean more important
#  this importance will give you which feature is important in deciding this tree
par(mfrow = c(1, 1), mar = c(11.5, 5, 4, 2), las = 1, cex = 0.5, cex.main = 1.4, cex.lab = 1.2)
varImpPlot(rf2, scale=T, main = "Variable Importance Plot for Estimating Morbimortalities")

#** (6) question: which variables(colums, features) are more influencial in the estimation of event type? ------
#   Answer: As it is shown in important variables plot, the most influencial variables are:
#   tmax, LONGITUDE, F, pcp, pop, ...
#   which makes sense, like our previous interpretation of result again LONGITUDE is influencial (place of living is important), 
#   population is important (more population leads to more morbi and Fujita(Thurnado power) are among most influencial features)
#   but in this case, morbi is not influenced by economical condition (rgdp), even if you are rich, if the natural even is strong, you will die!! :D

#** Random forest for estimating event type (EVTYPE) ------------
# we removed crop damage (CROPDMG_t) and property damage (PROPDMG_t) because DMG_t is the sum of these two variables, and it is better to be removed to 
# distinguish what are the real variables influencing the prediction of total damage
# we also remove factors to run random forest
str(storm_sample)
storm3 = storm[,c(-1,-10,-17)]
str(storm3)
storm3_s = storm_sample[,c(-1, -10, -17)]
str(storm3)

table(factor(storm3$EVTYPE))
levels(storm3$EVTYPE)
storm3$EVTYPE <- factor(storm3$EVTYPE);
table(factor(storm3$EVTYPE))
str(storm3)

#View(storm3)
table(factor(storm3_s$EVTYPE))
levels(storm3_s$EVTYPE)
storm3_s$EVTYPE <- factor(storm3_s$EVTYPE);
table(factor(storm3$EVTYPE))
str(storm3_s)

crf = randomForest(EVTYPE~., data=storm3_s, importance = T, method = " class", proximity = T )
print(crf)
predict(crf, type='prob')

plot(crf)

err<-crf$err.rate
names(err[1,])
library(reshape2)
library(ggplot2)

str(err[1,])
test <- data.frame(hmm=err, trees=as.numeric(rep(1:500)))
head(test)

str(test)
library(RColorBrewer)
labels <- as.factor(names(err[1,]))
cols = colorRampPalette(brewer.pal(11, "Spectral"))(15)
wtf <- as.factor(err)
ggplot(data=test,aes(x=trees)) +
  geom_line(aes(y=hmm.OOB, colour=cols[1]))+
  geom_line(aes(y=hmm.COLD, colour=cols[2]))+
  geom_line(aes(y=hmm.DRY, colour=cols[3]))+
  geom_line(aes(y=hmm.FIRE, colour=cols[4]))+
  geom_line(aes(y=hmm.FLOOD, colour=cols[5]))+
  geom_line(aes(y=hmm.FOG, colour=cols[6]))+
  geom_line(aes(y=hmm.FUNNEL.CLOUD, colour=cols[7]))+
  geom_line(aes(y=hmm.HEAT, colour=cols[8]))+
  geom_line(aes(y=hmm.ICE, colour=cols[9]))+
  geom_line(aes(y=hmm.LANDSLIDE, colour=cols[10]))+
  geom_line(aes(y=hmm.SNOW, colour=cols[11]))+
  geom_line(aes(y=hmm.STORM, colour=cols[12]))+
  geom_line(aes(y=hmm.SURF, colour=cols[13]))+
  geom_line(aes(y=hmm.TORNADO, colour=cols[14]))+
  geom_line(aes(y=hmm.WIND, colour=cols[15]))+
  scale_colour_manual(name="Event Types",labels = names(err[1,]),values=cols)+
  xlab('Number of trees')+
  ylab('Error (MSE)')+
  ggtitle("Random Forest Regression of Ecomate for estimating Event Types")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() 


library(RColorBrewer)
# par(mfrow = c(1, 1), las = 3, cex = 0.5, cex.main = 1.4, cex.lab = 1.2, xpd = T, mar = par()$mar + c(0,0,0,7))
# plot(crf, col=colorRampPalette(brewer.pal(11, "Spectral"))(14), lty = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1),
#      main="Random Forest Classification for event type estimating ")
# legend("topright", inset= c(-0.2,0),colnames(crf$err.rate), col=1:15,  lty = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1)  , bty='n', y.intersp= 0.5)

x = round(importance(crf),3)
y = x[,1:14]
str(y)

par(mfrow = c(1, 1), las = 3, cex = 0.8, cex.main = 1.4, cex.lab = 1.2, xpd = F, mar = c(10,7,3,9), mgp = c(5, 1, 0))

barplot( y, legend.text = rownames(x), col=colorRampPalette(brewer.pal(11, "Spectral"))(14),
         main="Variable Importance Plot for Estimating Event Type",
         xlab="Event",
         ylab="Importance",
         args.legend=list(x=ncol(y) + 7, y= max(colSums(y))) )
abline(h=0, col="black")


# ct = ctree(EVTYPE~., data=storm3)
# ct
# plot(ct, type='simple')
# print(rf)  
# par(mfrow = c(1, 1), mar = c(11.5, 5, 4, 2), las = 3, cex = 0.5, cex.main = 1.4, cex.lab = 1.2)

########## Recursive Partitioning (Decision trees) ######################
str(storm1)

rfit = rpart(DMG_t~., data=storm1, method="anova", control = rpart.control(minsplit=10))
par(mfrow = c(1, 1), mar = c(11.5, 5, 4, 2), las = 1, cex = 1, cex.main = 1.4, cex.lab = 1.2)
plot(rfit, uniform=T,
     main="Recursive Partitioning Tree on Total Damage", margin=0.2)
text(rfit,   use.n=TRUE, all= T, cex=0.8)

str(storm2)
rfit = rpart(MORBI_t~., data=storm2, method="anova", control = rpart.control(minsplit=10))
par(mfrow = c(1, 1), mar = c(0.1, 0.1, 3, 0.1), las = 1, cex = 1, cex.main = 1.4, cex.lab = 1.2)
plot(rfit, uniform=T, main="Recursive Partitioning Tree on Morbimortality", margin=0.2)
text(rfit,   use.n=TRUE, all= T, cex=0.8)

str(storm3)
#storm3 <- storm[,-c(4,5, 8, 9, 14, 15)]
storm3$EVTYPE[6]
cfit = rpart(EVTYPE~., data=storm3, method = "class")
par(mfrow = c(3, 1), mar = c(11.5, 5, 4, 2), las = 1, cex = 0.9, cex.main = 1.4, cex.lab = 1.2)
plot(as.party(cfit), 
     main="Recursive Partitioning Tree on Event Type",
     tp_args = list(id=FALSE))

######### Bayesian Analysis ########################
install.packages("bnlearn")
library(data.table)
library(lattice)
#library(bnlearn)
library(igraph)
library(bnlearn)
library(network)
library(deal)
library(RColorBrewer)
library(PerformanceAnalytics)
#** DMG_t --------
str(storm1)
#learning a bayesian network from continues data
bnhc <- hc(storm1, score="bic-g")# based on gausian 
bnhc
bnhc$nodes
bnhc$arcs
str(colnames(storm1))
edges=arcs(bnhc)
nodes=nodes(bnhc)

#cols = colorRampPalette(brewer.pal(11, "Spectral"))(length(colnames(storm1)))
net <- graph.data.frame(edges,directed=T,vertices=nodes)
par(mfrow = c(1, 1), mar = c(4, 0.1, 4, 0.1), las = 3, cex = 0.8, cex.main = 1.4, cex.lab = 1.2)
plot(net,vertex.label=V(net)$name,vertex.size=(8*nchar(V(net)$name)),
     main="Bayesian Network Plot on Total Damage",
     #xlab=colnames(storm1),
     edge.arrow.size=0.3,vertex.color=colorRampPalette(brewer.pal(11, "Spectral"))(length(colnames(storm1))),
     edge.color="black")
text(0, -1.5, toString(colnames(storm1), Width = " "), cex = .8, font=2)

chart.Correlation(storm1,pch=21,histogram=TRUE)

#** MORBI_t --------------
str(storm2)
storm2 <- na.omit(storm2)
#learning a beysian network from continues data
bnhc <- hc(storm2, score="bic-g")# based on gausian 
bnhc
bnhc$nodes
bnhc$arcs

edges=arcs(bnhc)
nodes=nodes(bnhc)
net <- graph.data.frame(edges,directed=T,vertices=nodes)
plot(net,vertex.label=V(net)$name,vertex.size=(8*nchar(V(net)$name)),
     main="Bayesian Network Plot on Morbimortalities",
     #xlab=colnames(storm1),
     edge.arrow.size=0.3,vertex.color=colorRampPalette(brewer.pal(11, "Spectral"))(length(colnames(storm2))),
     edge.color="black")
text(0, -1.5, toString(colnames(storm2), Width = " "), cex = .8, font=2)


chart.Correlation(storm2,pch=21,histogram=TRUE)

#** EVTYPE
library(deal)
str(storm3)
storm4 = storm3_s [,c(2,3,4,5,6,8,9,10,11,12,13,14)] # we removed year and crop_dmg because these columns caused Beysian analysis to become ill-conditioned because of two many repetitious values and zeros
str(storm4)

storm3.nw <- network(storm4)          
storm3.prior <- jointprior(storm3.nw) # make joint prior distribution
# learn estimate parameters
storm3.nw <- learn(storm3.nw,storm4,storm3.prior)$nw

result <- heuristic(initnw=storm3.nw, data=storm4, prior=storm3.prior,
                    restart=2, degree=10, trace=FALSE)
par(mfrow = c(1, 1), mar = c(11.5, 5, 4, 2), las = 3, cex = 0.8, cex.main = 1.4, cex.lab = 0.2)
plot(getnetwork(result),
     main="Bayesian Network Plot on Event Type")
text(170, -50, toString(colnames(storm4), Width = " "), cex = .8, font=2)

################# Map ################
library(googleVis)
#** Map plots to support question 3
#** (3) Across the United States, in which state people experienced the most number of Fatalities and Injuries?--------
statecodeweb=url(paste("http://eunyoungko.com/resources/rprojectdata/economic/","statecode.csv", sep=""))
statecode <- read.csv(statecodeweb,sep=",", header=TRUE)
colnames(statecode)<-c("STATENAME","STATE")
statecode
storm_v=merge(storm,statecode, by="STATE", all.x=TRUE)
head(storm_v)
str(storm_v)
storm_m = storm_v[,c(4,5,17, 8,9,10 )]
storm_m.state = aggregate(storm_m, by = list(storm_v$STATENAME), FUN = sum)
head(storm_m.state) 
colnames(storm_m.state)[1] <- c("STATENAME")
#** Morbi Map Plots ---------------
#** FATALITY MAP Plot for each state
head(storm_m.state[order(-storm_m.state$FATALITIES, na.last=TRUE),])
MapFATALITIES <- gvisGeoChart(storm_m.state, "STATENAME", "FATALITIES",
                              options=list(region="US", 
                                           displayMode="regions", 
                                           resolution="provinces",
                                           width=600, height=400))
g=plot(MapFATALITIES)
print(MapFATALITIES,file="required_map_plots/MapFATALITIES.html")


#** INJURIES MAP Plot for each state
head(storm_m.state[order(-storm_m.state$INJURIES, na.last=TRUE),])
MapINJURIES <- gvisGeoChart(storm_m.state, "STATENAME", "INJURIES",
                            options=list(region="US", 
                                         displayMode="regions", 
                                         resolution="provinces",
                                         width=600, height=400))
g=plot(MapINJURIES)
print(MapINJURIES,file="required_map_plots/MapINJURIES.html")


#** MORBI_t MAP Plot for each state
head(storm_m.state[order(-storm_m.state$MORBI_t, na.last=TRUE),])
MapMORBI_t <- gvisGeoChart(storm_m.state, "STATENAME", "MORBI_t",
                           options=list(region="US", 
                                        displayMode="regions", 
                                        resolution="provinces",
                                        width=600, height=400))
g=plot(MapMORBI_t)
print(MapMORBI_t,file="required_map_plots/MapMORBI_t.html")



#** property damage Map plots ----------------
#** PROPDMG_t MAP Plot for each state
head(storm_m.state[order(-storm_m.state$PROPDMG_t, na.last=TRUE),])
MapPROPDMG_t <- gvisGeoChart(storm_m.state, "STATENAME", "PROPDMG_t",
                             options=list(region="US", 
                                          displayMode="regions", 
                                          resolution="provinces",
                                          width=600, height=400))
g=plot(MapPROPDMG_t)
print(MapPROPDMG_t,file="required_map_plots/MapPROPDMG_t.html")


#** CROPDMG_t MAP Plot for each state
head(storm_m.state[order(-storm_m.state$CROPDMG_t, na.last=TRUE),])
MapCROPDMG_t <- gvisGeoChart(storm_m.state, "STATENAME", "CROPDMG_t",
                             options=list(region="US", 
                                          displayMode="regions", 
                                          resolution="provinces",
                                          width=600, height=400))
g=plot(MapCROPDMG_t)
print(MapCROPDMG_t,file="required_map_plots/MapCROPDMG_t.html")


#** DMG_t MAP Plot for each state
head(storm_m.state[order(-storm_m.state$DMG_t, na.last=TRUE),])
MapDMG_t <- gvisGeoChart(storm_m.state, "STATENAME", "DMG_t",
                         options=list(region="US", 
                                      displayMode="regions", 
                                      resolution="provinces",
                                      width=600, height=400))
g=plot(MapDMG_t)
print(MapDMG_t,file="required_map_plots/MapDMG_t.html")



