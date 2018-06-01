
##############################start the code from here #######################
# we = Amirsaman and Insu
# we fixed missing Injuries in the dataset, and we saved a new Rda file in the folder
# we added new columns in the data like Morbimortality + some columns based on our needs during the code
# we did Random forest, barplot, variable importance plot
# we defined 5 research question and the answers are provided below questions


rm(list=ls())
#install & load
#if you have already installed the packages only run line 12 and skip up to line 20
#**--- Required packages -------------------
packages <- c("R.utils", "data.table", "downloader", "lubridate", "plyr","dplyr","rstudioapi","randomForest","tree","party","rpart","grid","libcoin","partykit","igraph","PerformanceAnalytics","deal")
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
storm_f=readRDS('Ecomate.Rda')
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

storm <- storm_f[,-c(9, 10)]
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
with(morbimortality.state.sorted[1:nr,], barplot(Total, names.arg = morbimortality.state.sorted$State[1:nr], col = "blue", ylab = "Morbimortality", main = "Total (Fatalities + Injuries) - State"))

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
str(storm)
storm <-transform(storm, F = as.numeric(F))
storm <-transform(storm, YEAR = as.numeric(as.character(YEAR)))
str(storm)
storm1 = storm[,c(-2,-3)] # we removed factors, F is just for thornado and MAG is just for hail
str(storm1)

rf = randomForest(DMG_t~., data=storm1, importance =T,  na.action=na.omit)  
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

rf2 <- randomForest(DMG_t~., data=storm1, 
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
storm2 = storm
storm2$MORBI_t = storm2$FATALITIES + storm2$INJURIES 
str(storm2)
storm2 = storm2[,c(-1,-3,-4,-5,-6,-7,-8,-13, -20)]
str(storm2)
# we removed DMG_t because it was the sum of crop damage (CROPDMG_t) and property damage (PROPDMG_t) to avoid repetitious features
# We removed FATALITIES and INJURIES because MORBI_T is sum of them to extract real influencial factors 
# we also remove factors to run random forest

rf = randomForest(MORBI_t~., data=storm2, importance =T,  na.action=na.omit)  # remove factors to run random forest
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

rf2 <- randomForest(MORBI_t~., data=storm2, 
                    ntree=n, importance=TRUE, na.action=na.omit)  # now you can say the number of trees is the optimal one we found

#--- Importance of variables in estimating total damage (DMG_t): higher value mean more important
#  this importance will give you which feature is important in deciding this tree
par(mfrow = c(1, 1), mar = c(11.5, 5, 4, 2), las = 1, cex = 0.5, cex.main = 1.4, cex.lab = 1.2)
varImpPlot(rf2, scale=T, main = "Variable Importance Plot for estimating morbi (Injuries + fatalities)")
View(storm)

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
str(storm)
storm3 = storm[,c(-1,-4,-5,-6,-7,-8,-13)]
str(storm3)
#View(storm3)
table(factor(storm3$EVTYPE))
storm3 <- na.omit(storm3) # omiting NA variables to make ctree function work
levels(storm3$EVTYPE)
storm3$EVTYPE <- factor(storm3$EVTYPE);
table(factor(storm3$EVTYPE))
crf = randomForest(EVTYPE~., data=storm3, ntree=5, importance = T, method = " class", proximity = T )

# ct = ctree(EVTYPE~., data=storm3)
# ct
# plot(ct, type='simple')
# print(rf)  
# par(mfrow = c(1, 1), mar = c(11.5, 5, 4, 2), las = 3, cex = 0.5, cex.main = 1.4, cex.lab = 1.2)



########## Recursive Partitioning (Decision trees) ######################
str(storm1)

rfit = rpart(DMG_t~., data=storm1, method="anova", control = rpart.control(minsplit=10))
plot(rfit, uniform=T, main="reg", ,margin=0.2)
text(rfit,   use.n=TRUE, all= T, cex=0.8)


str(storm2)
rfit = rpart(MORBI_t~., data=storm2, method="anova", control = rpart.control(minsplit=2))
plot(rfit, uniform=T, main="reg", ,margin=0.2)
text(rfit,   use.n=TRUE, all= T, cex=0.8)

str(storm3)
cfit = rpart(EVTYPE~., data=storm3, method = "class")
plot(as.party(cfit), tp_args = list(id=FALSE))

######### Beysian Analysis ########################

#** DMG_t --------
str(storm1)
storm1 <- na.omit(storm1)
#learning a beysian network from continues data
bnhc <- hc(storm1, score="bic-g")# based on gausian 
bnhc
bnhc$nodes
bnhc$arcs
#library(igraph)
edges=arcs(bnhc)
nodes=nodes(bnhc)
net <- graph.data.frame(edges,directed=T,vertices=nodes)
plot(net,vertex.label=V(net)$name,vertex.size=40,
     edge.arrow.size=0.3,vertex.color="cyan",
     edge.color="black")
#library(PerformanceAnalytics) 
chart.Correlation(marks,pch=21,histogram=TRUE)

#** MORBI_t --------------
str(storm2)
storm2 <- na.omit(storm2)
#learning a beysian network from continues data
bnhc <- hc(storm2, score="bic-g")# based on gausian 
bnhc
bnhc$nodes
bnhc$arcs
#library(igraph)
edges=arcs(bnhc)
nodes=nodes(bnhc)
net <- graph.data.frame(edges,directed=T,vertices=nodes)
plot(net,vertex.label=V(net)$name,vertex.size=40,
     edge.arrow.size=0.3,vertex.color="cyan",
     edge.color="black")
#library(PerformanceAnalytics) 
chart.Correlation(marks,pch=21,histogram=TRUE)

#** EVTY
str(storm)
dim(storm)
storm4 = storm[,c(-4,-5,-6,-7,-8,-13,-17,-18)]
str(storm4)
levels(storm4$EVTYPE)
levels(storm4$STATE)
storm4 = na.omit(storm4)
str(storm4)
storm4$STATE <- factor(storm4$STATE)
storm4$EVTYPE <- factor(storm4$EVTYPE)
levels(storm4$EVTYPE)
levels(storm4$STATE)


storm4.nw <- network(storm4)          
storm4.prior <- jointprior(storm4.nw) # make joint prior distribution
# learn estimate parameters
storm4.nw <- learn(storm4.nw,storm4,storm4.prior)$nw

result <- heuristic(initnw=storm4.nw, data=storm4, prior=storm4.prior,
                    restart=2, degree=10, trace=FALSE)
#win.graph(width=7,height=7)
plot(getnetwork(result))


