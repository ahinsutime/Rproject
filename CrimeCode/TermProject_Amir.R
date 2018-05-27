#dirname(rstudioapi::getSourceEditorContext()$path)
setwd("/Users/Amirsam/Documents/GitHub/")
getwd()
rm(list=ls())
#rm(list=ls())




###########################################################
library(ggplot2)
library(dplyr)
library(readr)
#install.packages("corrplot")
library(corrplot)
library(lattice)
library(caret)
#install.packages("ROCR")
library(gplots)
library(ROCR)
library(tree)
library(randomForest)
#install.packages("rstanarm")
#install.packages("ggridges")
library(Rcpp)
library(rstanarm)
library(pROC)


library(data.table)
#install.packages("R.utils")
library(R.oo)
library(R.methodsS3)
library(R.utils) 
#install.packages("downloader")
library(downloader)
library(lubridate)
library(plyr)
library(stats)
library(graphics)
library(lattice)
library(ggplot2)

#--------------------   Crime in Korea
### day
# 1st Research question:
# In which day in Daejeon, the most crimes are happened?

Daejeon_Crime_Day = read.csv("Daejeon_Crime_Day.csv" , header = T)
wdf = Daejeon_Crime_Day[,2:8]
clean = function(df){
  as.numeric( gsub(",","",df) )
}
wdf = sapply(wdf, clean)
wdf = as.data.frame(wdf)
rownames(wdf) = Daejeon_Crime_Day[,1]
wdf
str(wdf)
wdf2 = as.matrix(wdf)
class(wdf2)
wdf2 = t(wdf2)
wdf2
wdf2 = as.data.frame(wdf2)
wdf2
str(wdf2)
wdf2[,1]
wdf2$Total = apply(wdf2, 1, sum)
wdf2
wdf2$City = as.factor(rep("Daejeon", 7))
wdf2
str(wdf2)
wdf2 = wdf2[order(-wdf2$Total, na.last=TRUE), ]
wdf2
x = rownames(wdf2)
barplot(wdf2$Total,col='cornsilk',names.arg=x,xlab="Week days",ylab='Crime rates in Daejeon')
# the most crimes are done on Friday

# Random forest for crimes in Daejeon

rf = randomForest(Total~., data=wdf2, importance =T)
print(rf)  # it is normal to have a different value because each computer has diferent random value 
plot(rf, main="Random Forest Regression for Boston")

length(rf$mse)
min(rf$mse)
mean(rf$mse)
head(rf$mse)
tail(rf$mse) 

n <- which.min(rf$mse)  # give me the index which mse error is minimum
n
rf2 <- randomForest(Total~., data=wdf2, 
                    ntree=n, importance=TRUE, na.action=na.omit)  # now you can say the number of trees is the optimal one we found
?randomForest
print(rf2)
# Importance of variables: higher value mean more important
#  this importance will give you which feature is important in deciding this tree, the features at the top are important because bigger dimension/ which features come to the top ? (important) /   IncMSE: if you don't use this feature how much error will increase in your dataset / rm has the highest values, number of rooms is so important (most people use the once) / IncNodePurity (how much subtree really about that tree for example rm has the most sub trees and so important)
importance(rf2)    
varImpPlot(rf2, scale=T, main = "Variable Importance Plot")


head(df2)
library(rpart)
wdf2
wdf3 = wdf2[,-9]
str(wdf3)
rfit = rpart(Total~., data=wdf3, method="anova", control = rpart.control(minsplit=3))
rfit
?rpart
#rfit
plot(rfit, uniform=T, main="reg", margin=0.2)
text(rfit,   use.n=TRUE, all= T, cex=0.8)

# library(bnlearn)
# #learning a beysian network from data
# bnhc <- hc(wdf3, score="bic-g")# based on gausian 
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


library(PerformanceAnalytics) # look at how they are related, they are kind of related by with correlation you cannot find which one is the reason for the other one
chart.Correlation(marks,pch=21,histogram=TRUE)
df2
# having continues and factor, we have to use different set of functions
library(deal) # package for having combination factor and continues values

wdf2.nw <- network(wdf2)          # specify prior network

wdf2.prior <- jointprior(wdf2.nw) # make joint prior distribution
# learn estimate parameters
wdf2.nw <- learn(wdf2.nw,wdf2,wdf2.prior)$nw
wdf2.nw

result <- heuristic(initnw=wdf2.nw, data=wdf2, prior=wdf2.prior,
                    restart=2, degree=10, trace=FALSE)
plot(getnetwork(result))

# linear regression
wdf3
str(wdf3)
wdf3
ur = lm(Total ~ Heavy + Theft + Violence +Intelligence   , data=wdf3)  # y variable employed x gross national product # assuming linear relationship
summary(ur)
#less than 0.05 null rejected, null: there is no correlation, that means GNP is really related to employed, both are significant, the model p value also small, individually this term mean anything, the overal model maybe a good fit, R squared, average of blue dots ybar, yhat is the y estiate, red line yhat, ybar=average of y values (blue dots), is between zero and 1, if it is close to 1 is a good fit. for everyone unit, how much employed is moving. we have *** in Pr, if p value less than 0.05, when you see at least * more *** more smaller p value, more significant 
with(wdf3, plot(Heavy,Total, pch=20, bg='cyan')) #pch point size, background of the point

lines(wdf3$Heavy, ur$fitted.values, col='red')  # lines give something on top of the plot
ur$fitted.values # for every year give you yhat values


# 3rd Research question: Is time of day influence crime rates?
# Answer: based on P-value of Chi-squared test we reject null hyphothesis, the time during the day influences the crime rate , and the highest crime is happening at 00:00~04:00
#Chi-squared test for given probabilities

#data:  tab1
#X-squared = 14864, df = 5, p-value < 2.2e-16


# Chi-square test for crime in Daejeon

wdf3$Time = factor(rownames(wdf3))
wdf3
str(wdf3)
tab1 = xtabs(Total ~Time , data=wdf3) ; tab1 #table( $) gives the numbers   #xtab sum of actual values of the types #sum(subset(Intersecsprays, sprays=="A")$count #table just count the number of rows, xtab will give you actual sum of that corresponding by spray types
tab1
#subset(InsectSprays, spray=="A")$count
#sum(subset(InsectSprays, spray=="A")$count)
#tab1
addmargins(tab1) #give new column and add the row

barplot(tab1, xlab="Spray Type", ylab = "Insect Count", col=2:7)

tab1 = as.matrix(tab1)
tab1
chisq.test(tab1)



#### Time
# 2nd Research question:
# In which time during a day in Daejeon, the most crimes are happened?

Daejeon_Crime_Time = read.csv("Daejeon_Crime_Time.csv", header = T)
Daejeon_Crime_Time
df = Daejeon_Crime_Time[,2:7]
clean = function(df){
  as.numeric( gsub(",","",df) )
}
df = sapply(df, clean)
df = as.data.frame(df)
rownames(df) = Daejeon_Crime_Time[,1]
df
str(df)
dim(df)
colnames(df) = c("(00:00~04:00)","(04:00~07:00)","(07:00~12:00)","(12:00~18:00)","(18:00~20:00)","(20:00~24:00)")
df
df2 = as.matrix(df)
class(df2)
df2 = t(df2)
df2
df2 = as.data.frame(df2)
df2
str(df2)
df2[,1]
df2$Total = apply(df2, 1, sum)
df2
df2$City = as.factor(rep("Daejeon", 6))
df2
str(df2)
df2 = df2[order(-df2$Total, na.last=TRUE), ]
df2
x = rownames(df2)
barplot(df2$Total,col='cornsilk',names.arg=x,xlab="Week days",ylab='Crime rates in Daejeon')




# Random forest for crimes in Daejeon

rf = randomForest(Total~., data=df2, importance =T)
print(rf)  # it is normal to have a different value because each computer has diferent random value 
plot(rf, main="Random Forest Regression for Boston")

length(rf$mse)
min(rf$mse)
mean(rf$mse)
head(rf$mse)
tail(rf$mse) 

n <- which.min(rf$mse)  # give me the index which mse error is minimum
n
rf2 <- randomForest(Total~., data=df2, 
                    ntree=n, importance=TRUE, na.action=na.omit)  # now you can say the number of trees is the optimal one we found
?randomForest
print(rf2)
# Importance of variables: higher value mean more important
#  this importance will give you which feature is important in deciding this tree, the features at the top are important because bigger dimension/ which features come to the top ? (important) /   IncMSE: if you don't use this feature how much error will increase in your dataset / rm has the highest values, number of rooms is so important (most people use the once) / IncNodePurity (how much subtree really about that tree for example rm has the most sub trees and so important)
importance(rf2)    
varImpPlot(rf2, scale=T, main = "Variable Importance Plot")


head(df2)
library(rpart)
df3 = df2[,-9]
str(df3)
rfit = rpart(Total~., data=df3, method="anova", control = rpart.control(minsplit=3))
rfit
?rpart
#rfit
plot(rfit, uniform=T, main="reg", margin=0.2)
text(rfit,   use.n=TRUE, all= T, cex=0.8)

library(bnlearn)
#learning a beysian network from data
bnhc <- hc(df3, score="bic-g")# based on gausian 
#bnhc <- hc(marks, score="bic")# not work because it is for factor data
bnhc
bnhc$nodes
bnhc$arcs

library(igraph)
edges=arcs(bnhc)
nodes=nodes(bnhc)
net <- graph.data.frame(edges,directed=T,vertices=nodes)
plot(net,vertex.label=V(net)$name,vertex.size=40,
     edge.arrow.size=0.3,vertex.color="cyan",
     edge.color="black")


library(PerformanceAnalytics) # look at how they are related, they are kind of related by with correlation you cannot find which one is the reason for the other one
chart.Correlation(marks,pch=21,histogram=TRUE)
df2
# having continues and factor, we have to use different set of functions
library(deal) # package for having combination factor and continues values

df2.nw <- network(df2)          # specify prior network

df2.prior <- jointprior(df2.nw) # make joint prior distribution
# learn estimate parameters
df2.nw <- learn(df2.nw,df2,df2.prior)$nw
df2.nw

result <- heuristic(initnw=df2.nw, data=df2, prior=df2.prior,
                    restart=2, degree=10, trace=FALSE)
plot(getnetwork(result))

# linear regression
df3
str(df3)
df3
ur = lm(Total ~ Heavy + Theft + Violence +Intelligence   , data=df3)  # y variable employed x gross national product # assuming linear relationship
summary(ur)
#less than 0.05 null rejected, null: there is no correlation, that means GNP is really related to employed, both are significant, the model p value also small, individually this term mean anything, the overal model maybe a good fit, R squared, average of blue dots ybar, yhat is the y estiate, red line yhat, ybar=average of y values (blue dots), is between zero and 1, if it is close to 1 is a good fit. for everyone unit, how much employed is moving. we have *** in Pr, if p value less than 0.05, when you see at least * more *** more smaller p value, more significant 
with(df3, plot(Heavy,Total, pch=20, bg='cyan')) #pch point size, background of the point

lines(df3$Heavy, ur$fitted.values, col='red')  # lines give something on top of the plot
ur$fitted.values # for every year give you yhat values


# 3rd Research question: Is time of day influence crime rates?
# Answer: based on P-value of Chi-squared test we reject null hyphothesis, the time during the day influences the crime rate , and the highest crime is happening at 00:00~04:00
#Chi-squared test for given probabilities

#data:  tab1
#X-squared = 14864, df = 5, p-value < 2.2e-16


# Chi-square test for crime in Daejeon

df3$Time = factor(rownames(df3))
df3
str(df3)
tab1 = xtabs(Total ~Time , data=df3) ; tab1 #table( $) gives the numbers   #xtab sum of actual values of the types #sum(subset(Intersecsprays, sprays=="A")$count #table just count the number of rows, xtab will give you actual sum of that corresponding by spray types
tab1
#subset(InsectSprays, spray=="A")$count
#sum(subset(InsectSprays, spray=="A")$count)
#tab1
addmargins(tab1) #give new column and add the row

barplot(tab1, xlab="Spray Type", ylab = "Insect Count", col=2:7)

tab1 = as.matrix(tab1)
tab1
chisq.test(tab1)

# 4rd Research question: Is the day of the week influence crime rates?
# Answer: based on P-value of Chi-squared test we reject null hyphothesis, the time during the day influences the crime rate , and the highest crime is happening at 00:00~04:00
#Chi-squared test for given probabilities




