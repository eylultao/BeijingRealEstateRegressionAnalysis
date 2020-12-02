setwd("C:/Users/Kayson/Documents/GitHub/BeijingRealEstateRegressionAnalysis" )
setwd("/Users/eylulaygun/Desktop/Year\ 5/STAT\ 306/Group\ project/BeijingRealEstateRegressionAnalysis" )
getwd()
library("dplyr")
library(data.table) #useful library for doing fast operations on large datasets

# PART1: Data cleaning and general preparation
# data source: https://www.kaggle.com/ruiqurm/lianjia
data_simple <- read.csv("new copy.csv", header = TRUE, fileEncoding="latin1")# had to typeset encoding to eliminate type convert error 
str(data_simple) # sanity check
data_simple <- na.omit( data.frame(data_simple)) # remove NA values

# DATA CLEANING!!!!!

# some columns are char when they have to be integers (eg. livingRoom, drawingRoom, etc)
# livingroom is actually the bedroom, and drawingroom is actually the living room
data_simple$bedRoom <- as.numeric(data_simple$livingRoom)
data_simple$livingRoom <- as.numeric(data_simple$drawingRoom)
data_simple$bathRoom <- as.numeric(data_simple$bathRoom)
data_simple$constructionTime <- as.numeric(data_simple$constructionTime)
# typecast categorical variables as factors, but also keep the non factor versions for creating interaction terms
data_simple$district2 <- data_simple$district # not a factor
data_simple$district <- as.factor(data_simple$district)
data_simple$buildingType2 <- data_simple$buildingType
data_simple$buildingType <- as.factor(data_simple$buildingType)
data_simple$buildingStructure2 <- data_simple$buildingStructure
data_simple$buildingStructure <- as.factor(data_simple$buildingStructure)
data_simple$tradeTime <- as.Date(data_simple$tradeTime) # typeset tradeTime to Date Type
data_simple$Year <- as.numeric(format(data_simple$tradeTime, format = "%Y" )) # new column with just the date
data_simple$totalPriceLog <- log(data_simple$totalPrice* 10000)
str(data_simple) # sanity check

# remove all houses less than 10 totalPrice
data_simple <- data_simple[data_simple$totalPrice >10,]
# remove data before year 2010
data_simple <- data_simple[data_simple$Year >2009,]
table(data_simple$Year) # distribution of years in the data
group_year <- aggregate( data_simple[, 4:4 ], list(data_simple$Year), mean) # get avg house price per year

# CREATE training and testing data
N <- length(data_simple$id)
set.seed(2020)
all_indices = seq(1, N)
training_indices = sort(sample(1:N, 3*N/5, replace = FALSE)) # ratio is 60/40
training_set = data_simple[training_indices,]
testing_indices =  sort(all_indices[!all_indices %in% training_indices]) # remove training indices from set
testing_set = data_simple[testing_indices,]
all.equal(sort(c(training_indices, testing_indices)), all_indices) # sanity check to ensure we separated tests correctly

# EXPLORE CORRELATION BETWEEN VARIABLES
cor(training_set$totalPrice, training_set$square*training_set$price) 
cor(training_set$totalPrice, training_set$price)

# correlation matrices
corrs_wrt_totalPrice <- cor(select_if(training_set, is.numeric), training_set$totalPrice) 
corrs_all <- cor(select_if(training_set, is.numeric)) # correlation between all variables

library(MASS)
# MODEL SELECTION 
# Fit the full model 
full.model <- lm(totalPrice ~square+livingRoom+drawingRoom+kitchen+bathRoom+buildingType+constructionTime
                 +renovationCondition+buildingStructure+elevator+fiveYearsProperty+subway+district+communityAverage
                 , data = training_set)
summary(full.model)
#Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
# living room and elevator removed, they are 
step.model$anova
#backward
step.model.backward <- stepAIC(full.model, direction = "backward", 
                               trace = FALSE)
summary(step.model.backward)
step.model.backward$anova
#forward
null.model <-lm(totalPrice ~square+livingRoom+drawingRoom+kitchen+bathRoom+buildingType+constructionTime
                +renovationCondition+buildingStructure+elevator+fiveYearsProperty+subway+district+communityAverage
                , data = training_set)
step.model.forward <- stepAIC(null.model, direction = "forward", 
                              trace = FALSE)
summary(step.model.forward)
step.model.forward$anova
#colinearity detection
DAAG::vif(step.model)
DAAG::vif(step.model.backward)
DAAG::vif(step.model.forward)

# use regsubsets
regsubsets()
library(leaps)
model.regsubsets <- regsubsets(totalPrice ~square+livingRoom+bedRoom+kitchen+bathRoom+buildingType+constructionTime
                               +renovationCondition+buildingStructure+elevator+fiveYearsProperty+subway+district+communityAverage , data = training_set, method = "exhaustive", nvmax = 15)
model.regsubsets.s <- summary(model.regsubsets)
model.regsubsets.s$which
model.regsubsets.s$cp
model.regsubsets.s$adjr2

attempt1 <- lm(totalPrice ~square+livingRoom+kitchen+buildingType
               +renovationCondition+buildingStructure+fiveYearsProperty+district+communityAverage , data = training_set)
plot( attempt1$fitted.values,attempt1$residuals, main= "Residual plot for attempt1")

# tried regsubsets on the log of total price, the results are worse than the totalPRice (lower adjr2 and higher cp)
model.regsubsets.log <- regsubsets(totalPriceLog ~square+livingRoom+bedRoom+kitchen+bathRoom+buildingType+constructionTime
                                   +renovationCondition+buildingStructure+elevator+fiveYearsProperty+subway+district+communityAverage , data = training_set, method = "exhaustive", nvmax = 15)
model.regsubsets.log.s <- summary(model.regsubsets.log)
model.regsubsets.log.s$which
model.regsubsets.log.s$cp
model.regsubsets.log.s$adjr2

# interaction terms
training_set$interaction1 <- training_set$communityAverage * training_set$district2
training_set$interaction2 <- training_set$square * training_set$district2
training_set$interaction3 <- training_set$square * training_set$communityAverage
training_set$interaction4 <- training_set$square * training_set$buildingType2
training_set$interaction5 <- training_set$buildingStructure2 * training_set$buildingType2
# testing
testing_set$interaction1 <- testing_set$communityAverage * testing_set$district2
testing_set$interaction2 <- testing_set$square * testing_set$district2
testing_set$interaction3 <- testing_set$square * testing_set$communityAverage
testing_set$interaction4 <- testing_set$square * testing_set$buildingType2
testing_set$interaction5 <- testing_set$buildingStructure2 * testing_set$buildingType2

# try regsubsets with interactions
model.regsubsets.interactions <-  regsubsets(totalPrice ~square+livingRoom+bedRoom+kitchen+bathRoom+buildingType+constructionTime
                                             +renovationCondition+buildingStructure+elevator+fiveYearsProperty+subway+district+communityAverage+interaction1 + interaction2  , data = training_set, method = "exhaustive", nvmax = 15)
model.regsubsets.interactions.s <- summary(model.regsubsets.interactions)
model.regsubsets.interactions.s$which
model.regsubsets.interactions.s$cp
model.regsubsets.interactions.s$adjr2


# no district,buildingType, buidingStructure, just their interaction 
model.regsubsets.interactions.d <-  regsubsets(totalPrice ~square+livingRoom+bedRoom+kitchen+bathRoom+constructionTime
                                               +renovationCondition+elevator+fiveYearsProperty+subway+communityAverage+interaction1 + interaction2 +interaction4 +interaction5 , 
                                               data = training_set, method = "exhaustive", nvmax = 15)
model.regsubsets.interactions.d.s <- summary(model.regsubsets.interactions.d)
model.regsubsets.interactions.d.s$which
model.regsubsets.interactions.d.s$cp
model.regsubsets.interactions.d.s$adjr2


# trying rlm function instead of lm
model.interactions <- rlm(totalPrice ~square+livingRoom+bedRoom+kitchen
                          +renovationCondition+elevator+fiveYearsProperty+subway+communityAverage+interaction1 + interaction2 +interaction4+interaction5 , data = training_set)
summary(model.interactions)

DAAG::vif(model.interactions)
# Values of VIF that exceed 10 are often regarded as indicating multicollinearity, but in weaker models values above 2.5 may be a cause for concern.
(DAAG::vif(model.interactions) < 10)

# plots for residuals 
p <- plot(model.interactions$fitted.values , model.interactions$residuals)

# final model with log price
model.interactions.log <- lm(totalPriceLog ~square+livingRoom+bedRoom+kitchen
                             +renovationCondition+elevator+fiveYearsProperty+subway+communityAverage+interaction1 + interaction2 +interaction4+interaction5 , data = training_set)
summary(model.interactions.log)
p <- plot(model.interactions.log$fitted.values , model.interactions.log$residuals, main= "res plot of logtotalprice 2")


# final model with log price and square footage as polynomial
model.interactions.log.poly <- lm(totalPriceLog ~square+livingRoom+bedRoom+kitchen
                                  +renovationCondition+elevator+fiveYearsProperty+subway+communityAverage+ I(square^2)+interaction1 + interaction2 +interaction4+interaction5 +Year , data = training_set)
summary(model.interactions.log.poly)
plot(model.interactions.log.poly$fitted.values , model.interactions.log.poly$residuals, main= "res plot of logtotalprice polynomial squareft")

# no district,buildingType, buidingStructure, just their interaction 
model.regsubsets.log.poly <-  regsubsets(totalPriceLog ~square+livingRoom+bedRoom+kitchen
                                         +renovationCondition+elevator+fiveYearsProperty+subway+communityAverage+ I(square^2)+interaction1 + interaction2 +interaction4+interaction5 , data = training_set, method = "exhaustive", nvmax = 15)
model.regsubsets.log.poly.s <- summary(model.regsubsets.log.poly)
model.regsubsets.log.poly.s$which
model.regsubsets.log.poly.s$cp
model.regsubsets.log.poly.s$adjr2
# living room and elevator is not included in the 2nd best model, best model has cp = 13.34070, adjr2 = 0.6566243

# final model attempt 2 (living room and elevator removed )
model.interactions.log.poly.reduced <- lm(totalPriceLog ~square+bedRoom+kitchen
                                          +renovationCondition+fiveYearsProperty+subway+communityAverage+ I(square^2)+interaction1 + interaction2 +interaction4+interaction5 , data = training_set)
summary(model.interactions.log.poly.reduced)
p <- plot(model.interactions.log.poly.reduced$fitted.values , model.interactions.log.poly.reduced$residuals, main= "res plot of reduced final model")
# qq normal plot of residuals
qqnorm(model.interactions.log.poly.reduced$residuals)
# qq normal plot of standardized residuals
standardized.residuals <- rstandard(model.interactions.log.poly.reduced)
qqnorm(standardized.residuals, main ="standardize residuals qq plot")
qqline(standardized.residuals)

# cross validation
# train on training_set, test on testing_set
error1 <- sum((testing_set$totalPriceLog - predict(model.interactions.log.poly.reduced, testing_set))^2)
rmse1 <- sqrt(sum((testing_set$totalPriceLog - predict(model.interactions.log.poly.reduced, testing_set))^2)/dim(testing_set)[1])
pred <- predict(model.interactions.log.poly.reduced, testing_set, se.fit = TRUE, interval = "prediction")


# train on the testing_set, test on the training_set (LOL!)
model.interactions.log.poly.reduced.testing <- lm(totalPriceLog ~square+bedRoom+kitchen
                                                  +renovationCondition+fiveYearsProperty+subway+communityAverage+ I(square^2)+interaction1 + interaction2 +interaction4+interaction5 , data = testing_set)
error2 <- sum((training_set$totalPriceLog - predict(model.interactions.log.poly.reduced.testing, training_set))^2)
rmse2 <- sqrt(sum((training_set$totalPriceLog - predict(model.interactions.log.poly.reduced.testing, training_set))^2)/dim(training_set)[1])
pred.testing <- predict(model.interactions.log.poly.reduced, training_set, se.fit = TRUE, interval = "prediction")


####################################################################
# final model attempt 3 (living room and elevator removed, YEAR INCLUDED )
# this is the very final model!! 
training_set$rencond <- as.factor(training_set$renovationCondition)
training_set$rencond <- relevel(training_set$rencond , ref =2)

model.interactions.log.poly.reduced.year <- lm(totalPriceLog ~square+bedRoom+kitchen
                                               +renovationCondition+fiveYearsProperty+subway+communityAverage+ I(square^2)+interaction1  +interaction4+interaction5 + Year , data = training_set)
summary(model.interactions.log.poly.reduced.year)
p <- plot(model.interactions.log.poly.reduced.year$fitted.values , model.interactions.log.poly.reduced.year$residuals, main= "res plot of reduced final model", ylab = "Residuals", xlab = "fitted values")
# qq normal plot of residuals
qqnorm(model.interactions.log.poly.reduced.year$residuals)
# qq normal plot of standardized residuals
standardized.residuals.year <- rstandard(model.interactions.log.poly.reduced.year)
qqnorm(standardized.residuals.year, main ="standardized residuals qq plot")
qqline(standardized.residuals.year)

# testing for final model attempt 3 
# cross validation
# train on training_set, test on testing_set
error1 <- sum((testing_set$totalPriceLog - predict(model.interactions.log.poly.reduced.year, testing_set))^2)
rmse1 <- sqrt(sum((testing_set$totalPriceLog - predict(model.interactions.log.poly.reduced.year, testing_set))^2)/dim(testing_set)[1])
pred <- predict(model.interactions.log.poly.reduced.year, testing_set, se.fit = TRUE, interval = "prediction")
filter3 <- testing_set[pred$fit[,2] < testing_set$totalPriceLog & testing_set$totalPriceLog < pred$fit[,3],]
dim(filter3)[1]/ dim(testing_set)[1] # 97.4% accuracy!!

# train on the testing_set, test on the training_set (LOL!)
model.interactions.log.poly.reduced.year.testing <- lm(totalPriceLog ~square+bedRoom+kitchen
                                                       +renovationCondition+fiveYearsProperty+subway+communityAverage+ I(square^2)+interaction1 +interaction4+interaction5 + Year , data = testing_set)
summary(model.interactions.log.poly.reduced.year.testing)
error2 <- sum((training_set$totalPriceLog - predict(model.interactions.log.poly.reduced.year.testing, training_set))^2)
rmse2 <- sqrt(sum((training_set$totalPriceLog - predict(model.interactions.log.poly.reduced.year.testing, training_set))^2)/dim(training_set)[1])
pred.testing <- predict(model.interactions.log.poly.reduced.year.testing, training_set, se.fit = TRUE, interval = "prediction" )
filter3_test <- training_set[pred.testing$fit[,2] < training_set$totalPriceLog & training_set$totalPriceLog < pred.testing$fit[,3],]
dim(filter3_test)[1]/ dim(training_set)[1] # predicted 97.2% correctly within the prediction interval
######


