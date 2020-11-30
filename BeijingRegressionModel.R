setwd("C:/Users/Kayson/Documents/GitHub/BeijingRealEstateRegressionAnalysis" )
setwd("/Users/eylulaygun/Desktop/Year\ 5/STAT\ 306/Group\ project/BeijingRealEstateRegressionAnalysis" )
getwd()
library("dplyr")

# PART1: Data cleaning and general preparation
# data source: https://www.kaggle.com/ruiqurm/lianjia
#data <- read.csv("new.csv", header = TRUE, fileEncoding="latin1") # had to typeset encoding to eliminate type convert error
# data simple has DOM removed from it, as the majority of DOM values are na, which reduces dataset size enormously( cuts N in half)
data_simple <- read.csv("new copy.csv", header = TRUE, fileEncoding="latin1")# had to typeset encoding to eliminate type convert error 
data_simple
str(data_simple) # sanity check
# remove NA values
data_simple <- na.omit( data.frame(data_simple)) 


# DATA CLEANING!!!!!
# remove all houses less than 10 totalPrice
data_simple <- data_simple[data_simple$totalPrice >10,]



# some columns are char when they have to be integers (eg. livingRoom, drawingRoom, etc)
# livingroom is actually the bedroom, and drawingroom is actually the living room
data_simple$bedRoom <- as.numeric(data_simple$livingRoom)
data_simple$livingRoom <- as.numeric(data_simple$drawingRoom)
data_simple$bathRoom <- as.numeric(data_simple$bathRoom)
data_simple$constructionTime <- as.numeric(data_simple$constructionTime)
# typecast categorical variables as factors
data_simple$district2 <- data_simple$district # not a factor
data_simple$district <- as.factor(data_simple$district)

data_simple$buildingType2 <- data_simple$buildingType
data_simple$buildingType <- as.factor(data_simple$buildingType)


data_simple$buildingStructure2 <- data_simple$buildingStructure
data_simple$buildingStructure <- as.factor(data_simple$buildingStructure)
# typeset tradeTime to Date Type
data_simple$tradeTime <- as.Date(data_simple$tradeTime)
data_simple$totalPriceLog <- log(data_simple$totalPrice* 10000)
str(data_simple)

# CREATE training and testing data
N <- length(data_simple$id)
set.seed(2020)
all_indices = seq(1, N)
training_indices = sort(sample(1:N, 3*N/5, replace = FALSE))


training_set = data_simple[training_indices,]
testing_indices =  sort(all_indices[!all_indices %in% training_indices]) # remove training indices from set
testing_set = data_simple[testing_indices,]

all.equal(sort(c(training_indices, testing_indices)), all_indices) # sanity check to ensure we separated tests correctly

# EXPLORE CORRELATION BETWEEN VARIABLES
plot(training_set$totalPrice, training_set$square*training_set$price) # perfectly linear relationship. They are highly correlated with each other and are bound to have perfect linear relationship.
cor(training_set$totalPrice, training_set$square*training_set$price) 
cor(training_set$totalPrice, training_set$price)
# it wouldn't make sense to calculate pearson correlation between a categorical variable that looks like an "integer" with a continuous var such as totalPrice
# But I was curious and made a plot to see how house prices differ across districts in Beijing
plot(training_set$totalPrice, training_set$district)
# additional plots for other categorical variables
plot(training_set$totalPrice, training_set$buildingType)
plot(training_set$totalPrice, training_set$buildingStructure)
plot(training_set$totalPrice, training_set$communityAverage)

# correlation matrices
corrs_wrt_totalPrice <- cor(select_if(training_set, is.numeric), training_set$totalPrice) 
corrs_all <- cor(select_if(training_set, is.numeric)) # correlation between all variables
# DATA VISUALIZATION
#construct side by side boxplots to visualize the totoal prices across different building types
boxplot(totalPrice~buildingType,
        data=training_set,
        main="Different boxplots for building type",
        xlab="differnent building type",
        ylab="total price",
        col="orange",
        border="black"
)
##construct side by side boxplots to visualize the totoal prices across different building structures
boxplot(totalPrice~buildingStructure,
        data=training_set,
        main="Different boxplots for building type",
        xlab="differnent building type",
        ylab="total price",
        col="orange",
        border="black"
);

library(ggplot2)
library(data.table) #useful library for doing fast operations on large datasets
# trying to understand the most expensive neighborhoods
training_set
mean_price_by_district <- aggregate(training_set[, 4:4 ], list(training_set$district), mean)
mean_price_by_district
mean_price_by_district <- data.frame(district_number = mean_price_by_district$Group.1, mean_price = round( mean_price_by_district$x))
p <- ggplot(data = mean_price_by_district, aes(x = district_number, y = mean_price)) 
p + geom_bar(stat="identity", fill = "steelblue") + geom_text(aes(label=mean_price), vjust=1.5, color="white", size=3.5) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
  ggtitle("Distribution of Mean House Price Across Districts")
# trying to understand the variance of housing price of different districts
var_price_by_district <- aggregate(training_set[, 4:4 ], list(training_set$district), var)
var_price_by_district
var_price_by_district <- data.frame(district_number = var_price_by_district$Group.1, variance_price = round( var_price_by_district$x))
p <- ggplot(data = var_price_by_district, aes(x = district_number, y = variance_price)) 
p + geom_bar(stat="identity", fill = "steelblue") + geom_text(aes(label=variance_price), vjust=1.5, color="white", size=3.5) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
  ggtitle("Distribution of Variance of House Price Across Districts")
# How is community score changing across districts
mean_community_score_by_district <- aggregate(training_set[, 19:19 ], list(training_set$district), mean)
mean_community_score_by_district <- data.frame(district_number = mean_community_score_by_district$Group.1, community_avg = round( mean_community_score_by_district$x))

p2 <- ggplot(data = mean_community_score_by_district, aes(x = district_number, y = community_avg)) 
p2 + geom_bar(stat="identity", fill = "steelblue") + geom_text(aes(label=community_avg), vjust=1.5, color="white", size=3.5) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  ggtitle("Distribution of Mean Community Score Across Districts")

# the bar plot is very similar to the housing prices
# potential multicollinearity problem, will need to be adressed
# TODO: find a better way to explore collinearity
#We can explore collinearity once we have selected our model.
# TODO : how are building types distributed across districts? 
#district <- aggregate(training_set[, 11:11 ], list(training_set$district), count)
#The overall proportion of 4 building types.
district=data_simple[data_simple$district,]
building_type_by_district <- table(district$buildingType)
pie(building_type_by_district)
#district1=data_simple[data_simple$district==2,]
#table2 <- table(district1$buildingType)
# use the pie chart to show how building types are distributed within each district
par(mfrow=c(4,4)) 
pie( table(data_simple$buildingType[data_simple$district==1]), col=blues9, xlab="district 1",radius=1)
pie( table(data_simple$buildingType[data_simple$district==2]), col=blues9, xlab="district 2",radius=1)
pie( table(data_simple$buildingType[data_simple$district==3]), col=blues9, xlab="district 3",radius=1)
mtext(side=3, text="Frequency of buildingtype by district",font=2,line=2,adj=1.2)
pie( table(data_simple$buildingType[data_simple$district==4]), col=blues9, xlab="district 4",radius=1)
pie( table(data_simple$buildingType[data_simple$district==5]), col=blues9, xlab="district 5",radius=1)
pie( table(data_simple$buildingType[data_simple$district==6]), col=blues9, xlab="district 6",radius=1)
pie( table(data_simple$buildingType[data_simple$district==7]), col=blues9, xlab="district 7",radius=1)
pie( table(data_simple$buildingType[data_simple$district==8]), col=blues9, xlab="district 8",radius=1)
pie( table(data_simple$buildingType[data_simple$district==9]), col=blues9, xlab="district 9",radius=1)
pie( table(data_simple$buildingType[data_simple$district==10]), col=blues9, xlab="district 10",radius=1)
pie( table(data_simple$buildingType[data_simple$district==11]), col=blues9, xlab="district 11",radius=1)
pie( table(data_simple$buildingType[data_simple$district==12]), col=blues9, xlab="district 12",radius=1)
pie( table(data_simple$buildingType[data_simple$district==13]), col=blues9, xlab="district 13",radius=1)
#plot correlation matrices(heatmap)
par(mfrow=c(1,1))
corrs_all2<-corrs_all[-c(1,2),-c(1,2)]
#library(reshape2)
#melted_corrs <- melt(corrs_all2)
#ggplot(data = melted_corrs, aes(x=Var1, y=Var2, fill=value)) + 
#  geom_tile()
install.packages("corrplot")
library(corrplot)
corrplot(corrs_all2, type = "lower")
 MODELS
training_set$district <- relevel(training_set$district, ref = 13) # use the cheapest district as baseline
model_basic1 <- lm(totalPrice ~ square + district, data = training_set)
summary(model1)

training_set$district <- relevel(training_set$district, ref = 13) # use the cheapest district as baseline
# where is bedroom?? 
model_basic2 <- lm(totalPrice ~ square + district + bathRoom + drawingRoom + livingRoom + subway+ communityAverage, data = training_set)
summary(model_basic2)

# TODO: model selection methods (either backwards or forwards, do we need to implement ourselves? I think so)
# to try:
# - stepwise model selection 
# - using anova in case stepwise is not accepted
# - 1


library(MASS)
# Fit the full model 
full.model <- lm(totalPrice ~square+livingRoom+drawingRoom+kitchen+bathRoom+buildingType+constructionTime
                 +renovationCondition+buildingStructure+elevator+fiveYearsProperty+subway+district+communityAverage
                   , data = training_set)
summary(full.model)
#Stepwise regression model
#Both
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
# living room and elevator removed
step.model$anova

?stepAIC()
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
# Both and backward are the same, so backward is better than forward method.

# use regsubsets
model.regsubsets <- regsubsets(totalPrice ~square+livingRoom+bedRoom+kitchen+bathRoom+buildingType+constructionTime
                               +renovationCondition+buildingStructure+elevator+fiveYearsProperty+subway+district+communityAverage , data = training_set, method = "exhaustive", nvmax = 15)
model.regsubsets.s <- summary(model.regsubsets)
model.regsubsets.s$which
model.regsubsets.s$cp
model.regsubsets.s$adjr2

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
 
# try regsubsets with interactions
model.regsubsets.interactions <-  regsubsets(totalPrice ~square+livingRoom+bedRoom+kitchen+bathRoom+buildingType+constructionTime
                                             +renovationCondition+buildingStructure+elevator+fiveYearsProperty+subway+district+communityAverage+interaction1 + interaction2  , data = training_set, method = "exhaustive", nvmax = 15)
model.regsubsets.interactions.s <- summary(model.regsubsets.interactions)
model.regsubsets.interactions.s$which
model.regsubsets.interactions.s$cp
model.regsubsets.interactions.s$adjr2


# no district,buildingType, buidingStructure, just their interaction 
model.regsubsets.interactions.d <-  regsubsets(totalPrice ~square+livingRoom+bedRoom+kitchen+bathRoom+constructionTime
                                             +renovationCondition+elevator+fiveYearsProperty+subway+communityAverage+interaction1 + interaction2 +interaction4 +interaction5 , data = training_set, method = "exhaustive", nvmax = 15)
model.regsubsets.interactions.d.s <- summary(model.regsubsets.interactions.d)
model.regsubsets.interactions.d.s$which
model.regsubsets.interactions.d.s$cp
model.regsubsets.interactions.d.s$adjr2

# FOR NOW! This is the final model
model.interactions <- rlm(totalPrice ~square+livingRoom+bedRoom+kitchen
                         +renovationCondition+elevator+fiveYearsProperty+subway+communityAverage+interaction1 + interaction2 +interaction4+interaction5 , data = training_set)
summary(model.interactions)

DAAG::vif(model.interactions)
# Values of VIF that exceed 10 are often regarded as indicating multicollinearity, but in weaker models values above 2.5 may be a cause for concern.
(DAAG::vif(model.interactions) < 10)

# plots for residuals 
p <- plot(model.interactions$fitted.values , model.interactions$residuals)
# todo assess residuals
# todo detech significant predictors

# todo: test the predictions with testing data
data_simple[data_simple$totalPrice == 4800.0,]

# final model with log price
model.interactions.log <- lm(totalPriceLog ~square+livingRoom+bedRoom+kitchen
                          +renovationCondition+elevator+fiveYearsProperty+subway+communityAverage+interaction1 + interaction2 +interaction4+interaction5 , data = training_set)
summary(model.interactions.log)

p <- plot(model.interactions.log$fitted.values , model.interactions.log$residuals, main= "res plot of logtotalprice 2")


# final model with log price and square footage as polynomial
model.interactions.log.poly <- lm(totalPriceLog ~square+livingRoom+bedRoom+kitchen
                             +renovationCondition+elevator+fiveYearsProperty+subway+communityAverage+ I(square^2)+interaction1 + interaction2 +interaction4+interaction5 , data = training_set)
summary(model.interactions.log.poly)

p <- plot(model.interactions.log.poly$fitted.values , model.interactions.log.poly$residuals, main= "res plot of logtotalprice polynomial squareft")


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

