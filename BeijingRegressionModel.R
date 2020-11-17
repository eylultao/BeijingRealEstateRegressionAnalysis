setwd("/Users/eylulaygun/Desktop/Year\ 5/STAT\ 306/Group\ project/BeijingRealEstateRegressionAnalysis" )
getwd()
library("dplyr")

# PART1: Data cleaning and general preparation
# data source: https://www.kaggle.com/ruiqurm/lianjia
#data <- read.csv("new.csv", header = TRUE, fileEncoding="latin1") # had to typeset encoding to eliminate type convert error
# data simple has DOM removed from it, as the majority of DOM values are na, which reduces dataset size enormously( cuts N in half)
data_simple <- read.csv("new copy.csv", header = TRUE, fileEncoding="latin1") # had to typeset encoding to eliminate type convert error 
str(data_simple) # sanity check
# remove NA values
data_simple <- na.omit( data.frame(data_simple)) 
# some columns are char when they have to be integers (eg. livingRoom, drawingRoom, etc)
data_simple$livingRoom <- as.numeric(data_simple$livingRoom)
data_simple$bathRoom <- as.numeric(data_simple$bathRoom)
data_simple$constructionTime <- as.numeric(data_simple$constructionTime)
data_simple$drawingRoom <- as.numeric(data_simple$drawingRoom)
# typecast categorical variables as factors
data_simple$district <- as.factor(data_simple$district)
data_simple$buildingType <- as.factor(data_simple$buildingType)
data_simple$buildingStructure <- as.factor(data_simple$buildingStructure)
# typeset tradeTime to Date Type
data_simple$tradeTime <- as.Date(data_simple$tradeTime)
data_simple$totalPriceLog <- log(data_simple$totalPrice* 10000)

str(data_simple)


# CREATE training and testing data
N <- length(data_simple$id)

set.seed(2020)
all_indices = seq(1, N)
training_indices = sort(sample(1:N, N/2, replace = FALSE))
training_set = data_simple[training_indices,]
testing_indices =  sort(all_indices[!all_indices %in% training_indices]) # remove training indices from set
testing_set = data_simple[testing_indices,]

all.equal(sort(c(training_indices, testing_indices)), all_indices) # sanity check to ensure we separated tests correctly


# EXPLORE CORRELATION BETWEEN VARIABLES
plot(training_set$totalPrice, training_set$square*training_set$price) # perfectly linear relationship
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

# 
# DATA VISUALIZATION
library(ggplot2)
library(data.table) #useful library for doing fast operations on large datasets
# trying to understand the most expensive neighborhoods
mean_price_by_district <- aggregate(training_set[, 4:4 ], list(training_set$district), mean)
mean_price_by_district <- data.frame(district_number = mean_price_by_district$Group.1, mean_price = round( mean_price_by_district$x))
p <- ggplot(data = mean_price_by_district, aes(x = district_number, y = mean_price)) 
p + geom_bar(stat="identity", fill = "steelblue") + geom_text(aes(label=mean_price), vjust=1.5, color="white", size=3.5) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
  ggtitle("Distribution of Mean House Price Across Districts")

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

# TODO : how are building types distributed across districts? 
building_type_by_district <- aggregate(training_set[, 11:11 ], list(training_set$district), )

var_price_by_district <- aggregate(training_set[, 4:4 ], list(training_set$district), var)
var_price_by_district <- data.frame(district_number = var_price_by_district$Group.1, var_price = round( var_price_by_district$x))
p <- ggplot(data = var_price_by_district, aes(x = district_number, y = var_price)) 
p + geom_bar(stat="identity", fill = "steelblue") + geom_text(aes(label=var_price), vjust=1.5, color="white", size=3.5) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
  ggtitle("Distribution of Var House Price Across Districts")


# BASIC LINEAR MODELS
training_set$district <- relevel(training_set$district, ref = 13) # use the cheapest district as baseline
model_basic1 <- lm(totalPrice ~ square + district, data = training_set)
summary(model1)

training_set$district <- relevel(training_set$district, ref = 13) # use the cheapest district as baseline
# where is bedroom?? 
model_basic2 <- lm(totalPrice ~ square + district + bathRoom + drawingRoom + livingRoom + subway+ communityAverage, data = training_set)
summary(model_basic2)

# TODO: model selection methods (either backwards or forwards, do we need to implement ourselves?)
# to try:
# - stepwise model selection 
# - using anova in case stepwise is not accepted
# - 
