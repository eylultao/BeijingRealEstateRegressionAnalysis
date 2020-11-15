setwd("/Users/eylulaygun/Desktop/Year\ 5/STAT\ 306/Group\ project" )
getwd()
library("dplyr")

# PART1: Data cleaning and general preparation
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

# correlation matrices
corrs_wrt_totalPrice <- cor(select_if(training_set, is.numeric), training_set$totalPrice) 
corrs_all <- cor(select_if(training_set, is.numeric)) # correlation between all variables

# 

