setwd("/Users/eylulaygun/Desktop/Year\ 5/STAT\ 306/Group\ project/BeijingRealEstateRegressionAnalysis" )

library("dplyr")
library(data.table) #useful library for doing fast operations on large datasets

data_simple <- read.csv("new copy.csv", header = TRUE, fileEncoding="latin1")# had to typeset encoding to eliminate type convert error 
str(data_simple) # sanity check
data_simple <- na.omit( data.frame(data_simple)) # remove NA values
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

##### NOW RESIDUAL PLOTS FOR ALL 6 ATTEMPTS

# attempt 1
attempt1 <- lm(totalPrice ~square+livingRoom+kitchen+buildingType
               +renovationCondition+buildingStructure+fiveYearsProperty+district+communityAverage , data = training_set)
plot( attempt1$fitted.values,attempt1$residuals, main= "Residual plot for attempt1")


# attempt 2
attempt2 <- lm(totalPriceLog ~square+livingRoom+bedRoom+kitchen+bathRoom+buildingType+constructionTime
                                   +renovationCondition+buildingStructure+elevator+fiveYearsProperty+subway+district+communityAverage , data = training_set)
plot( attempt2$fitted.values,attempt2$residuals, main= "Residual plot for attempt2")


# attempt 3
attempt3 <-  lm(totalPrice ~square+livingRoom+bedRoom+kitchen+bathRoom+buildingType+constructionTime
                                             +renovationCondition+buildingStructure+elevator+fiveYearsProperty+subway+district+communityAverage+interaction1 + interaction2  , data = training_set)
plot( attempt3$fitted.values,attempt3$residuals, main= "Residual plot for attempt3")

# attempt 4
attempt4 <- lm(totalPrice ~square+livingRoom+bedRoom+kitchen+bathRoom+constructionTime
                                                           +renovationCondition+elevator+fiveYearsProperty+subway+communityAverage+interaction1 + interaction2 +interaction4 +interaction5 , 
                                                           data = training_set)
plot( attempt4$fitted.values,attempt4$residuals, main= "Residual plot for attempt4")

#attempt 5
attempt5 <- lm(totalPriceLog ~square+bedRoom+kitchen +renovationCondition+fiveYearsProperty+
                 subway+communityAverage+ I(square^2)+interaction1 + interaction2 +interaction4+interaction5
               , data = training_set)
plot( attempt5$fitted.values,attempt5$residuals, main= "Residual plot for attempt5")

# attempt 6 (final)
training_set$rencond <- as.factor(training_set$renovationCondition)
training_set$rencond <- relevel(training_set$rencond , ref =2)

attempt6 <-  lm(totalPriceLog ~square+bedRoom+kitchen
                +rencond+fiveYearsProperty+subway+communityAverage+ I(square^2)+interaction1  
                +interaction4+interaction5 + Year , data = training_set)

plot( attempt6$fitted.values,attempt6$residuals, main= "Residual plot for attempt6")
qqnorm(attempt6$residuals)
# qq normal plot of standardized residuals
attempt6.standardized <- rstandard(attempt6)
qqnorm(attempt6.standardized, main ="standardized residuals qq plot")
qqline(attempt6.standardized)
