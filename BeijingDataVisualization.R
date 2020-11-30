setwd("C:/Users/Kayson/Documents/GitHub/BeijingRealEstateRegressionAnalysis" )
setwd("/Users/eylulaygun/Desktop/Year\ 5/STAT\ 306/Group\ project/BeijingRealEstateRegressionAnalysis" )
getwd()
library("dplyr")
library(ggplot2) # for fancier data visualization
library(data.table) #useful library for doing fast operations on large datasets


# PART1: Data cleaning and general preparation
# data source: https://www.kaggle.com/ruiqurm/lianjia
# data simple has DOM removed from it, as the majority of DOM values are na, which reduces dataset size enormously( cuts N in half)
data_simple <- read.csv("new copy.csv", header = TRUE, fileEncoding="latin1") # had to typeset encoding to eliminate type convert error 
# data_simple <- read.csv("new copy.csv", header = TRUE) # comment out depending on the computer language  


data_simple <- na.omit( data.frame(data_simple)) # remove NA values
data_simple <- data_simple[data_simple$totalPrice >10,] # remove all houses less than 10 totalPrice
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
data_simple$totalPriceLog <- log(data_simple$totalPrice* 10000)
str(data_simple) # sanity check

# Part 2: create training and testing data
N <- length(data_simple$id)
set.seed(2020)
all_indices = seq(1, N)
training_indices = sort(sample(1:N, 3*N/5, replace = FALSE)) # ratio is 60/40
training_set = data_simple[training_indices,]
testing_indices =  sort(all_indices[!all_indices %in% training_indices]) # remove training indices from set
testing_set = data_simple[testing_indices,]

# Part 3: EXPLORE CORRELATION BETWEEN VARIABLES, check for linear relationships
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


# Part 4: DATA VISUALIZATION
#construct side by side boxplots to visualize the total prices across different building types
boxplot(totalPrice~buildingType,
        data=training_set,
        main="Different boxplots for building type",
        xlab="differnent building type",
        ylab="total price",
        col="orange",
        border="black"
)
#construct side by side boxplots to visualize the total prices across different building structures
boxplot(totalPrice~buildingStructure,
        data=training_set,
        main="Different boxplots for building type",
        xlab="differnent building type",
        ylab="total price",
        col="orange",
        border="black"
)

# Mean housing price accross different districts -> understanding most expensive neighborhoods
mean_price_by_district <- aggregate(training_set[, 4:4 ], list(training_set$district), mean)
mean_price_by_district <- data.frame(district_number = mean_price_by_district$Group.1, mean_price = round( mean_price_by_district$x))
p_mean_by_district <- ggplot(data = mean_price_by_district, aes(x = district_number, y = mean_price)) 
p_mean_by_district + geom_bar(stat="identity", fill = "steelblue") + geom_text(aes(label=mean_price), vjust=1.5, color="white", size=3.5) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
  ggtitle("Distribution of Mean House Price Across Districts")

# Variance of housing price accross different districts
var_price_by_district <- aggregate(training_set[, 4:4 ], list(training_set$district), var)
var_price_by_district <- data.frame(district_number = var_price_by_district$Group.1, variance_price = round( var_price_by_district$x))
p_var_by_district <- ggplot(data = var_price_by_district, aes(x = district_number, y = variance_price)) 
p_var_by_district + geom_bar(stat="identity", fill = "steelblue") + geom_text(aes(label=variance_price), vjust=1.5, color="white", size=3.5) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
  ggtitle("Distribution of Variance of House Price Across Districts")

# How is community score changing across districts
mean_community_score_by_district <- aggregate(training_set[, 19:19 ], list(training_set$district), mean)
mean_community_score_by_district <- data.frame(district_number = mean_community_score_by_district$Group.1, community_avg = round( mean_community_score_by_district$x))
p_score_by_district <- ggplot(data = mean_community_score_by_district, aes(x = district_number, y = community_avg)) 
p_score_by_district + geom_bar(stat="identity", fill = "steelblue") + geom_text(aes(label=community_avg), vjust=1.5, color="white", size=3.5) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  ggtitle("Distribution of Mean Community Score Across Districts")
# the bar plot of community score per district is very similar to the mean housing prices per district
# potential multicollinearity problem, will need to be adressed

#The overall proportion of 4 building types.
district=data_simple[data_simple$district,]
building_type_by_district <- table(district$buildingType)
pie(building_type_by_district)


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
# TODO: can we make them appear bigger?

#plot correlation matrices(heatmap)
par(mfrow=c(1,1))
corrs_all2<-corrs_all[-c(1,2),-c(1,2)]

#install.packages("corrplot")
library(corrplot)
corrplot(corrs_all2, type = "lower")

# overall, for all the plots we have, it would be nice to give them titles,
# specify the x,y axis notation, and include what they they mean. 
# eg: BuildingType (Categorical:  tower(1), bungalow(2), plate and tower(3), plate(4))


data_simple$Year <- as.numeric(format(data_simple$tradeTime, format = "%Y" ))
group_year <- aggregate( data_simple[, 4:4 ], list(data_simple$Year), mean)

