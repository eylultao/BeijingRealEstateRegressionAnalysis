setwd("/Users/eylulaygun/Desktop/Year\ 5/STAT\ 306/Group\ project" )
getwd()
library("dplyr")

# PART1: Data cleaning and general preparation
data <- read.csv("new.csv", header = TRUE, fileEncoding="latin1") # had to typeset encoding to eliminate type convert error
# data simple has DOM removed from it, as the majority of DOM values are na, which reduces dataset size enormously( cuts N in half)
data_simple <- read.csv("new copy.csv", header = TRUE, fileEncoding="latin1") # had to typeset encoding to eliminate type convert error 
str(data) # sanity check
# remove NA values
data_simple <- na.omit( data.frame(data_simple)) 

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

# correlation matrices
corrs_wrt_totalPrice <- cor(select_if(training_set, is.numeric), training_set$totalPrice) 
corrs_all <- cor(select_if(training_set, is.numeric)) # correlation between all variables

# 


