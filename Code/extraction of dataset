

#########################
# Importing Libraries and Dataset
library(dplyr)
library(tidyr)
library(ggplot2)
library(randomForest)
library(caret)
library(corrplot)
library(cowplot)
library(DT)

# Load data
data <- read.csv("C:/Users/wwwsa/OneDrive/Airbnb_NYC_2019.csv")
scoringData <- read.csv("C:/Users/wwwsa/OneDrive/Airbnb_NYC_2019.csv")

###################################
# Data Exploration
str(data) # Check the structure of the data
# Combine train and test
scoringData$price <- NA
scoringData$zipcode <- as.factor(scoringData$zipcode)
allData <- rbind(data, scoringData)

## Understand the price variable
# Price distribution
ggplot(data = allData, aes(x = price)) +
  geom_histogram()

## Correlation with price
numericVars <- which(sapply(allData, is.numeric))
numericVarNames <- names(numericVars)
cat('There are', length(numericVars), 'numeric variables')

allData_numVars <- allData[, numericVars]
allData_numVars$id <- NULL
cor_numVars <- cor(allData_numVars, use = "pairwise.complete.obs")
cor_sorted <- as.matrix(sort(cor_numVars[,'price'], decreasing = TRUE))
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x) > 0.2)))
cor_numVars <- cor_numVars[CorHigh, CorHigh]
corrplot.mixed(cor_numVars, tl.col = "black", tl.pos = "lt")

## Visualization of selected predictors
g1 <- ggplot(data = allData, aes(x = weekly_price)) +
  geom_histogram()
c1 <- ggplot(data = allData, aes(x = weekly_price, y = price)) +
  geom_point()
plot_grid(g1, c1, labels = "AUTO")

g2 <- ggplot(data = allData, aes(x = cleaning_fee)) +
  geom_histogram()
c2 <- ggplot(data = allData, aes(x = cleaning_fee, y = price)) +
  geom_point()
plot_grid(g2, c2, labels = "AUTO")

g3 <- ggplot(data = allData, aes(x = accommodates)) +
  geom_histogram()
c3 <- ggplot(data = allData, aes(x = accommodates, y = price)) +
  geom_point()
plot_grid(g3, c3, labels = "AUTO")

###################################
# Preparing the Data for Analysis
# Handling missing values
# Cleaning_fee missing data
data[is.na(data$cleaning_fee), "cleaning_fee"] <- 0
scoringData[is.na(scoringData$cleaning_fee), "cleaning_fee"] <- 0

# Impute missing values for beds
data[is.na(data$beds), "beds"] <- median(data$beds, na.rm = TRUE)
scoringData[is.na(scoringData$beds), "beds"] <- median(scoringData$beds, na.rm = TRUE)

# Combine train and test data
scoringData$property_type <- as.factor(scoringData$property_type)
combinedData <- rbind(data, scoringData)

# Handle factor levels
combinedData$property_type <- factor(combinedData$property_type, levels = levels(data$property_type))

# Fix missing values in reviews_per_month
combinedData[is.na(combinedData$reviews_per_month), "reviews_per_month"] <- 0

###################################
# Modeling
# Split the data back into train and test datasets
trainData <- combinedData[1:nrow(data), ]
testData <- combinedData[(nrow(data) + 1):nrow(combinedData), ]

# Random Forest model
rfModel <- randomForest(price ~ ., data = trainData, ntree = 500)

# Predict on test data
testPredictions <- predict(rfModel, newdata = testData)

###################################
# Result Summary
# Calculate RMSE on test data
testRMSE <- sqrt(mean((testPredictions - testData$price)^2))

# Create a data frame with results
result <- data.frame(Model = "Random Forest", RMSE = testRMSE)

# Display the result using DT
datatable(result)

