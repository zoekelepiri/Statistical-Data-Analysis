install.packages("caret")
library(caret)

# Split to train and test set
trainIndex <- createDataPartition(my_data$SalePrice, p = 0.67, list = FALSE, times = 1)

Train <- my_data[trainIndex,]
Test <- my_data[-trainIndex,]


install.packages("class")

# knn_model <- knn(train = Train[, 1:4], test = Test[log(YearRemodAdd)+ log(YearBuilt) + log(`@1stFlrSF`) + log(GarageYrBlt) + log(TotRmsAbvGrd) + MSZoning + OverallQual + ExterQual + HeatingQC + OverallCond], cl = Train$SalePrice, k = k)


# Convert MSZ zoning to one hot vector
my_data <- cbind(my_data, model.matrix(~ MSZoning - 1, data = my_data))
# Drop column
my_data$MSZoning <- NULL

# Convert OverallQual to one hot vector
my_data <- cbind(my_data, model.matrix(~ OverallQual - 1, data = my_data))
# Drop column
my_data$OverallQual <- NULL



# Convert ExterQual to one hot vector
my_data <- cbind(my_data, model.matrix(~ ExterQual - 1, data = my_data))
# Drop column
my_data$ExterQual <- NULL

#HeatingQC

# Convert ExterQual to one hot vector
my_data <- cbind(my_data, model.matrix(~ HeatingQC - 1, data = my_data))
# Drop column
my_data$HeatingQC <- NULL

#OverallCond
# Convert ExterQual to one hot vector
my_data <- cbind(my_data, model.matrix(~ OverallCond - 1, data = my_data))
# Drop column
my_data$OverallCond <- NULL


# Load the required library for KNN
library(class)

# Specify the number of neighbors (k)
k <- 20


selected_columns <- c('YearRemodAdd', 'YearBuilt', '@1stFlrSF', 'GarageYrBlt', 'TotRmsAbvGrd')
one_hot_columns <- names(my_data)[19:50]  # Example of column names for one-hot encoded 'MSZoning', adjust as needed
all_columns <- c(selected_columns, one_hot_columns)

print(all_columns)

knn_model <- knn(train = Train[, all_columns, drop = FALSE], 
                 test = Test[, all_columns, drop = FALSE], 
                 cl = Train$SalePrice, 
                 k = k)


# Print the confusion matrix
conf_matrix <- table(Actual = Test$SalePrice, Predicted = knn_model)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy: ", accuracy, "\n")

# Extract true values and predicted values
true_values <- Test$SalePrice
predicted_values <- knn_model

if(is.factor(predicted_values)) {
  predicted_values <- as.numeric(as.character(predicted_values))
}

if(is.factor(true_values)) {
  true_values <- as.numeric(as.character(true_values))
}


# Calculate error metrics
absolute_error <- abs(predicted_values - true_values)

print(mean(absolute_error))
print(median(absolute_error))


print(median(absolute_error))
 
error <- predicted_values - true_values
print(mean(error))
print(median(error))

magnitude_error <- abs(error)
print(mean(magnitude_error))

print(median(magnitude_error))

relative_error <- error / true_values
magnitude_relative_error <- abs(relative_error)

print(mean(magnitude_relative_error))

print(median(magnitude_relative_error))





# Error loss function
ErrorKnn.vector <- true_values - predicted_values

# Absolute error loss function
AEKnn.vector <- abs(true_values - predicted_values)

# Magnitude pf relative error loss dunction
MREKnn.vector <- (abs(true_values - predicted_values))/ true_values


# Magnitude pf relative error to the estimate loss function
MERKnn.vector <- (abs(true_values - predicted_values))/ predicted_values


list(MEKnn = mean(ErrorKnn.vector),
     MdEKnn = median(ErrorKnn.vector),
     MAEKnn = mean(AEKnn.vector),
     MdAEKnn = median(AEKnn.vector),
     MMREKnn = mean(MREKnn.vector),
     MdMREKnn = median(MREKnn.vector),
     MMERKnn = mean(MERKnn.vector),
     MdMERKnn = median(MERKnn.vector))

wilcox.test(Error.vector, ErrorKnn.vector, paired = TRUE)
wilcox.test(AE.vector, AEKnn.vector, paired = TRUE)
wilcox.test(MRE.vector, MREKnn.vector, paired = TRUE)
wilcox.test(MER.vector, MERKnn.vector, paired = TRUE)


