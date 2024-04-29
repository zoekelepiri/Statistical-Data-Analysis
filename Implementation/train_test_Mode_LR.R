install.packages("caret")
library(caret)

# Split to train and test set
trainIndex <- createDataPartition(my_data$SalePrice, p = 0.67, list = FALSE, times = 1)

Train <- my_data[trainIndex,]
Test <- my_data[-trainIndex,]

my_data$GarageYrBlt <- as.numeric(substr(my_data$GarageYrBlt,1,4))
str(my_data)
# Train the final model
ModelFinalTraining <- lm(log(SalePrice)~log(YearRemodAdd)+ log(YearBuilt) + log(`@1stFlrSF`) + log(GarageYrBlt) + log(TotRmsAbvGrd) + MSZoning + OverallQual + ExterQual + HeatingQC + OverallCond, data = Train)

# Predict values for the Test set
predicted <- exp(predict(ModelFinalTraining, Test))
actual <- Test$SalePrice

# Error loss function
Error.vector <- actual - predicted

# Absolute error loss function
AE.vector <- abs(actual - predicted)

# Magnitude pf relative error loss dunction
MRE.vector <- (abs(actual - predicted))/ actual


# Magnitude pf relative error to the estimate loss function
MER.vector <- (abs(actual - predicted))/ predicted


list(ME = mean(Error.vector),
     MdE = median(Error.vector),
     MAE = mean(AE.vector),
     MdAE = median(AE.vector),
     MMRE = mean(MRE.vector),
     MdMRE = median(MRE.vector),
     MMER = mean(MER.vector),
     MdMER = median(MER.vector))


