install.packages("haven")
library("haven")
# Loading
install.packages("xlsx")
library("xlsx")
install.packages("summarytools")
library("summarytools")
install.packages("ggplot2")
library("ggplot2")
my_data <- ames_housing #read the excel file
str(my_data)
summary(my_data)

#NULL VALUES CHECK
count_null <- sum(my_data$TotalBsmtSF == 0, na.rm = TRUE)
my_data$TotalBsmtSF[my_data$TotalBsmtSF == 0] <- NA
my_data <- na.omit(my_data)

my_data[my_data == "#NULL!"] <- NA

#Exclude NA values from the dataset
df_filtered <- my_data%>%filter(!is.na(LotFrontage))
df_filtered <- df_filtered%>%filter(!is.na(GarageYrBlt))
my_data <- df_filtered
# Drop columns Street and Utilities 
my_data <- subset(my_data, select = -c(Street, Utilities, Order))

new_df = subset(my_data, select = c(LotFrontage,LotArea, YearBuilt, YearRemodAdd, TotalBsmtSF, `@1stFlrSF`, `@2ndFlrSF`,BedroomAbvGr, KitchenAbvGr, KitchenAbvGr, TotRmsAbvGrd, Fireplaces, GarageYrBlt, PoolArea, YrSold,SalePrice))
new_df = data.frame(lapply(new_df, function(x) as.numeric(as.factor(x))-1))

par(mfrow = c(5, 3),mai=c(0.3,0.3,0.3,0.3))
for (i in 1:ncol(new_df)){
  hist(new_df[[i]], main=paste("Histogram", names(new_df)[i]), xlab = paste("Values",names(new_df)[i]), col = "white", border = 4)
  box(lty = "solid")
}

# -------------------------BOX PLOT -----------------------
#Box plot 
par(mfrow = c(5, 3),mai=c(0.2,0.2,0.2,0.2))
for (i in 1:ncol(new_df)){
  boxplot(new_df[[i]], 
          main = paste("Box Plot ", names(new_df)[i]), 
          xlab = paste("Values", names(new_df)[i]),
          col = "white",
          border = 4,
          notch = FALSE
  )
  box()
}
par(mfrow = c(4, 2),mai=c(0.2,0.2,0.2,0.2))
for (i in 1:4){
  ggplot(my_data, aes(y = log(SalePrice), x = TotalBsmtSF)) +geom_boxplot(color  = 4)
  ggplot(my_data, aes(y = log(SalePrice), x = YearBuilt)) +geom_boxplot(color  = 4)
  ggplot(my_data, aes(y = log(SalePrice), x = MSZoning)) +geom_boxplot(color  = 4)
  ggplot(my_data, aes(y = log(SalePrice), x = CentralAir)) +geom_boxplot(color  = 4)
}
ggplot(my_data, aes(y = log(SalePrice), x = TotalBsmtSF)) +geom_boxplot(color  = 4)
ggplot(my_data, aes(y = log(SalePrice), x = YearBuilt)) +geom_boxplot(color  = 4)
ggplot(my_data, aes(y = log(SalePrice), x = MSZoning)) +geom_boxplot(color  = 4)
ggplot(my_data, aes(y = log(SalePrice), x = CentralAir)) +geom_boxplot(color  = 4)


# BOX PLOT 2
variable_names <- colnames(my_data)
par(mfrow = c(6, 4), mai = c(0.2, 0.2, 0.2, 0.2))
# Loop through each variable and create a boxplot
for (i in 1:ncol(my_data)) {
  ggplot(my_data, aes(x = my_data[[i]], y = my_data$SalePrice )) +
    geom_boxplot() +
    labs(title = paste("Box Plot for", variable_names[i]), x = "Sale Price", y = variable_names[i]) +
    theme_minimal()
}
ggplot(my_data, aes(y = MSZoning, x = SalePrice)) +geom_boxplot()
ggplot(my_data, aes(x = MSZoning, y = SalePrice)) +
  geom_boxplot() +
  labs(title = paste("Box Plot for", variable_names[i]), x = "Sale Price", y = variable_names[i]) +
  theme_minimal()