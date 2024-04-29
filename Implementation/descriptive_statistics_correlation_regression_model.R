#installations 
install.packages("haven")
library("haven")
# Loading
install.packages("xlsx")
library("xlsx")
install.packages("summarytools")
library("summarytools")
install.packages("ggplot2")
library("ggplot2")
install.packages("psych")
library("psych")
install.packages("DescTools")
library(DescTools)
install.packages("DTK")
library(DTK)
install.packages("magrittr") 
library(magrittr)
install.packages("dplyr")
library(dplyr)
install.packages("GGally")
library(GGally)
install.packages("hms")
install.packages("car")
library("car")
install.packages("corrplot")
library(corrplot)
install.packages("e1071")
library(e1071)
install.packages("tidyverse")
library(tidyverse)

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
my_data <- subset(my_data, select = -c(Street, Utilities))

lapply(new_df, function(x) shapiro.test(x)$p.value)

#check normal distribution
shapiro.test(my_data$SalePrice) # p<0.05 kai ara den exoume kanoniki katanomi
ggplot(my_data, aes(x=SalePrice))+ geom_histogram()

#CHECK THE NORMALLITY
skew_value <- skewness(my_data$`@2ndFlrSF`)
print(skew_value)

shapiro.test(my_data$Order) # p<0.05 kai ara den exoume kanoniki katanomi
ggplot(my_data, aes(x=Order))+ geom_histogram(bins = 50,color = 4, fill = "white")
ggplot(df_filtered, aes(sample=Order)) + stat_qq_line() + stat_qq()


shapiro.test(as.numeric(substr(df_filtered$LotFrontage,1,4))) #no
ggplot(my_data, aes(x=as.numeric(substr(df_filtered$LotFrontage,1,4))))+ geom_histogram(bins = 30,color = 4, fill = "white")
ggplot(df_filtered, aes(sample=LotFrontage)) + stat_qq_line() + stat_qq()

shapiro.test(df_filtered$LotArea) #no
ggplot(my_data, aes(x=LotArea))+ geom_histogram(bins = 30,color = 4, fill = "white")
ggplot(df_filtered, aes(sample=LotArea)) + stat_qq_line() + stat_qq()

shapiro.test(df_filtered$YearBuilt) #no
ggplot(df_filtered, aes(x=YearBuilt))+ geom_histogram(bins = 30,color = 4, fill = "white")
ggplot(df_filtered, aes(sample=YearBuilt)) + stat_qq_line() + stat_qq()

shapiro.test(df_filtered$YearRemodAdd) #no
ggplot(df_filtered, aes(x=YearRemodAdd))+ geom_histogram(bins = 30,color = 4, fill = "white")
ggplot(df_filtered, aes(sample=YearRemodAdd)) + stat_qq_line() + stat_qq()

shapiro.test(df_filtered$TotalBsmtSF) #no
ggplot(df_filtered, aes(x=TotalBsmtSF))+ geom_histogram(bins = 30,color = 4, fill = "white")
ggplot(df_filtered, aes(sample=TotalBsmtSF)) + stat_qq_line() + stat_qq()

shapiro.test(df_filtered$`@1stFlrSF`) #no
ggplot(df_filtered, aes(x=`@1stFlrSF`))+ geom_histogram(bins = 30,color = 4, fill = "white")
ggplot(df_filtered, aes(sample=`@1stFlrSF`)) + stat_qq_line() + stat_qq()

shapiro.test(df_filtered$`@2ndFlrSF`) #no
ggplot(df_filtered, aes(x=`@2ndFlrSF`))+ geom_histogram(bins = 30,color = 4, fill = "white")
ggplot(df_filtered, aes(sample=`@2ndFlrSF`)) + stat_qq_line() + stat_qq()

shapiro.test(df_filtered$BedroomAbvGr) #no
ggplot(df_filtered, aes(x=BedroomAbvGr))+ geom_histogram(bins = 30,color = 4, fill = "white")
ggplot(df_filtered, aes(sample=BedroomAbvGr)) + stat_qq_line() + stat_qq()

shapiro.test(df_filtered$KitchenAbvGr) #no
ggplot(df_filtered, aes(x=KitchenAbvGr))+ geom_histogram()
ggplot(df_filtered, aes(sample=KitchenAbvGr)) + stat_qq_line() + stat_qq()

shapiro.test(df_filtered$TotRmsAbvGrd) #no
ggplot(df_filtered, aes(x=TotRmsAbvGrd))+ geom_histogram()

ggplot(df_filtered, aes(sample=TotRmsAbvGrd)) + stat_qq_line() + stat_qq()

shapiro.test(df_filtered$Fireplaces) #no
ggplot(df_filtered, aes(x=Fireplaces))+ geom_histogram()
ggplot(df_filtered, aes(sample=Fireplaces)) + stat_qq_line() + stat_qq()

#check
shapiro.test(as.numeric(substr(df_filtered$GarageYrBlt,1,4))) #no
ggplot(df_filtered, aes(x=GarageYrBlt))+ geom_histogram()
ggplot(df_filtered, aes(sample=GarageYrBlt)) + stat_qq_line() + stat_qq()

shapiro.test(df_filtered$PoolArea) #no
ggplot(df_filtered, aes(x=PoolArea))+ geom_histogram()
ggplot(df_filtered, aes(sample=PoolArea)) + stat_qq_line() + stat_qq()

shapiro.test(df_filtered$YrSold) #no
ggplot(df_filtered, aes(x=YrSold))+ geom_histogram()
ggplot(df_filtered, aes(sample=YrSold)) + stat_qq_line() + stat_qq()

#T-TEST && ANOVA

#1st
describeBy(my_data$SalePrice, my_data$CentralAir)
ggplot(my_data, aes(y=log(SalePrice), x = CentralAir)) + geom_boxplot()
leveneTest(log(SalePrice) ~ CentralAir, center = mean, data = my_data) # p > 0.05 --> t-test

#ANOVA
#2nd --> MSZoning
describeBy(my_data$SalePrice, my_data$MSZoning)
anovaMSZoning <- aov(log(SalePrice)~MSZoning, data = my_data)
summary(anovaMSZoning)
#Ζευγαρωτές συγκρίσεις
leveneTest(log(SalePrice)~MSZoning, center = mean, data = my_data)
DunnettTest(log(my_data$SalePrice), my_data$MSZoning) # diaferoun oxi oles oi diaspores

#3rd --> OverallQual
describeBy(my_data$SalePrice, my_data$OverallQual)
anovaOverallQual <- aov(log(SalePrice)~OverallQual, data = my_data)
summary(anovaOverallQual)
leveneTest(log(SalePrice)~OverallQual, center = mean, data = my_data) # p < 0.05 important difference
DunnettTest(log(my_data$SalePrice), my_data$OverallQual) # oles oi diaspores diaferoun

#4rd --> OverallCond
describeBy(my_data$SalePrice, my_data$OverallCond)
anovaOverallCond <- aov(log(SalePrice)~OverallCond, data = my_data)
summary(anovaOverallCond) # p << 
leveneTest(log(SalePrice)~OverallCond, center = mean, data = my_data) # p < 0.05 important difference
DunnettTest(log(my_data$SalePrice), my_data$OverallCond) # kapoies diaferoun

#5th --> ExterQual
describeBy(my_data$SalePrice, my_data$ExterQual)
anovaExterQual <- aov(log(SalePrice)~ExterQual, data = my_data)
summary(anovaExterQual) # p << 
leveneTest(log(SalePrice)~ExterQual, center = mean, data = my_data) # p < 0.05 important difference
DunnettTest(log(my_data$SalePrice), my_data$ExterQual) # oles diaferoun

#6th --> BsmtCond 
describeBy(my_data$SalePrice, my_data$BsmtCond)
anovaBsmtCond <- aov(log(SalePrice)~BsmtCond, data = my_data)
summary(anovaBsmtCond) # p << 
leveneTest(log(SalePrice)~BsmtCond, center = mean, data = my_data) # p > 0.05 non important difference
#DunnettTest(log(my_data$SalePrice), my_data$BsmtCond) 

#7th --> HeatingQC
describeBy(my_data$SalePrice, my_data$HeatingQC)
anovaHeatingQC <- aov(log(SalePrice)~HeatingQC, data = my_data)
summary(anovaHeatingQC) # p << 
leveneTest(log(SalePrice)~HeatingQC, center = mean, data = my_data) # p < 0.05 important difference
DunnettTest(log(my_data$SalePrice), my_data$HeatingQC) #ola diaferoun

#8th --> Central Air
describeBy(my_data$SalePrice, my_data$CentralAir)
anovaCentralAir <- aov(log(SalePrice)~CentralAir, data = my_data)
summary(anovaCentralAir) # p << 
leveneTest(log(SalePrice)~CentralAir, center = mean, data = my_data) # p < 0.05 important difference
DunnettTest(log(my_data$SalePrice), my_data$CentralAir) #ola diaferoun

#CORRELATION

#Order
ggplot(my_data, aes(x=Order, y = SalePrice)) + geom_point() + labs(x = "Order Values", y = "Sales Price Values")
cor.test(y = my_data$SalePrice ,x = my_data$Order, method = "spearman") # no correlation between them

#Lot Frontage
ggplot(my_data, aes(x=LotFrontage, y = SalePrice)) + geom_point() + labs(x = "Lot Fontage Values", y = "Sales Price Values")
#make the lot fontage numeric because was string 
cor.test(y = df_filtered$SalePrice ,x = as.integer(df_filtered$LotFrontage), method = "spearman") # positive correlation xamili

#Lot area
ggplot(df_filtered, aes(x=LotArea, y = SalePrice)) + geom_point() + labs(x = "Lot Area Values", y = "Sales Price Values")
cor.test(y = df_filtered$SalePrice ,x = df_filtered$LotArea, method = "spearman") # positive correlation metria
#0.4383469 

#year bulit near the normal distribution
ggplot(df_filtered, aes(x=YearBuilt, y = SalePrice)) + geom_point() + labs(x = "Year Built Values", y = "Sales Price Values")
cor.test(y = df_filtered$SalePrice ,x = df_filtered$YearBuilt, method = "spearman") # positive metria correlation between them
#0.6960984 

#Year Remod Add
ggplot(df_filtered, aes(x=YearRemodAdd, y = SalePrice)) + geom_point() + labs(x = "Year Remod Add Values", y = "Sales Price Values")
cor.test(y = df_filtered$SalePrice ,x = df_filtered$YearRemodAdd, method = "spearman") # positive ipsili correlation between them
#0.6350473 

#TotalBsmtSF
ggplot(df_filtered, aes(x=TotalBsmtSF, y = SalePrice)) + geom_point() + labs(x = "TotalBsmtSF Values", y = "Sales Price Values")
cor.test(y = df_filtered$SalePrice ,x = df_filtered$TotalBsmtSF, method = "spearman") # positive ipsili correlation between them
#0.6238983 

#@1stFlrSF
ggplot(df_filtered, aes(x=`@1stFlrSF`, y = SalePrice)) + geom_point() + labs(x = "@1stFlrSF Values", y = "Sales Price Values")
cor.test(y = df_filtered$SalePrice ,x = df_filtered$`@1stFlrSF`, method = "spearman") # positive ipsili correlation between them

#@2ndFlrSF
ggplot(df_filtered, aes(x=`@2ndFlrSF`, y = SalePrice)) + geom_point() + labs(x = "@2ndFlrSF Values", y = "Sales Price Values")
cor.test(y = df_filtered$SalePrice ,x = df_filtered$`@2ndFlrSF`, method = "spearman") # positive xamili correlation between them

#BedroomAbvGr
ggplot(my_data, aes(x=BedroomAbvGr, y = SalePrice)) + geom_point() + labs(x = "BedroomAbvGr Values", y = "Sales Price Values")
cor.test(y = df_filtered$SalePrice ,x = df_filtered$BedroomAbvGr, method = "spearman") # positive poli xamili correlation between them

#KitchenAbvGr
ggplot(my_data, aes(x=KitchenAbvGr, y = SalePrice)) + geom_point() + labs(x = "KitchenAbvGr Values", y = "Sales Price Values")
cor.test(y = df_filtered$SalePrice ,x = df_filtered$KitchenAbvGr, method = "spearman") # no correlation between them

#TotRmsAbvGrd
ggplot(df_filtered, aes(x=TotRmsAbvGrd, y = SalePrice)) + geom_point() + labs(x = "TotRmsAbvGrd Values", y = "Sales Price Values")
cor.test(y = df_filtered$SalePrice ,x = df_filtered$TotRmsAbvGrd, method = "spearman") # positive metria correlation between them

#Fireplaces
ggplot(df_filtered, aes(x=Fireplaces, y = SalePrice)) + geom_point() + labs(x = "Fireplaces Values", y = "Sales Price Values")
cor.test(y = df_filtered$SalePrice ,x = df_filtered$Fireplaces, method = "spearman") # positive metria correlation between them

#GarageYrBlt
ggplot(df_filtered, aes(x=GarageYrBlt, y = SalePrice)) + geom_point() + labs(x = "GarageYrBlt Values", y = "Sales Price Values")
cor.test(y = df_filtered$SalePrice ,x = as.numeric(substr(df_filtered$GarageYrBlt,1,4)), method = "spearman") # positive metria pros ipsili correlation between them

#PoolArea
ggplot(df_filtered, aes(x=PoolArea, y = SalePrice)) + geom_point() + labs(x = "PoolArea VAlues", y = "Sales Price Values")
cor.test(y = df_filtered$SalePrice ,x = df_filtered$PoolArea, method = "spearman") # positive poli xamili eos katholou correlation between them

#YrSold
ggplot(df_filtered, aes(x=YrSold, y = SalePrice)) + geom_point() + labs(x = "YrSold VAlues", y = "Sales Price Values")
cor.test(y = df_filtered$SalePrice ,x = df_filtered$YrSold, method = "spearman") # no correlation between them

#Normal Distribution Check
shapiro.test(df_filtered$Order) #no
ggplot(df_filtered, aes(x=Order))+ geom_histogram()
ggplot(df_filtered, aes(sample=Order)) + stat_qq_line() + stat_qq()

#CORRELATION IN GRAPH
ggpairs(df_filtered, columns = c(19,1,4))
ggpairs(df_filtered, columns = c(19, 1, 4), upper = list(continuous = wrap("cor", method = "spearman")))

new_df = subset(df_filtered, select = c(Order,LotFrontage, LotArea, YearBuilt, YearRemodAdd, TotalBsmtSF, BedroomAbvGr, KitchenAbvGr, `@1stFlrSF`, TotRmsAbvGrd, Fireplaces, PoolArea, YrSold,GarageYrBlt,SalePrice))
new_df = data.frame(lapply(new_df, function(x) as.numeric(as.factor(x))-1))

ggpairs(df_filtered, columns = c(15,1,2,3,4,5,6), upper = list(continuous = wrap("cor", method = "spearman")))
ggpairs(new_df, columns = c(15,1,2,3,4,5,6,7,8,9,10,11,12,13,14), upper = list(continuous = wrap("cor", method = "spearman")))

my_data$GarageYrBlt <- as.integer(my_data$GarageYrBlt)
my_data$LotFrontage <- as.integer(my_data$LotFrontage)

# CORRELATION PLOT 
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
M<-cor(new_df)
corrplot(M, method="number")
ggcorr(new_df, nbreaks = 6, label = TRUE, label_size = 3, color = "grey50", size = 3,method = c("all.obs", "spearman"))

# skewness & kurtosis
skewness_values <- sapply(new_df, skewness)
print(skewness_values)

skew_value <- skewness(new_df$Order)
print(skew_value)

kurtosis_values <- sapply(new_df, kurtosis)
print(kurtosis_values)

#LINAER REGRESSION MODEL 

# model 1.0
model1 <- lm(log(SalePrice)~log(YearBuilt), data = my_data)
summary(model1)

# p-value: < 2.2e-16


#2.0
model2 <- lm(log(SalePrice)~log(GarageYrBlt) + log(YearBuilt), data = my_data)
summary(model2)

#  p-value: < 2.2e-16


anova(model1, model2) # Choose model 2

# 2.2e-16


#3.0
model3 <- lm(log(SalePrice)~log(GarageYrBlt) +log(YearRemodAdd)+ log(YearBuilt), data = my_data)
summary(model3)
# 2.2e-16


anova(model2, model3) # Choose model 3

# 2.2e-16


#4.0
model4 <- lm(log(SalePrice)~log(GarageYrBlt) + log(YearRemodAdd)+ log(YearBuilt) + log(`@1stFlrSF`), data = my_data)
summary(model4)
# 2.2e-16

anova(model3, model4) # choose model 4

my_data$GarageYrBlt <- as.integer(my_data$GarageYrBlt)
my_data$LotFrontage <- as.integer(my_data$LotFrontage)



#5.0
model5 <- lm(log(SalePrice)~log(TotalBsmtSF) +log(YearRemodAdd)+ log(YearBuilt) + log(`@1stFlrSF`) + log(GarageYrBlt), data = my_data)
summary(model5)
# 2.2e-16

anova(model4, model5) # choose model 5
# 7.276e-06



#6.0
model6 <- lm(log(SalePrice)~log(TotalBsmtSF) +log(YearRemodAdd)+ log(YearBuilt) + log(`@1stFlrSF`) + log(GarageYrBlt) + log(TotRmsAbvGrd), data = my_data)
summary(model6)

anova(model5, model6) # Choose model 6
#  2.2e-16



#7.0
model7 <- lm(log(SalePrice)~log(TotalBsmtSF) +log(YearRemodAdd)+ log(YearBuilt) + log(`@1stFlrSF`) + log(GarageYrBlt) + log(TotRmsAbvGrd) + MSZoning, data = my_data)
summary(model7)
#  2.2e-16

anova(model6, model7) # choose model 7
#   4.053e-11


#8.0
model8 <- lm(log(SalePrice)~log(TotalBsmtSF) +log(YearRemodAdd)+ log(YearBuilt) + log(`@1stFlrSF`) + log(GarageYrBlt) + log(TotRmsAbvGrd) + MSZoning + OverallQual, data = my_data)
summary(model8)
#  2.2e-16

anova(model7, model8) # Choose model 8
#   2.2e-16





#9.0
model9 <- lm(log(SalePrice)~log(YearRemodAdd)+ log(YearBuilt) + log(`@1stFlrSF`) + log(GarageYrBlt) + log(TotRmsAbvGrd) + MSZoning + OverallQual, data = my_data)
summary(model9)
#  2.2e-16

anova(model8, model9)
#   0.001239



#10.0
model10 <- lm(log(SalePrice)~log(YearRemodAdd)+ log(YearBuilt) + log(`@1stFlrSF`) + log(GarageYrBlt) + log(TotRmsAbvGrd) + MSZoning + OverallQual+ OverallCond , data = my_data)
summary(model10)
#  2.2e-16

anova(model8, model10) # Choose model 10
#   6.079e-09


#11.0
model11 <- lm(log(SalePrice)~log(YearRemodAdd)+ log(YearBuilt) + log(`@1stFlrSF`) + log(GarageYrBlt) + log(TotRmsAbvGrd) + MSZoning + OverallQual + ExterQual + OverallCond, data = my_data)
summary(model11)
#  2.2e-16

anova(model10, model11) # Choose model 10
#   2.2e-16


#12.0
model12 <- lm(log(SalePrice)~log(YearRemodAdd)+ log(YearBuilt) + log(`@1stFlrSF`) + log(GarageYrBlt) + log(TotRmsAbvGrd) + MSZoning + OverallQual + OverallCond + BsmtCond, data = my_data)
summary(model12)
#  2.2e-16

anova(model10, model12) # IS BIGGER THAN 0.05 !! so we choose model 11

#Check the final models with extra graphs

#Residuals vs fitted values
plot(model12, 1)
#Q-Q plot
plot(model12, 2)
#Scale Location 
plot(model12, 3)


