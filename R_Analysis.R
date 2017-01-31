############################ Modelling House Prices ############################
#### Kaggle data set: https://www.kaggle.com/c/house-prices-advanced-regression-techniques
setwd("/media/rakesh/G/DataScience/PracticeDatasets/HousePrices")
dir()

library(caret)
library(dplyr)
library(ggplot2)

house.train.alldata <- read.csv("train.csv", header = TRUE)
head(house.train.alldata)
tail(house.train.alldata)
dim(house.train.alldata) ## A 1460x81 data frame.
View(house.train.alldata)

house.test <- read.csv("test.csv", header = TRUE)
head(house.test)
tail(house.test)
dim(house.test) ## A 1460x80 data frame.

str(house.train.alldata)
house.train.alldata$MSSubClass <- as.factor(house.train.alldata$MSSubClass)
house.train.alldata$OverallQual <- as.factor(house.train.alldata$OverallQual)
house.train.alldata$OverallCond <- as.factor(house.train.alldata$OverallCond)

## Replicate on test set:
house.test$MSSubClass <- as.factor(house.test$MSSubClass)
house.test$OverallQual <- as.factor(house.test$OverallQual)
house.test$OverallCond <- as.factor(house.test$OverallCond)

## Column that cause error in simple lm model: 7, 10, 73, 74 & 75
names(house.train.alldata[, c(7, 10, 73, 74, 75)]) 
## Alley, Utilities, PoolQC, Fence, MiscFeature
str(house.train.alldata[, c(7, 10, 73, 74, 75)])
sapply(house.train.alldata[, c(7, 10, 73, 74, 75)], function(x)(sum(is.na(x))))
## Alley, PoolQC, Fence & MiscFeature variables have > 1170 NA values out of 
## 1460 observations. 
sapply(house.train.alldata[, c(7, 10, 73, 74, 75)], FUN = table)
## Utilities have 1459 obs with "AllPub" and just 1 obs with "NoSeWa". 
## Extreme case of near zero variance predictor. 

## Same issue exists with test data set.
sapply(house.test[, c(7, 10, 73, 74, 75)], function(x)(sum(is.na(x))))
sapply(house.test[, c(7, 10, 73, 74, 75)], FUN = table)

##Removing these variables from testset and training set:
house.train.alldata$Alley <- NULL
house.train.alldata$Utilities <- NULL
house.train.alldata$PoolQC <- NULL
house.train.alldata$Fence <- NULL
house.train.alldata$MiscFeature <- NULL

house.test$Alley <- NULL
house.test$Utilities <- NULL
house.test$PoolQC <- NULL
house.test$Fence <- NULL
house.test$MiscFeature <- NULL

lm.initmodel <- lm(SalePrice~., data = house.train.alldata[, -1])
summary(lm.initmodel) ##Adjusted Rsquared = 0.8895

table(sapply(house.train.alldata, class)) ## 41 factor and 35 integer variables
int.variables <- which("integer" == sapply(house.train.alldata, class))
## int.variables is a named integer vector.

str(house.train.alldata[, int.variables]) ##Includes Id and SalePrice.
length(int.variables) 
## 33 integer variables in the dataframe(excluding id & SalePrice).
## Removing id and SalesPrice from int.variables:
int.variables <- int.variables[c(-1, -35)]
head(int.variables)
tail(int.variables)

names(int.variables)

#### Separate discrete variables from int.variables ####

## YearBuilt, YearRemodAdd, and GarageYrBlt though discrete are as good as 
## continuous.
## Discrete Variables: YrSold, BsmtFullBath, BsmtHalfBath, FullBath, HalfBath,
## BedroomAbvGr, KitchenAbvGr, TotRmsAbvGrd, Fireplaces, GarageCars, MoSold,
## YrSold

discrete.variables <- int.variables[c("BsmtFullBath", "BsmtHalfBath", 
                                      "FullBath", "HalfBath", "BedroomAbvGr",
                                      "KitchenAbvGr", "TotRmsAbvGrd",
                                      "Fireplaces", "GarageCars", "MoSold",
                                      "YrSold")]

int.variables <- int.variables[!(names(int.variables) %in% names(discrete.variables))]
length(discrete.variables) ##11 discrete variables and 22 continuous variables
length(int.variables)

################################################################################

#### Analyze frequency distribution of continuous and discrete predictors ####
## Continuous Variables:
par(mfrow = c(2, 2))
for(i in 1:length(int.variables)) {
    lim <- quantile(house.train.alldata[, int.variables[i]], na.rm = TRUE)
    br <- max(min(10*(lim[5] - lim[4])/(lim[4] - lim[3]), 50), 20)
    hist(house.train.alldata[, int.variables[i]],
         breaks = br,
         ylab = "Frequency",
         xlab = "",
         main = names(int.variables[i]))
    par(new = T)
    plot(density(house.train.alldata[, int.variables[i]], na.rm = TRUE), 
         col = "blue", axes = FALSE, xlab = "", ylab = "", main = "")
}
rm(lim, i, br) ##Remove the temporary variables.
## LotArea has a highly rightskewed distribution
## MasVnrArea, BsmtFinSF1, BsmtFinSF2, X2ndFlrSF, LowQualFinSF, WoodDeckSF, 
## OpenPorchSF, EnclosedPorch, X3SsnPorch, ScreenPorch, PoolArea, MiscVal have 
## high rightskewed distribution with a steep peek at 0 and small amount of 
## non-Zero observations.

## Distribution of Discrete variables:
par(mfrow = c(2, 2))
for(i in 1:length(discrete.variables)) {
    barplot(table(house.train.alldata[, discrete.variables[i]]), 
         main = names(discrete.variables[i]))
}

## BsmtFullBath has only 4 values, the 3rd and 4th values are of very low 
## frequency. Create a factor variable: BsmtFullBathPresent = 1 iff 
## BsmtFullBath > 0 and 0 other wise.

## BsmtHalfBath has 0 for nearly 1300 observations (Near zero variance). Can 
## cause problems in model building and cross validation. This predictor can be 
## removed.

## FullBath and HalfBath have just 2 values for most of the observations. Create
## a factor variable FullBath2 = 0 iff FullBath <= 1 & FullBath2 = 1 iff 
## FullBath >=1. ///ly HalfBath1 = 0 iff HalfBath = 0 & HalfBath1 = 1 iff 
## HalfBath >= 1. 


## For BedroomAbvGr convert to factor with levels: <=1, 2, 3, >=4
## KithchenAbvGr should be ignored as nearzero variance predictor
## Convert TotalRmsAbvGrd to Factor with levels <=4, 5, 6, 7, 8, >=9
## Convert Fireplaces should be converted to factor with levels 0, 1, >=2
## GarageCars: Convert to factor - levels: 0, 1, 2, >=3

#### Factor Variables ####
factor.variables <- which("factor" == sapply(house.train.alldata, class))
head(factor.variables)
tail(factor.variables)
length(factor.variables)

par(mfrow = c(2, 2))
for(i in 1:length(factor.variables)) {
    plot(house.train.alldata[, factor.variables[i]], 
         main = names(factor.variables[i]))
}

table(house.train.alldata$MSSubClass)
## From MSSubClass, create 1 variables MSSubClassStyle, use HouseStyle for story
## MSSubClassStyle levels: (Newer, OLDER, Other & All Ages)
table(house.train.alldata$HouseStyle)
## Re-code HouseStyle: Levels: 1Story & 1.5Unf, 1.5Fin, 2 to 2.5, Others

table(house.train.alldata$LotShape)
## Re-code LotShape with two levels Reg and IR

table(house.train.alldata$LotConfig)
## Re-code LotConfig with three levels Inside, CulDSac & Others

table(house.train.alldata$Neighborhood)
## Neighborhood has only two values with frequency > 150, including it in model 
## Will cause issue during cross validation. Hence ignore this value.

table(house.train.alldata$OverallQual)
## Recode OverallQual as OverallQual <=4, 5, 6, 7, >=8

table(house.train.alldata$OverallCond)
## Recode OverallCond as OverallCond <=4, 5, 6, 7, >=8

table(house.train.alldata$Exterior1st)
## Recode Exterior1st with levels HdBoard, MetalSd, Plywood, VinylSd, Wd Sdng and Others

table(house.train.alldata$Exterior2nd)
## Recode Exterior2nd levels HdBoard, MetalSd, Plywood, VinylSd, Wd Sdng and Others

table(house.train.alldata$Foundation)
## Recode Foundation levels: CBlock, PConc, Others

table(house.train.alldata$BsmtQual)
## Recode BsmtQualMod levels FA_TA, Gd, Ex

table(house.train.alldata$BsmtExposure) ## Use as-is

table(house.train.alldata$BsmtFinType1)
## Use As is. 37 NA values, 74 next lowest frequency.

table(house.train.alldata$HeatingQC)
## Recode HeatingQC with 3 levels: Ex, Gd, TA_Fa_Po

table(house.train.alldata$KitchenQual)
## Recode KitchenQual with 3 levels: Ex, Gd, TA_Fa

table(house.train.alldata$FireplaceQu)
## Recode FireplaceQu with 2 levels Ex_Gd and TA_Fa_Po

table(house.train.alldata$GarageType)
## Recode GarageType with 3 levels Attchd, Detchd, Others

table(house.train.alldata$GarageFinish)
## Use as is

table(house.train.alldata$SaleCondition)
## Recode SaleCondition with 3 levels Normal, Partial and Others

## Delete factor variables:  Street, MSZoning, LandContour, Neighborhood, 
## Condition1, Condition2, BldgType, RoofStyle, RoofMatl, MasVnrType, ExterQual,
## ExterCond, BsmtCond, BsmtFinType2, Heating, CentralAir, Electrical, 
## Functional, GarageQual, GarageCond, PavedDrive, 
## SaleType 


#### Remove unwanted variables: ####
RmContinuous <- c("MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "X2ndFlrSF", 
                  "LowQualFinSF", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch",
                  "X3SsnPorch", "ScreenPorch", "PoolArea", "MiscVal")

RmDiscrete <- c("BsmtHalfBath", "KitchenAbvGr")

RmFactor <- c("Street", "MSZoning", "LandContour", "LandSlope","Neighborhood", "Condition1",
              "Condition2", "BldgType", "RoofStyle", "RoofMatl", "MasVnrType", 
              "ExterQual", "ExterCond", "BsmtCond", "BsmtFinType2", "Heating", 
              "CentralAir", "Electrical", "Functional", "GarageQual", 
              "GarageCond", "PavedDrive", "SaleType")

## Check if any typo in variable names (All TRUE)
table(c(RmContinuous, RmDiscrete, RmFactor) %in% names(house.train.alldata))

## Create new dataframes without the variables to be removed:
SelectedVarTrain <- house.train.alldata[, !names(house.train.alldata) %in% c(RmContinuous, RmDiscrete, RmFactor)]
SelectedVarTest <- house.test[, !names(house.test) %in% c(RmContinuous, RmDiscrete, RmFactor)]

#### Predictor Reengineering ####
source("FeatureEngineer.R")
SelectedVarTrain <- featureEngineer(SelectedVarTrain)
SelectedVarTest <- featureEngineer(SelectedVarTest)

## Reorder SelectedVarTrain to bring SalePrice to last column:
SalePrice <- SelectedVarTrain$SalePrice
SelectedVarTrain$SalePrice <- NULL
SelectedVarTrain$SalePrice <- SalePrice
rm(SalePrice)

## Explore the response variable:
library(gridExtra) ## For grid.arrange() function to combine 2 plots.
p0 <- ggplot(data = house.train.alldata, aes(SalePrice))
p1 <- p0 + geom_density(kernel = "gaussian", fill = "#ff4d4d", alpha = 0.5)
p2 <- p0 + geom_histogram(alpha = 0.7, fill = "#003333")
grid.arrange(p1, p2, ncol = 2)
head(sort(house.train.alldata$SalePrice, decreasing = TRUE), n = 20)
rm(p0, p1, p2)
## It appears that there are some outliers with sale price > $600,000
## Let us create a scatterplot of SalePrice vs GrLivArea

p1 <- ggplot(SelectedVarTrain, aes(x = GrLivArea, y = SalePrice))
p1 + geom_point(alpha = 0.7, colour = "#003333") + 
    geom_text(aes(label=ifelse((GrLivArea>3000|SalePrice>4e+5), Id,'')),
              hjust=0,vjust=0) + geom_smooth() +
    labs(x = "Above Ground Living Area(in sq ft)", y = "Sale Price(in $)",
         title = "Sale Price vs Above Ground Living Area")

## Clearly sample number 524 & 1299 are outliers. Also, sample 692 & 1183, 
## though appears to follow the trend, are far apart from the rest of the data 
## points We remove these samples and create the plot again:

p2 <- ggplot(SelectedVarTrain[-c(524, 692, 1183, 1299),], 
             aes(x = GrLivArea, y = SalePrice))
p2 + geom_point(alpha = 0.7, colour = "#003333") + 
    geom_text(aes(label=ifelse((GrLivArea>3000|SalePrice>4e+5), Id,'')),
              hjust=0,vjust=0) + geom_smooth() +
    labs(x = "Above Ground Living Area(in sq ft)", y = "Sale Price(in $)",
         title = "Sale Price vs Above Ground Living Area w/o Outliers")

rm(p1, p2, i)
## Clearly sample number 524, 692, 1183 & 1299 are outliers. We remove these 
## samples from the dataset.
SelectedVarTrain <- SelectedVarTrain[-c(524, 692, 1183, 1299), ]


#### Check for Near-Zero Vairance predictors ####
## nearZeroVar() function with saveMatrics = FALSE returns the number of near-Zero Variance predictors.
nearZeroVar(SelectedVarTrain, saveMetrics = FALSE)
nearZeroVar(SelectedVarTest, saveMetrics = FALSE)
## No Near-zero variance predictors


##### Identifying Correlated predictors #####
## First we detach the Id and SalePrice columns from the training dataset. Next 
## we construct a model.matrix. The dataframe output of the model.matrix is 
## used to remove predictors with pair-wise correlation > 0.9. We also remove 
## the intercept term that the model.matrix() function adds.

Train.Id <- SelectedVarTrain$Id
Train.SalePrice <- SelectedVarTrain$SalePrice
Test.Id <- SelectedVarTest$Id
SelectedVarTrain$Id <- NULL
SelectedVarTrain$SalePrice <- NULL
SelectedVarTest$Id <- NULL

## Create a formula object for use in model.matrix
var.name <- names(SelectedVarTrain)
formula.obj = paste("~", var.name[1])
for(i in 2:length(var.name)){
    formula.obj <- paste(formula.obj, "+", var.name[i])
}

## Use of model.matrix function causes samples with NA values to be omitted.
## To prevent this we set the global parameter 'na.action' to "na.pass". We reset 
## the parameter back to default value("na.omit") after creating the model matrix.
## The training.data and the test.data created below will be used for model building
## while SelectedVarTrain and SelectedVarTest can be used for easy interpretation
## of the results with Id serving as the link.

default.na.action <- options('na.action')
options(na.action='na.pass')
training.data <- model.matrix(as.formula(formula.obj), SelectedVarTrain)
View(training.data)
dim(training.data) ## 1458 x 82
test.data <- model.matrix(as.formula(formula.obj), SelectedVarTest)
View(test.data)
dim(test.data) ## 1459 x 82
options(na.action = as.character(default.na.action))
options('na.action')

correlations =cor(training.data[, -1], use = "pairwise.complete.obs")
tooHigh <- findCorrelation(correlations, .9)
tooHigh
dimnames(training.data)[[2]][tooHigh]

## Remove highly correlated predictors and the constant(intercept) term added by 
## model.matrix() function:
training.data <- training.data[, -c(1, tooHigh)]
test.data <- test.data[, -c(1, tooHigh)]
View(training.data)

#### Split training.data into training.set(80%)) and validation.set(20%) ####

set.seed(100)
train.index <- createDataPartition(y = Train.SalePrice,
                                   times = 1,
                                   p = 0.8,
                                   list = FALSE,
                                   groups = 4)

head(train.index)
## Convert the train.index matrix to an integer vector.
train.index <- as.integer(train.index)

## Create training.set and validation.set, Also split the Train.Id and 
## Train.SalePrice accordingly.
training.set <- training.data[train.index, ]
validation.set <- training.data[-train.index, ]
training.saleprice <- Train.SalePrice[train.index]
validation.saleprice <- Train.SalePrice[-train.index]
training.id <- Train.Id[train.index]
validation.id <- Train.Id[-train.index]
rm(training.data, Train.SalePrice, Train.Id)

#### Random Forest ####
## install.packages("RANN")
library(RANN)
library(randomForest)

mtryGrid <- data.frame(mtry = floor(seq(10, ncol(training.set), length = 10)))
ctrl = trainControl(method = "cv")
set.seed(100) 
rf.model <- train(x = training.set, y = training.saleprice, 
                  method = "rf",
                  tuneGrid = mtryGrid, 
                  preProcess = "knnImpute",
                  ntree = 1000,
                  trControl = ctrl)

plot(rf.model) ##See a plot of RMSE vs mtry

?predict.train
rf.pred <-  predict(rf.model,
                    newdata = validation.set)

R2(rf.pred, validation.saleprice) ## R2: 88.098%
RMSE(rf.pred, validation.saleprice) ## 24460.54
RMSE(log(rf.pred), log(validation.saleprice)) ## 0.1387783 - Used to score 
## submissions at Kaggle. On submission of the test set predictions, the score 
## obtained is 0.16401

#### Test set prediction ####
rf.test.pred = predict(rf.model, newdata = test.data)
rf.solution <- data.frame(Id = Test.Id, SalePrice = rf.test.pred)
write.csv(rf.solution, file = "random_forest_r_submission.csv", 
          row.names=FALSE)


################################################################################

#### Boosting Model ####
library(gbm)
gbmGrid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                       n.trees = seq(100, 1000, by = 50),
                       shrinkage = c(0.01, 0.1),
                       n.minobsinnode = 20)

set.seed(100)
gbm.fit <- train(x = training.set, y = training.saleprice,
                 method = "gbm",
                 tuneGrid = gbmGrid,
                 trControl = ctrl,
                 verbose = FALSE)

gbm.fit
plot(gbm.fit, auto.key = list(columns = 4, lines = TRUE))
gbm.fit$finalModel

gbm.pred <-  predict(gbm.fit, newdata = validation.set)
R2(gbm.pred, validation.saleprice) ## R2: 88.72%
RMSE(gbm.pred, validation.saleprice) ## 24087.75
RMSE(log(gbm.pred), log(validation.saleprice)) ## 0.1320108 - Used to score 
## submissions at Kaggle. On submission of the test set predictions, the score 
## obtained is 0.15875

#### Test set prediction ####
gbm.test.pred = predict(gbm.fit, newdata = test.data)
gbm.solution <- data.frame(Id = Test.Id, SalePrice = gbm.test.pred)
write.csv(gbm.solution, file = "gbm_r_submission.csv", 
          row.names=FALSE) 


################################################################################

#### SVM Model ####
library(kernlab)

svm.fit =train(x = training.set, y = training.saleprice,
               method = "svmRadial",
               preProc = c("center", "scale", "knnImpute"),
               tuneLength = 14,
               trControl = ctrl)

##The tuneLength argument will use the default grid of 14 cost values between 
## 2^(-2), 2^(-1), ..., 2^(11). Again sigma is estimated analytically by default.

svm.fit
svm.fit$finalModel

svm.pred <- predict(svm.fit, newdata = validation.set)
R2(svm.pred, validation.saleprice) ## R2: 90.99%
RMSE(svm.pred, validation.saleprice) ## 21214.57
RMSE(log(svm.pred), log(validation.saleprice)) ## 0.1237407 - Used to score 
## submissions at Kaggle. On submission of the test set predictions, the score 
## obtained is 0.15187

svm.test.pred = predict(svm.fit, newdata = test.data)
svm.solution <- data.frame(Id = Test.Id, SalePrice = svm.test.pred)
write.csv(svm.solution, file = "svm_r_submission.csv", 
          row.names=FALSE) 


################################################################################

