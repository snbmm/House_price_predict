# This R environment comes with many helpful analytics packages installed
# It is defined by the kaggle/rstats Docker image: https://github.com/kaggle/docker-rstats
# For example, here's a helpful package to load

library(tidyverse) # metapackage of all tidyverse packages
library(corrplot)
library(mice)

# Input data files are available in the read-only "../input/" directory
# For example, running this (by clicking run or pressing Shift+Enter) will list all files under the input directory
list.files(path = "../input")

# You can write up to 20GB to the current directory (/kaggle/working/) that gets preserved as output when you create a version using "Save & Run All" 
# You can also write temporary files to /kaggle/temp/, but they won't be saved outside of the current session

# First load the house data 
house.data.train<-read.csv("/Users/minxiangliu/Desktop/MFIT/867/Team Assignment Code/train.csv")
house.data.predict <-  read.csv("/Users/minxiangliu/Desktop/MFIT/867/Team Assignment Code/test.csv")
hist(house.data.train$SalePrice)
hist(log(house.data.train$SalePrice))
#Combine both train and test data to do the data cleaning
# train has sale price and test doesn't
house.data.X <- rbind(subset(house.data.train, select=-c(SalePrice)), house.data.predict)

# md.pattern is hard to read, so we are using colSums to check all NA value
colSums(is.na(house.data.X))

############################
#Data cleaning
############################
a <- table(house.data.X$MSZoning[house.data.X$Neighborhood == 'IDOTRR'])
barplot(a)
# Imputing NA for IDOTRR with RM
house.data.X$MSZoning[house.data.X$Neighborhood == 'IDOTRR' & is.na(house.data.X$MSZoning)] <- 'RM'

b <- table(house.data.X$MSZoning[house.data.X$Neighborhood == 'Mitchel'])
barplot(b)
# Imputing NA for Mitchel with RL
house.data.X$MSZoning[house.data.X$Neighborhood == 'Mitchel' & is.na(house.data.X$MSZoning)] <- 'RL'

# Set LotFrontage to 0 for Inside and Cul-de-sac
house.data.X$LotFrontage[house.data.X$LotConfig == 'CulDSac' & is.na(house.data.X$LotFrontage)] <- 0

# Set LotFrontage to 1, 2, 3 * sqrt(LotArea) for Inside, Corner, FR2 and FR3
house.data.X$LotFrontage[house.data.X$LotConfig == 'Inside' & is.na(house.data.X$LotFrontage)] <- 
  sqrt(house.data.X$LotArea[house.data.X$LotConfig == 'Inside' & is.na(house.data.X$LotFrontage)])
house.data.X$LotFrontage[house.data.X$LotConfig == 'Corner' & is.na(house.data.X$LotFrontage)] <- 
  2 * sqrt(house.data.X$LotArea[house.data.X$LotConfig == 'Corner' & is.na(house.data.X$LotFrontage)])
house.data.X$LotFrontage[house.data.X$LotConfig == 'FR2' & is.na(house.data.X$LotFrontage)] <- 
  2 * sqrt(house.data.X$LotArea[house.data.X$LotConfig == 'FR2' & is.na(house.data.X$LotFrontage)])
house.data.X$LotFrontage[house.data.X$LotConfig == 'FR3' & is.na(house.data.X$LotFrontage)] <- 
  3 * sqrt(house.data.X$LotArea[house.data.X$LotConfig == 'FR3' & is.na(house.data.X$LotFrontage)])

# We dont have enough data to impute Alley, will replace NA with factor 'NA'
house.data.X$Alley = factor(house.data.X$Alley, exclude = NULL)

# We have only 2 missing Utilities and 1 NoSeWa, imputing with AllPub
house.data.X$Utilities[is.na(house.data.X$Utilities)] <- 'AllPub'

#We are missing one Exterior1st and one Exterior2nd, imputing with factor NA
house.data.X$Exterior2nd = factor(house.data.X$Exterior2nd, exclude = NULL)
house.data.X$Exterior1st = factor(house.data.X$Exterior1st, exclude = NULL)

#We are imputing MasVnrType and MasVnrArea with None and o
house.data.X$MasVnrType[is.na(house.data.X$MasVnrType)] <- 'None'
house.data.X$MasVnrArea[is.na(house.data.X$MasVnrArea)] <- 0

#We will set all no-basement related variable to NA or 0:
house.data.X$TotalBsmtSF[is.na(house.data.X$TotalBsmtSF)] <- 0
house.data.X$BsmtQual[house.data.X$TotalBsmtSF == 0] <- NA
house.data.X$BsmtQual = factor(house.data.X$BsmtQual, exclude = NULL)
house.data.X$BsmtCond[house.data.X$TotalBsmtSF == 0] <- NA
house.data.X$BsmtCond = factor(house.data.X$BsmtCond, exclude = NULL)
house.data.X$BsmtExposure[house.data.X$TotalBsmtSF == 0] <- NA
house.data.X$BsmtExposure = factor(house.data.X$BsmtExposure, exclude = NULL)
house.data.X$BsmtFinType1[house.data.X$TotalBsmtSF == 0] <- NA
house.data.X$BsmtFinType1 = factor(house.data.X$BsmtFinType1, exclude = NULL)
house.data.X$BsmtFinType2[house.data.X$TotalBsmtSF == 0] <- NA
house.data.X$BsmtFinType2 = factor(house.data.X$BsmtFinType2, exclude = NULL)
house.data.X$BsmtFinSF1[house.data.X$TotalBsmtSF == 0] <- 0
house.data.X$BsmtFinSF2[house.data.X$TotalBsmtSF == 0] <- 0
house.data.X$BsmtUnfSF[house.data.X$TotalBsmtSF == 0] <- 0
house.data.X$BsmtFullBath[house.data.X$TotalBsmtSF == 0] <- 0
house.data.X$BsmtHalfBath[house.data.X$TotalBsmtSF == 0] <- 0

# Electrical will follow the majority
house.data.X$Electrical[is.na(house.data.X$Electrical)] <- 'SBrkr'

# Set KitchenQual to TA
house.data.X$KitchenQual[is.na(house.data.X$KitchenQual)] <- 'TA'

# Set Functional to Typ
house.data.X$Functional[is.na(house.data.X$Functional)] <- 'Typ'

# Set no Fireplace house FireplaceQu to NA
house.data.X$FireplaceQu[house.data.X$Fireplaces == 0] <- 'NA'

# Same to basement, we decide garage based on area
house.data.X$GarageFinish[house.data.X$GarageArea == 0] <- 'NA'
house.data.X$GarageType[house.data.X$GarageArea == 0] <- 'NA'
house.data.X$GarageQual[house.data.X$GarageArea == 0] <- 'NA'
house.data.X$GarageCond[house.data.X$GarageArea == 0] <- 'NA'
house.data.X$GarageYrBlt[house.data.X$GarageArea == 0] <- 0
house.data.X$GarageCars[house.data.X$GarageArea == 0] <- 0

a <- table(house.data.X$GarageCond)
barplot(a)
mean(replace(house.data.X$GarageArea, house.data.X$GarageArea == 0, NA), na.rm = TRUE)

house.data.X$GarageYrBlt[is.na(house.data.X$GarageYrBlt)] <-house.data.X$YearBuilt[is.na(house.data.X$GarageYrBlt)]
house.data.X$GarageYrBlt[house.data.X$GarageYrBlt ==2207] <-2007
house.data.X$GarageFinish[is.na(house.data.X$GarageFinish)] <- 'Unf'
house.data.X$GarageCars[is.na(house.data.X$GarageCars)] <- 2
house.data.X$GarageArea[is.na(house.data.X$GarageArea)] <- 500
house.data.X$GarageQual[is.na(house.data.X$GarageQual)] <- 'TA'
house.data.X$GarageCond[is.na(house.data.X$GarageCond)] <- 'TA'

# Set pool QC to be NA if poolarea is 0
barplot(table(house.data.X$PoolQC[house.data.X$PoolQC != 'NA']))
house.data.X$PoolQC[house.data.X$PoolArea == 0] <- 'NA'
house.data.X$PoolQC[is.na(house.data.X$PoolQC)] <- 'GD'


house.data.X$Fence[is.na(house.data.X$Fence)] <- 'NA'
house.data.X$MiscFeature[is.na(house.data.X$MiscFeature)] <- 'NA'
barplot(table(house.data.X$SaleType[house.data.X$SaleType != 'NA']))
house.data.X$SaleType[is.na(house.data.X$SaleType)] <- 'WD'

#Check if the data is completed
colSums(is.na(house.data.X))

#######################
# Feature engineering
#######################
house.data.X$MoSold = factor(house.data.X$MoSold, exclude = NULL)
house.data.X$MSSubClass <- as.factor(house.data.X$MSSubClass)

house.data.X$TotalArea = house.data.X$GrLivArea + house.data.X$TotalBsmtSF
house.data.X$AdjustedAge = (house.data.X$YearBuilt + house.data.X$YearRemodAdd)/2
house.data.X$age_sold = house.data.X$YrSold - house.data.X$YearBuilt
house.data.X$age_sold[house.data.X$age_sold<0] <- 0
house.data.X$YrSold = factor(house.data.X$YrSold, exclude = NULL)

house.data.X$PorchArea = house.data.X$OpenPorchSF + house.data.X$EnclosedPorch 
                        + house.data.X$X3SsnPorch + house.data.X$ScreenPorch
house.data.X$above_ratio = house.data.X$GrLivArea/house.data.X$TotalArea
house.data.X$patio_spaceness = house.data.X$LotArea/house.data.X$TotRmsAbvGrd
house.data.X$indoor_spaceness = house.data.X$TotalArea/house.data.X$TotRmsAbvGrd
house.data.X$average_room_size = house.data.X$GrLivArea/house.data.X$TotRmsAbvGrd

library(SetMethods)
num_cols <- unlist(lapply(house.data.X, is.numeric)) 

library(moments)
data.numeric <- house.data.X[, num_cols]
data.numeric[data.numeric == 0] <- NA
skewness(data.numeric, na.rm = TRUE)
skewness(log1p(data.numeric), na.rm = TRUE)
# We are seeing improvement on LotFrontage, LotArea, MasVnrArea, BsmtFinSF1, BsmtFinSF2. BsmtUnfSF, TotalBsmtSF, X1stFlrSF, LowQualFinSF, average_room_size
# GrLivArea, FullBath, TotRmsAbvGrd, GarageArea, WoodDeckSF, OpenPorchSF, EnclosedPorch, ScreenPorch, MiscVal, TotalArea, PorchArea, patio_spaceness, indoor_spaceness
cols_skewed <- c("LotFrontage", "LotArea", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "X1stFlrSF", "LowQualFinSF",
                "GrLivArea", "FullBath", "TotRmsAbvGrd", "GarageArea", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch", "ScreenPorch", "MiscVal", 
                "TotalArea", "PorchArea", "patio_spaceness", "indoor_spaceness", "average_room_size")
house.data.X[, cols_skewed] <- log1p(house.data.X[, cols_skewed])

colSums(is.na(house.data.X))

# Split the data into Training/testing sets
library (caTools)
library(glmnet)
set.seed(7)
inx <- sample.split(seq_len(nrow(house.data.train)), 0.7)

#create the y variable and matrix (capital X) of x variables (will make the code below easier to read + will ensure that all interactoins exist)
y.training<-log(house.data.train[inx,]$SalePrice)

# X<-model.matrix(~ . -Id, house.data.X)[, -1]
#                 # + GarageArea*GarageCars + LotArea*LotConfig + OverallCond*OverallQual +MasVnrArea*MasVnrType
#                 # + Heating*HeatingQC + Fireplaces/TotalArea + ExterQual*Exterior1st
#                 # + MSZoning*LotArea + YrSold*MoSold
                
X<-model.matrix(~. 
                -Id -LotFrontage-Alley-HouseStyle-YearRemodAdd- MasVnrType-BsmtFinSF2 -X2ndFlrSF-BsmtHalfBath 
                -TotRmsAbvGrd -TotalBsmtSF-FireplaceQu-GarageYrBlt-GarageArea-GarageFinish-OpenPorchSF-EnclosedPorch
                -X3SsnPorch -MiscVal -MiscFeature-PorchArea -above_ratio -patio_spaceness- indoor_spaceness
                -average_room_size -Street -SaleTypeCon -Electrical
                +YrSold*MoSold+ log(OverallCond*OverallQual)*TotalArea +LotArea*Neighborhood*MSZoning
                +LotConfig*LotArea +Fence*Neighborhood, house.data.X)[, -1]

X<-cbind(house.data.X$ID,X)
md.pattern(house.data.X)

# split X into trainig and testing as before
X.train <- X[1:1460, ]
X.training<-X.train[inx,]
X.testing<-X.train[!inx,]

lasso.fit<-glmnet(x = X.training, y = y.training, alpha = 1)
ridge.fit<-glmnet(x = X.training, y = y.training, alpha = 0)
plot(lasso.fit, xvar = "lambda")
plot(ridge.fit, xvar = "lambda")

crossval <-  cv.glmnet(x = X.train, y = log(house.data.train$SalePrice), alpha = 1) #create cross-validation data. By default, the function performs ten-fold cross-validation, though this can be changed using the argument nfolds. 
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
crossval2 <-  cv.glmnet(x = X.train, y = log(house.data.train$SalePrice), alpha = 0) #create cross-validation data. By default, the function performs ten-fold cross-validation, though this can be changed using the argument nfolds. 
plot(crossval2)
penalty.ridge <- crossval2$lambda.min #determine optimal penalty parameter, lambda
log(penalty.ridge) #see where it was on the graph

plot(crossval,xlim=c(log(penalty.lasso)-3,log(penalty.lasso)+3)) # lets zoom-in
lasso.opt.fit <-glmnet(x = X.training, y = y.training, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients
plot(crossval2,xlim=c(log(penalty.ridge)-3,log(penalty.ridge)+3)) # lets zoom-in
ridge.opt.fit <-glmnet(x = X.training, y = y.training, alpha = 0, lambda = penalty.ridge) #estimate the model with the optimal penalty
#coef(ridge.opt.fit) #resultant model coefficients

# predicting the performance on the testing set
lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.testing))
ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.testing))

lasso.testing.MSE <- mean((lasso.testing- house.data.train[!inx,]$SalePrice )^2) #calculate and display MSE in the testing set
lasso.testing.MAPE <- mean(abs(lasso.testing-house.data.train[!inx,]$SalePrice)/house.data.train[!inx,]$SalePrice*100) # MAPE: mean absolute percentage error 
ridge.testing.MSE <- mean((ridge.testing- house.data.train[!inx,]$SalePrice )^2) #calculate and display MSE in the testing set
ridge.testing.MAPE <- mean(abs(ridge.testing-house.data.train[!inx,]$SalePrice)/house.data.train[!inx,]$SalePrice*100) # MAPE: mean absolute percentage error 


result <- data.frame(Id = house.data.predict$Id)
X.result<- X[1461:2919,]
identical(rownames(X.testing), rownames(X.result))
result$SalePrice <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.result))

write.csv(result, file = "./result.csv", row.names = FALSE) # export the predicted prices into a CSV file
house.data.train<-read.csv("./result.csv")