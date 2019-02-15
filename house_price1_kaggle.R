library(dplyr)
library(data.table)
sd <- fread("~/data/house_prices/train.csv",stringsAsFactors = TRUE, na.strings = c(" ","NA","","?",".","-"))
sd_test <- fread("~/data/house_prices/test.csv",stringsAsFactors = TRUE, na.strings = c(" ","NA","","?",".","-"))
fin <- fread("~/data/house_prices/sample_submission.csv")

sd_test$SalePrice <- 0
colnames(sd_test)
sd_test[667,60] <- round(mean(sd_test$GarageYrBlt,na.rm = TRUE))
df <- rbind(sd,sd_test)
df_factor <- as.data.frame(df)[,sapply(df,is.factor)]
str(df_factor)
df_factor <- df_factor %>% mutate_if(is.factor,as.numeric)
df_num <- as.data.frame(df)[,sapply(df,is.numeric)]

df_factor$Alley <- ifelse(is.na(df_factor$Alley)==TRUE,3,df_factor$Alley)
df_factor$BldgType <- ifelse(is.na(df_factor$BldgType)==TRUE,5,df_factor$BldgType)
df_factor$BsmtCond <- ifelse(is.na(df_factor$BsmtCond)==TRUE,4,df_factor$BsmtCond)
df_factor$BsmtExposure <- ifelse(is.na(df_factor$BsmtExposure)==TRUE,4,df_factor$BsmtExposure)
df_factor$BsmtFinType1 <- ifelse(is.na(df_factor$BsmtFinType1)==TRUE,3,df_factor$BsmtFinType1)
df_factor$BsmtFinType2 <- ifelse(is.na(df_factor$BsmtFinType2)==TRUE,6,df_factor$BsmtFinType2)
df_factor$BsmtQual <- ifelse(is.na(df_factor$BsmtQual)==TRUE,4,df_factor$BsmtQual)
df_factor$CentralAir <- ifelse(is.na(df_factor$CentralAir)==TRUE,2,df_factor$CentralAir)
df_factor$Condition1 <- ifelse(is.na(df_factor$Condition1)==TRUE,9,df_factor$Condition1)
df_factor$Condition2 <- ifelse(is.na(df_factor$Condition2)==TRUE,8,df_factor$Condition2)
df_factor$Electrical <- ifelse(is.na(df_factor$Electrical)==TRUE,5,df_factor$Electrical)
df_factor$ExterCond <- ifelse(is.na(df_factor$ExterCond)==TRUE,5,df_factor$ExterCond)
df_factor$Exterior1st <- ifelse(is.na(df_factor$Exterior1st)==TRUE,11,df_factor$Exterior1st)
df_factor$Exterior2nd <- ifelse(is.na(df_factor$Exterior2nd)==TRUE,13,df_factor$Exterior2nd)
df_factor$ExterQual <- ifelse(is.na(df_factor$ExterQual)==TRUE,4,df_factor$ExterQual)
df_factor$Fence <- ifelse(is.na(df_factor$Fence)==TRUE,5,df_factor$Fence)
df_factor$FireplaceQu <- ifelse(is.na(df_factor$FireplaceQu)==TRUE,6,df_factor$FireplaceQu)
df_factor$Foundation <- ifelse(is.na(df_factor$Foundation)==TRUE,6,df_factor$Foundation)
df_factor$Functional <- ifelse(is.na(df_factor$Functional)==TRUE,7,df_factor$Functional)
df_factor$GarageCond <- ifelse(is.na(df_factor$GarageCond)==TRUE,5,df_factor$GarageCond)
df_factor$GarageFinish <- ifelse(is.na(df_factor$GarageFinish)==TRUE,3,df_factor$GarageFinish)
df_factor$GarageQual <- ifelse(is.na(df_factor$GarageQual)==TRUE,5,df_factor$GarageQual)
df_factor$GarageType <- ifelse(is.na(df_factor$GarageType)==TRUE,2,df_factor$GarageType)
df_factor$Heating <- ifelse(is.na(df_factor$Heating)==TRUE,6,df_factor$Heating)
df_factor$HeatingQC <- ifelse(is.na(df_factor$HeatingQC)==TRUE,5,df_factor$HeatingQC)
df_factor$HouseStyle <- ifelse(is.na(df_factor$HouseStyle)==TRUE,8,df_factor$HouseStyle)
df_factor$KitchenQual <- ifelse(is.na(df_factor$KitchenQual)==TRUE,4,df_factor$KitchenQual)
df_factor$LandContour <- ifelse(is.na(df_factor$LandContour)==TRUE,4,df_factor$LandContour)
df_factor$LandSlope <- ifelse(is.na(df_factor$LandSlope)==TRUE,3,df_factor$LandSlope)
df_factor$LotConfig <- ifelse(is.na(df_factor$LotConfig)==TRUE,5,df_factor$LotConfig)
df_factor$LotShape <- ifelse(is.na(df_factor$LotShape)==TRUE,4,df_factor$LotShape)
df_factor$MasVnrType <- ifelse(is.na(df_factor$MasVnrType)==TRUE,3,df_factor$MasVnrType)
df_factor$MiscFeature <- ifelse(is.na(df_factor$MiscFeature)==TRUE,5,df_factor$MiscFeature)
df_factor$MSZoning <- ifelse(is.na(df_factor$MSZoning)==TRUE,4,df_factor$MSZoning)
df_factor$Neighborhood <- ifelse(is.na(df_factor$Neighborhood)==TRUE,25,df_factor$Neighborhood)
df_factor$PavedDrive <- ifelse(is.na(df_factor$PavedDrive)==TRUE,3,df_factor$PavedDrive)
df_factor$PoolQC <- ifelse(is.na(df_factor$PoolQC)==TRUE,4,df_factor$PoolQC)
df_factor$RoofMatl <- ifelse(is.na(df_factor$RoofMatl)==TRUE,8,df_factor$RoofMatl)
df_factor$RoofStyle <- ifelse(is.na(df_factor$RoofStyle)==TRUE,6,df_factor$RoofStyle)
df_factor$SaleCondition <- ifelse(is.na(df_factor$SaleCondition)==TRUE,6,df_factor$SaleCondition)
df_factor$SaleType <- ifelse(is.na(df_factor$SaleType)==TRUE,9,df_factor$SaleType)
df_factor$Street <- ifelse(is.na(df_factor$Street)==TRUE,2,df_factor$Street)
df_factor$Utilities <- ifelse(is.na(df_factor$Utilities)==TRUE,1,df_factor$Utilities)

df_num$LotFrontage <- ifelse(is.na(df_num$LotFrontage)==TRUE,0,df_num$LotFrontage)
df_num$MasVnrArea <- ifelse(is.na(df_num$MasVnrArea)==TRUE,0,df_num$MasVnrArea)
df_num$BsmtFinSF1 <- ifelse(is.na(df_num$BsmtFinSF1)==TRUE,0,df_num$BsmtFinSF1)
df_num$BsmtFinSF2 <- ifelse(is.na(df_num$BsmtFinSF2)==TRUE,0,df_num$BsmtFinSF2)
df_num$BsmtUnfSF <- ifelse(is.na(df_num$BsmtUnfSF)==TRUE,0,df_num$BsmtUnfSF)
df_num$TotalBsmtSF <- ifelse(is.na(df_num$TotalBsmtSF)==TRUE,0,df_num$TotalBsmtSF)
df_num$BsmtFullBath <- ifelse(is.na(df_num$BsmtFullBath)==TRUE,0,df_num$BsmtFullBath)
df_num$BsmtHalfBath <- ifelse(is.na(df_num$BsmtHalfBath)==TRUE,0,df_num$BsmtHalfBath)
df_num$GarageYrBlt <- ifelse(is.na(df_num$GarageYrBlt)==TRUE,0,df_num$GarageYrBlt)
df_num$GarageCars <- ifelse(is.na(df_num$GarageCars)==TRUE,0,df_num$GarageCars)
df_num$GarageArea <- ifelse(is.na(df_num$GarageArea)==TRUE,0,df_num$GarageArea)

df_2 <- cbind(df_factor,df_num)
sd_2 <- df_2[1:1460,]
sd_test_2 <- df_2[1461:2919,]
summary(sd_2)

qw <- lm(SalePrice~., data=sd_2)
summary(qw)

fit4 <- lm(SalePrice~RoofMatl
           +ExterQual
           +BsmtQual
           +BsmtExposure
           +KitchenQual
           +Functional
           +PoolQC
           +SaleCondition
           +LotArea
           +OverallQual
           +OverallCond
           +MasVnrArea
           +`1stFlrSF`
           +`2ndFlrSF`
           +GarageYrBlt
           +GarageCars
           +PoolArea, data = sd_2)

fit3 <- lm(SalePrice~Neighborhood
           +Condition2
           +RoofMatl
           +MasVnrType
           +ExterQual
           +BsmtQual
           +BsmtExposure
           +KitchenQual
           +Functional
           +PoolQC
           +SaleCondition
           +LotArea
           +OverallQual
           +OverallCond
           +MasVnrArea
           +`1stFlrSF`
           +`2ndFlrSF`
           +BsmtFullBath
           +KitchenAbvGr
           +TotRmsAbvGrd
           +GarageYrBlt
           +GarageCars
           +ScreenPorch
           +PoolArea, data = sd_2)
summary(fit3)

anova(qw, fit3)
step(qw, direction = "backward")

fit5 <- lm(formula = log(SalePrice) ~ Street + Alley + LandContour + Utilities + 
     LandSlope + Neighborhood + Condition2 + HouseStyle + RoofStyle + 
     RoofMatl + Exterior1st + MasVnrType + ExterQual + BsmtQual + 
     BsmtCond + BsmtExposure + BsmtFinType1 + HeatingQC + KitchenQual + 
     Functional + PoolQC + SaleCondition + MSSubClass + LotFrontage + 
     log(LotArea) + OverallQual + OverallCond + log(YearBuilt) + MasVnrArea + 
     BsmtFinSF1 + `1stFlrSF` + `2ndFlrSF` + BsmtFullBath + BedroomAbvGr + 
     KitchenAbvGr + TotRmsAbvGrd + Fireplaces + GarageYrBlt + 
     GarageCars + WoodDeckSF + ScreenPorch + PoolArea + YrSold, 
   data = sd_2)

summary(fit5)
fit2 <- lm(log(SalePrice)~
             log(GrLivArea)
           +ScreenPorch
           +log(YearBuilt)
           +BsmtFinSF1
           +HouseStyle
           +Street
           +WoodDeckSF
           +KitchenAbvGr
           +BsmtFullBath
           +log(LotArea)
           +BsmtExposure
           +Functional
           +OverallCond
           +GarageCars
           +KitchenQual
           +PoolArea
           +BsmtQual
           +OverallQual
           ,data = sd_2)

summary(fit2)

predi2 <- predict(predi4,newdata = sd_test_2)
fin$SalePrice <- exp(predi2)
write.csv(fin,"~/data/house_prices/final2_4.csv",row.names = FALSE)
