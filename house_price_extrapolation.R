library(data.table)

train <- fread("~/data/house_prices/train.csv")
test <- fread("~/data/house_prices/test.csv")
sub <- fread("~/data/house_prices/sample_submission.csv")

train <- data.frame(train)

str(train)

is.na(train)
apply(train, 2, function(x){sum(is.na(x))})

sum(is.na(train))

apply(train, 2, function(x){sum(is.na(x)/nrow(train))})

columns <- sapply(train, function(x){sum(is.na(x))/nrow(train)})<=0.5

train_tiny <- train[,columns]

train_tiny1 <- data.matrix(train_tiny, rownames.force = NA)

sum(is.na(train_tiny))

apply(train_tiny, 2, function(x){sum(is.na(x))})

library(randomForest)
library(missForest) #заменяет пропущенные значения прогнозными значениями
m <- missForest(train_tiny1) #Когда делаем missforest надо перевести в data.matrix
train_tiny <- m$ximp

train_tiny1 <- data.frame(m$ximp)

?merge

sum(is.na(train_tiny1))

missForest(train)

model <- lm(SalePrice~., train_tiny1)
summary(model)

model1 <- lm(SalePrice~MSSubClass+LotArea+OverallQual+OverallCond+YearBuilt+
               YearRemodAdd+MasVnrArea+BsmtFinSF1+BsmtUnfSF+X1stFlrSF+
               X2ndFlrSF+BsmtFullBath+BedroomAbvGr+KitchenAbvGr+TotRmsAbvGrd+
               Fireplaces+GarageCars+WoodDeckSF+WoodDeckSF+ScreenPorch, train_tiny1)
summary(model1)

library(rpart)
model2 <- rpart(SalePrice~., train_tiny1)
summary(model3)

model3 <- randomForest(SalePrice~.,train_tiny1)
library(ROSE)
roc.curve(test$SalePrice, predict(model3, test))
