install.packages("asd") 
library(asd)
install.packages("data.table")
library(data.table) # быстрая загрузка данных

df <- fread("~/data/Titanic/train.csv")
test <- fread("~/data/Titanic/test.csv")

str(df) # structure of data
summary(df) # МЦТ of data
head(df) # First N rows
library(ggplot2) # library
str(df) # structure of data

# делим данные 70 на 30 
install.packages("ROSE")
library(ROSE)
library(caret)
library(rpart)
df$PassengerId <- NULL

index <- createDataPartition(df$Survived,p=0.7,list=FALSE)
str(index)
index

df_train <- df[index,]
df_test <- df[-index,]
# data manipulation/ data engineering
head(df)
df_train$Name <- NULL
# строим модель на 70% данных (трайн)
# rpart(DP~ID,data)
colnames(df)
df_train$Ticket <- NULL

library(randomForest)
des_rand <- randomForest(Survived~.,df_train, ntree=50)
des_lm <- lm(Survived~.,df_train)

des_tree <- rpart(Survived~.,df_train) # дерево решения 
df_train$Cabin <- NULL
head(df_train)
# вытаскиваем эффективность на 30% данных (тест)
test_res <- predict(des_lm,newdata=df_test)
#
roc.curve(df_test$Survived,test_res,plotit = FALSE)
#
fin <- predict(des_lm,newdata=test)
fin
subm <- fread("~/data/Titanic/gender_submission.csv")
head(subm)

View(subm)
fin <- ifelse(fin>=0.5,1,0)
subm$Survival <- fin
