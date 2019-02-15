library(asd)
library(data.table) # быстрая загрузка данных

df <- fread("~/data/Titanic/train.csv", na.strings = c("-", "?", " ", "NA", ""))
test <- fread("~/data/Titanic/test.csv")

df$Age <- ifelse(is.na(df$Age),median(df$Age,na.rm = TRUE),df$Age)

median(df$Age, na.rm = TRUE) #na.rm делает так что бы данные читались без NA
median(test$Age, na.rm = TRUE)

str(df) # structure of data
summary(df) # МЦТ of data
head(df) # First N rows
library(ggplot2) # library
str(df) # structure of data


# делим данные 70 на 30 
library(ROSE)
install.packages("tidyr")
library(caret)
library(rpart)
df$PassengerId <- NULL 
df$Ticket <- NULL
df$Name <- NULL
df$Cabin <- NULL

df <- na.omit(df)

index <- createDataPartition(df$Survived,p=0.7,list=FALSE)
str(index)

df_train <- df[index,]
df_test <- df[-index,]

# data manipulation/ data engineering
head(df)
df_train$Name <- NULL
# строим модель на 70% данных (трайн)
# rpart(DP~ID,data)
colnames(df_train)

#дерево решения
des_tree <- rpart(Survived~.,df_train)

#random_forest
df_train$Sex <- as.factor(df_train$Sex)

des_rand <- randomForest(as.factor(Survived)~Pclass+Sex+SibSp+Parch+Fare, df_train,do.trace=TRUE,ntree=2000)

#regression
des_reg <- lm(Survived~., df_train)

#regression - glm
des_glm <- glm(Survived~.- Fare, data = df_train, family = binomial)

head(df_train)
# вытаскиваем эффективность на 30% данных (тест)
test_res <- predict(des_tree,newdata=df_test)

df_test$Sex <- as.factor(df_test$Sex)
test_ran <- predict(des_rand, newdata=df_test)
test_reg <- predict(des_reg, newdata = df_test)
test_glm <- predict(des_glm, newdata = df_test)
#
roc.curve(df_test$Survived,test_res,plotit = FALSE)
roc.curve(df_test$Survived,test_ran,plotit = FALSE)
roc.curve(df_test$Survived,test_reg,plotit = FALSE)
roc.curve(df_test$Survived,test_glm,plotit = FALSE)

#
test$Sex <- as.factor(test$Sex)
fin <- predict(des_rand,newdata=test)
fin_reg <- predict(des_reg, newdata=test)
fin
subm <- fread("~/data/Titanic/gender_submission.csv")
head(subm)


#
subm$Survived <- fin_reg

subm$Survived <- ifelse(subm$Survived>0.3,1,0)
subm[153,2] <- 0

write.csv(subm,"~/data/Titanic/final_ran1.csv",row.names = FALSE)
