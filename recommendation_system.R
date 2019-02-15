install.packages("recosystem")
library(recosystem)
install.packages("recommenderlab")
library(recommenderlab)
library(devtools)
install_github(repo="SlopeOne", username = "tarashnot")
library(SlopeOne)
install_github(repo="SVDApproximation", username = "tarashnot")
library(SVDApproximation)
data("ratings")
attach(ratings)

library(dplyr)
library(plotly)
sample <- sample_n(ratings, 100000)
head(ratings)
#Нарисуем распределение рэйтингов через Плотли
#plot_ly(x=~ratings, data=sample)
qplot(as.character(sample$rating),fill=as.character(sample$rating))

# Нарисуем распределение среднего по юзерам рэйтинг через Плотли
mean_id <- sample %>% dplyr::group_by(user) %>% summarise(mean=mean(rating))
# Нарисуем распределение среднего по продуктам рэйтинг через Плотли
mean_item <- sample %>% dplyr::group_by(item) %>% summarise(mean=mean(rating))

qplot((mean_id$mean),fill="red",bins=20,xlab = "Mean by user")
qplot((mean_item$mean),bins=20,xlab = "Mean by item")


set.seed(1)
in_train <- rep(TRUE, nrow(ratings))
in_train[sample(1:nrow(ratings), size = round(0.2 * length(unique(ratings$user)), 0) * 5)] <- FALSE

ratings_train <- ratings[(in_train)]
ratings_test <- ratings[(!in_train)]

nrow(ratings_test)/nrow(ratings_train)*100 # Доля теста от трэйна

write.table(ratings_train, file = "trainset.txt", sep = " ", row.names = FALSE, col.names = FALSE)
write.table(ratings_test, file = "testset.txt", sep = " ", row.names = FALSE, col.names = FALSE)

# Код рекомендательной системы
r = Reco()

opts <- r$tune("trainset.txt", opts = list(dim = c(17:20), lrate = c(0.05),
                                           nthread = 4, cost_p1 = c(0), niter = 200, nfold = 10, verbose = TRUE))
r$train("trainset.txt", opts = c(opts$min, nthread = 4, niter = 500, verbose = FALSE))

outfile = tempfile()

r$predict("testset.txt", outfile)

scores_real <- read.table("testset.txt", header = FALSE, sep = " ")$V3
scores_pred <- scan(outfile)

rmse_mf <- sqrt(mean((scores_real-scores_pred) ^ 2))

rmse_mf
