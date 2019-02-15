install.packages("neuralnet")
library(neuralnet)
# Read the daa
data = read.csv("~/data/neuralnet/cereals.csv", header=T)

View(data)
# Обязательное условие нормализовать данные.


# Random sampling
samplesize = 0.60 * nrow(data)
set.seed(80)
index = sample(seq_len(nrow(data)),size=samplesize )


datatrain = data[ index, ]
datatest = data[ -index, ]

max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))


library(neuralnet)

trainNN = scaled[index , ]
testNN = scaled[-index , ]

set.seed(2)
NN = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN, hidden = c(10,40,60), linear.output = T )

plot(NN)

predict_testNN = compute(NN, testNN[,c(1:5)])
predict_testNN = (predict_testNN$net.result * (max(data$rating) - min(data$rating))) + min(data$rating)

plot(datatest$rating, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")

abline(0,1)

RMSE.NN = (sum((datatest$rating - predict_testNN)^2) / nrow(datatest)) ^ 0.5

