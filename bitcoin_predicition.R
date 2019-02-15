library(prophet)
library(data.table)
df <- fread("~/data/crypto/crypto-markets.csv")
df <- df[df$name=="Bitcoin",]
colnames(df)
df <- df[,c(4,9)]
head(df)
colnames(df) <- c("ds","y")

df$y<- log(df$y)
m <- prophet(df)
future <- make_future_dataframe(m, periods = 130)
head(future)
forecast <- predict(m, future)
head(forecast)
plot(m, forecast)
prophet_plot_components(m, forecast)

table(df$name=="Bitcoin")
