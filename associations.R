library(data.table)

df <- fread("~/data/accos/assoc.csv", encoding = "UTF-8")

prog <- fread("~/data/accos/prog.csv")

prog$V1 <- NULL 

prog1 <- fread("~/data/accos/program.csv")

prog$V1 <- NULL

data.frame(kodi) 

kodi$word <- ifelse(df$word=="кодирование", kodi$word <- 1, 0)

n=0



re <- apply(kodi[,1:2], 1, min)

NE <- "кодирование"

kodi <- df[1:as.integer(length(rownames(df))),1]

kodi$word <- ifelse((df$word=="кодирование"),1,0)

kodi$assoc <- ifelse(kodi$word > 0, df$assoc, )

kodi <- df[df$word=="кодирование",1:2]
programist <- df[df$word=="программист",1:2]
programma <- df[df$word=="программа",1:2]
algo <- df[df$word=="алгоритм",1:2]
comp <- df[df$word=="компьютер",1:2]
mass <- df[df$word=="массив",1:2]
info <- df[df$word=="информация",1:2]
file <- df[df$word=="файл",1:2]
team <- df[df$word=="команда",1:2]
site <- df[df$word=="сайт",1:2]
math <- df[df$word=="математика",1:2]
number <- df[df$word=="число",1:2]
cifra <- df[df$word=="цифра",1:2]
lang <- df[df$word=="язык",1:2]

prog$assoc <- NULL

assoc1 <- rbind(kodi,programist, programma, algo, comp, mass, info, file, team, site, math, number, cifra, lang)
assoc2 <- cbind(prog, assoc1)
colnames(assoc2) <- c("word","assoc1", "assoc2")
View(assoc2)

qw <- as.data.frame(unique(assoc2$assoc2))
colnames(qw) <- "word"
newsparse <- as.data.frame(as.matrix(assoc2))
library(wordcloud)
wordcloud(newsparse, max.words = 100, random.order = FALSE)

write.csv(assoc2, "~/data/accos/program.csv", row.names = FALSE)

as <- data.frame(table(unlist(strsplit(tolower(prog1$assoc2), " "))))

colnames(as) <- c("assoc2", "freq")

dff <- as %>% dplyr::group_by(assoc2) %>% 
  dplyr::summarise(suuum=sum(freq))

ggplot(as$assoc2, as$freq)
library(plotly)
plot_ly(x=~as$freq,y=~as$assoc2, type="scatter", showlegend = FALSE)

as1 <- as.data.frame(order(as$freq, method="auto"))
as1 <- as[order(-as$freq),]
as1$id <- 1:nrow(as1)

write.csv(as1, "~/data/accos/words.csv", row.names = FALSE)



vozm <- df[df$word=="возможность",1:2]
progres <- df[df$word=="прогресс",1:2]
comp <- df[df$word=="компьютер",1:2]
pute <- df[df$word=="путешествие",1:2]

