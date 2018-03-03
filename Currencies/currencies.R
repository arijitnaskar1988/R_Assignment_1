cur<- read.csv("currencies.csv")

install.packages("corrplot")
library(corrplot)

  targetCurr<- 
  data.frame(cur$Euro,
         cur$Japanese.Yen,
         cur$U.K..Pound.Sterling,
         cur$U.S..Dollar,
         cur$Australian.Dollar,
         cur$Indian.Rupee,cur$Chinese.Yuan)

library(ggcorrplot)

cormat <- round(cor(targetCurr),2)

corr<-cor(as.matrix(targetCurr), use = "pairwise.complete.obs")

corrplot(corr)
  