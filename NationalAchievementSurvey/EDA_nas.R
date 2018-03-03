
nas<-read.csv("EDA_nas.csv")

library(ggplot2)

ggplot(nas,aes(x=factor(nas$Watch.TV),y=(nas$Science..),col=factor(nas$Watch.TV)))+
    geom_boxplot()+
    geom_jitter(width = .32)+
    stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red")
    
    

ggplot(nas,aes(x=factor(nas$Father.edu),y=(nas$Maths..),col=factor(nas$Father.edu)))+
  geom_boxplot()+
  geom_jitter(width = .32)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red" )


ggplot(nas,aes(x=factor(nas$Play.games),y=(nas$Reading..),col=factor(nas$Play.games)))+
  geom_boxplot()+
  geom_jitter(width = .32)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red" )


ggplot(nas,aes(x=factor(nas$Father.edu),y=(nas$Reading..),col=factor(nas$Father.edu)))+
  geom_boxplot()+
  geom_jitter(width = .32)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red" )+
  stat_summary(fun.y=sd, geom="point", shape=20, size=5, color="blue", fill="blue" )

  

library(dplyr)
library(ggplot2)
Illiterate.Mother<- nas %>% filter(nas$Mother.edu == "Illiterate")

ggplot(Illiterate.Mother,aes(x=factor(Illiterate.Mother$Siblings)))+
         geom_histogram(stat = "count")

str(nas$Age)
summary(nas$Age)

targetedStudent<- nas%>%
    filter(nas$Age!= "11- years")


ggplot(data= targetedStudent,aes(x=factor(targetedStudent$Father.edu),y= targetedStudent$Science..))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red" )

targetedStudent1<- nas%>%
  filter(nas$Age!= "11- years" & nas$Father.edu == "Degree & above") 

ggplot(data= targetedStudent1,aes(x=factor(targetedStudent1$Age),
                                  y= targetedStudent1$Science..))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red" )


