odi<- read.csv("odi-batting.csv")

onlyCenturyOwner<- odi%>%
  filter(odi$Runs>=100)

m<-onlyCenturyOwner%>%
  group_by(onlyCenturyOwner$Player)%>%
  summarise(Count=n())
  
calculateStikeRate<-function(totalRuns,totalBallsFaced)
{
  return((totalRuns*100)/totalBallsFaced)
}

onlyCenturyOwner%>%
  group_by(Player)%>%
  summarise(totalRun=sum(Runs),TotalBallFaced=sum(Balls),
            StrikeRate=calculateStikeRate(totalRun,TotalBallFaced))%>%
  arrange(desc(StrikeRate))


onlyCenturyOwner$strikeRate<- 
  calculateStikeRate(onlyCenturyOwner$Runs,onlyCenturyOwner$Balls)


library(ggplot2)

#24-02-2010 >- 
indian<-onlyCenturyOwner%>% filter(Country=="India")
x<-"24-02-2010"
y<-as.Date(x,"%d-%m-%Y")
indian$date<- strptime(x=as.character(indian$MatchDate),format = "%d-%m-%Y")
indian$Year<- format(indian$date,"%Y")

ggplot(indian,
       aes(x=factor(Year)))+
  geom_histogram( stat = "count")

