goldSilver<- read.csv("EDA_Gold_Silver_prices.csv")

library(zoo)
#as.Date(as.yearmon("Mar-01", "%b-%y"))
goldSilver$Date<- as.Date(as.yearmon(goldSilver$Month, "%b-%y"))

cor(goldSilver$SilverPrice,goldSilver$GoldPrice)
cor(goldSilver$GoldPrice,goldSilver$SilverPrice)




library(dplyr)
subsetdate<-goldSilver %>% 
  filter(goldSilver$Date>=as.Date(as.yearmon("01-Jan-2008","%d-%b-%Y")) &
           goldSilver$Date<as.Date(as.yearmon("01-Jan-2009","%d-%b-%Y")))
cor(subsetdate$SilverPrice,subsetdate$GoldPrice)

