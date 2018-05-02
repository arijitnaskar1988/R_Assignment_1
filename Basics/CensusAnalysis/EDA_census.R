library(gdata)

ffdata = read.xls("EDA_census.xlsx", 
                  sheet=2)
library(dplyr)

ffdata$literacyRatesTotal <-ffdata$Illiterate.Total/ffdata$Total.Person
ffdata$literacyRatesM <-ffdata$Illiterate.M/ffdata$Total.M
ffdata$literacyRatesF <-ffdata$Illiterate.F/ffdata$Total.F

#target columns
m<- c(1:16,46,47,48)
fffataFilteredColumn<- ffdata[,m]


# literacyRate for 20-24 F 
x<-fffataFilteredColumn%>%
    filter(fffataFilteredColumn$Total.Rural.Urban=="Total" & 
             fffataFilteredColumn$Area.Name=="INDIA" &
             fffataFilteredColumn$AgeGroup == "20-24")
x$literacyRatesF
    

# literacy rate for Old Age group
oldGrup<-fffataFilteredColumn%>%
  filter(fffataFilteredColumn$Total.Rural.Urban=="Total" &
           fffataFilteredColumn$Area.Name=="INDIA" &
           (fffataFilteredColumn$AgeGroup == "55-59" |
              fffataFilteredColumn$AgeGroup == "60-64"|
              fffataFilteredColumn$AgeGroup == "65-69"|
              fffataFilteredColumn$AgeGroup == "70-74"|
              fffataFilteredColumn$AgeGroup == "75-79"|
              fffataFilteredColumn$AgeGroup == "80+"))

sum(oldGrup$Illiterate.Total)/sum(oldGrup$Total.Person)



# young Group Analysis
targteAge<-c("0-6","7","8","9","10","11","12","13","14",
             "15","16","17","18","19","20-24","25-29")
youngGrup<-fffataFilteredColumn%>%
  filter((fffataFilteredColumn$Total.Rural.Urban=="Total") & 
         (fffataFilteredColumn$Area.Name=="INDIA")  &
         (fffataFilteredColumn$AgeGroup %in% targteAge))

sum(youngGrup$Illiterate.Total)/sum(youngGrup$Total.Person)

library(ggplot2)
OnlyIndia<-fffataFilteredColumn%>%
      filter((fffataFilteredColumn$Total.Rural.Urban=="Total") & 
           (fffataFilteredColumn$Area.Name!="INDIA"))

ggplot(OnlyIndia,aes(x=factor(OnlyIndia$Area.Name),
                                y= OnlyIndia$literacyRatesTotal))+
geom_boxplot()


withState<-fffataFilteredColumn%>%
  filter(fffataFilteredColumn$Total.Rural.Urban =="Total" & 
           fffataFilteredColumn$Area.Name!="INDIA"  & 
           fffataFilteredColumn$AgeGroup =="All ages")
  
library(ggplot2)
ggplot(withState,aes(x=factor(withState$Area.Name),y=withState$literacyRatesF))+
         geom_point()+
  theme(axis.text.x = element_text(angle =90, hjust = 1,inherit.blank = TRUE))
      

