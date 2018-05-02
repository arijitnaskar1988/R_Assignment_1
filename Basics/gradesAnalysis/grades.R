grades<- read.csv("grades.csv")

library(stringr)

grades$extention<- str_split_fixed(grades$submission,"[\\.]",4)[,4]

length(which(grades$extention=="zip"))/length(grades$extention)

grades$convertedsubmissionDate<- as.POSIXct(strptime(as.character(grades$submit_time), 
                                                     "%m/%d/%y-%H:%M:%S"))
grades$submissionDateOnly<- as.Date(grades$convertedsubmissionDate,format = "%B %d %Y")
grades$submissionTimeOnly<- format(grades$convertedsubmissionDate, format="%H") 
                                                     
grades$deadline<- as.POSIXct(strptime(as.character("Jan 3, 2017 - 23:59:59"),"%b %d,%Y - %H:%M:%S"))

grades$ontimeSubmission<- grades$convertedsubmissionDate <= grades$deadline


library(dplyr)

grades%>%
group_by(submissionDateOnly)%>%
  summarise(Count=n())

timeOnly<-grades%>%
  group_by(submissionTimeOnly)%>%
  summarise(Count=n())

length(which(grades$ontimeSubmission==FALSE))
