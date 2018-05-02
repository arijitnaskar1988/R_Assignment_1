newsPopularity<- read.csv("popularity.csv")


library(ggplot2)

# The mode of a categorical variable is the value (category) that occurs the most often.
# What is the mode of the num_keywords variable in the News Popularity data set?

ggplot(data=newsPopularity,mapping =aes(x=factor(newsPopularity$num_keywords))) + 
  geom_histogram(stat = "count")+
  theme(axis.text.x = element_text(angle = 45,hjust = 2,inherit.blank = TRUE))


mean(newsPopularity$shares)
median(newsPopularity$shares)

ylim1 = boxplot.stats(newsPopularity$shares)$stats[c(1, 5)]

ggplot(newsPopularity,aes(x="",y=shares))+
    geom_boxplot(notch = F)+
  geom_jitter(width = .2) 


#What number of shares falls at the 78th percentile?
quantile(newsPopularity$shares, seq(0, 1, 0.13))

#The mean number of shares lies between:
mean(newsPopularity$shares)
quantile(newsPopularity$shares, seq(0, 1, 0.1))


# Apply the quantile function again to get the percentile distribution shares at 
#1%, 2%, 3%,...100% in your R console. 
# At which percentile do you see the most drastic change in the number of shares?
drasticshares<-data.frame(quantile(newsPopularity$shares, seq(0, 1, 0.01)))
drasticshares$ID <- seq(0,nrow(drasticshares)-1)

require(scales)

ggplot(drasticshares,
       aes(x=(drasticshares$ID),
           y=(drasticshares$quantile.newsPopularity.shares..seq.0..1..0.01..)))+
  geom_point(col="red")+
  geom_line()+
  theme(axis.text.y = element_text(angle = 45,hjust = 0,inherit.blank = TRUE))+
  scale_y_continuous(labels = scales::comma)

# remove outliers
# remove 95 percentile in the plot
# this is the manupulation only in the ggplot
ggplot(drasticshares,
       aes(x=(drasticshares$ID),
           y=(drasticshares$quantile.newsPopularity.shares..seq.0..1..0.01..)))+
  geom_point(col="red")+
  geom_line()+
  theme(axis.text.y = element_text(angle = 45,hjust = 0,inherit.blank = TRUE))+
  scale_y_continuous(labels = scales::comma)+
  scale_y_continuous(limits = 
                        quantile(drasticshares$quantile.newsPopularity.shares..seq.0..1..0.01..,
                                 c(0, 0.95)))

# to show % in y axis scale_y_continuous(labels = scales::percent)





# manupulate the data to remove the outliers from the data 
drasticsharesRemovingOutliers<- 
        data.frame(Val=quantile(newsPopularity$shares, seq(0.0, .94, 0.01)))

drasticsharesRemovingOutliers$ID <- seq(0,nrow(drasticsharesRemovingOutliers)-1)

str(drasticsharesRemovingOutliers)


mean(drasticsharesRemovingOutliers$Val)
sd(drasticsharesRemovingOutliers$Val)
sd(newsPopularity$shares[which(newsPopularity$shares< 10800)])


  

