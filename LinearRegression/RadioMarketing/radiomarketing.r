radiomarketingData<- read.csv("radiomarketing.csv")



plot(radiomarketingData$Radio,radiomarketingData$Sales)



set.seed(100)
trainindices_1= sample(1:nrow(radiomarketingData), 0.7*nrow(radiomarketingData))

train.Radio = radiomarketingData[trainindices_1,]

test.radio.problem = radiomarketingData[-trainindices_1,]


model_1<- lm(Sales~Radio,data=train.Radio)

summary(model_1)

predict_2<- predict(model_1,test.radio.problem[-2])
