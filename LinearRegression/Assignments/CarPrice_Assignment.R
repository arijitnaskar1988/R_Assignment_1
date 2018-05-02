#-------------------------------------------------------------------------#
#                     Data Loading                                        #
#-------------------------------------------------------------------------#

carprice<- read.csv("CarPrice_Assignment.csv", stringsAsFactors = T)

#-------------------------------------------------------------------------#
#                     Library Import                                      #
#-------------------------------------------------------------------------#
library(ggplot2)
library(tidyr)
library(psych)
library(DescTools) 
library(corrplot)
library(reshape2)
library(car)
library(MASS)

#-------------------------------------------------------------------------#
#                     Utility helper function                             #
#-------------------------------------------------------------------------#

# validate Na's in the data set
analyze_NAs<-function(dataset)
{
  na_values<- as.data.frame(sapply(dataset,function(x)sum(is.na(x))))
  colnames(na_values)<- c('NA_Count')
  ggplot(na_values,aes(rownames(na_values),NA_Count,label='NA Values',fill='count of NA\'s'))+
    geom_col()+
    theme(axis.text.x = element_text(angle =90, hjust = 1,vjust=.5,inherit.blank = TRUE,size=8))+
    labs(x = "Column Names", y= "Count OF NA's")
}

# helper function to find and analyze the outliers 
analyze_Outliers<-function(targetColumn)
{
  #browser()
  outlier_Upper_cutoff<- quantile(targetColumn,.75)+ 1.5 * IQR(targetColumn)
  outlier_lower_cutoff<- quantile(targetColumn,.25) - 1.5 * IQR(targetColumn)
  index_outlier_ROT<- which(targetColumn>outlier_Upper_cutoff | targetColumn<outlier_lower_cutoff)
  str(index_outlier_ROT)
  summary(targetColumn)
  Desc(targetColumn)
}


#-------------------------------------------------------------------------#
#                     data understanding  - Overview                      #
#-------------------------------------------------------------------------#
# 
dim(carprice)
summary(carprice)
describe(carprice)
str(carprice)

#-------------------------------------------------------------------------#
#                     data quality and data gathering issue               #
#-------------------------------------------------------------------------#


# analysis of NA values in the whole data frame
sum(is.na(carprice)) 
analyze_NAs(carprice) # all good with NA's as no NA values

# uniqueness of CarId 
sum(duplicated(carprice$car_ID)) # No Duplicates, Car_ID Unique data
length(unique(carprice$car_ID)) == length(carprice$car_ID) # all the unique rows


# keep the original data frame for future reference
# and process the data on a new dataframe
carprice.Processed <- carprice


#-------------------------------------------------------------------------#
#                     Numerical Variables Analysis                        #
#-------------------------------------------------------------------------#

# saperate numerical variable along with the Price to understand the outliers
carprice.Numerical <- carprice.Processed[c(10:14,17,19:26)]

# distributions of all the numerical variables
ggplot(stack(carprice.Numerical), aes(x = ind, y = values)) +
  geom_boxplot(outlier.colour = "Red")+ 
  facet_wrap(~ind, scales='free')+
  geom_jitter(width = 0.05, alpha= .2)


# distributions of all the numerical variables
ggplot(data = melt(carprice.Numerical), mapping = aes(x = value)) + 
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')

# So, there are some outliers for 
#   1.  Wheelbase
#   2.  carLength
#   3.  carWidth 
#   4.  engineSize
#   5.  stroke
#   6.  compressionRatio 
#   7.  hp
#   8.  peak RPM
#   9   city mpg
#   10. highway

# so lets analyze individual columns in detail for justification
analyze_Outliers(carprice.Numerical$wheelbase)                     # 3 /205
analyze_Outliers(carprice.Numerical$carlength)                     # 1 /205
analyze_Outliers(carprice.Numerical$carwidth)                      # 8 /205  
analyze_Outliers(carprice.Numerical$enginesize)                    # 10/205  
analyze_Outliers(carprice.Numerical$stroke)                        # 20/205  
analyze_Outliers(carprice.Numerical$compressionratio)              # 28/205  
analyze_Outliers(carprice.Numerical$horsepower)                    # 6 /205
analyze_Outliers(carprice.Numerical$peakrpm)                       # 2 /205
analyze_Outliers(carprice.Numerical$citympg)                       # 2 /205
analyze_Outliers(carprice.Numerical$highwaympg)                    # 3 /205
analyze_Outliers(carprice.Numerical$curbweight)                    # 0 /205
analyze_Outliers(carprice.Numerical$carheight)                     # 0 /205
analyze_Outliers(carprice.Numerical$boreratio)                     # 0 /205


# lets check the distribution of the data in PRICE
analyze_Outliers(carprice.Processed$price)                         # 15 /205



# relation between the all numerical variables vs PRICE
carprice.Numerical%>%
    gather(-price, key="var" , value="value")%>%
    ggplot(aes(x=value,y=price))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~var , scales= "free")+
    theme_bw()+xlab("Independent Variables")+ ylab("PRICE")

# correlation between all numerical variables
corrplot(cor(carprice.Numerical),method = "circle",type="full",
         outline = T,addgrid.col = "darkgray",order="hclust",
         mar = c(2,0,1,0),title = "Numeric - Correlation All variables")


#-------------------------------------------------------------------------#
#                     categorical variables Analysis                      #
#-------------------------------------------------------------------------#

# Carbody vs Price
ggplot(carprice.Processed,aes(x = carbody,y = price,fill=carbody))+
  geom_boxplot(outlier.colour = "blue",outlier.size = 1)+
  scale_y_continuous(name = "Price")+
  ggtitle(label = "Carbody vs Price")

# Fueltype vs Price
ggplot(carprice.Processed,aes(x = fueltype,y = price,fill=fueltype))+
  geom_boxplot(outlier.colour = "blue",outlier.size = 1)+
  scale_y_continuous(name = "Price of the CAR")+
  xlab("Fueltype")+ 
  ggtitle(label = "Fueltype vs Price")

# Drivewheel vs Price
ggplot(carprice.Processed,aes(x = drivewheel,y = price,fill=drivewheel))+
  geom_boxplot(outlier.colour = "blue",outlier.size = 1)+
  scale_y_continuous(name = "Price")+
  ggtitle(label = "Drivewheel vs Price")

# Enginelocation vs Price
ggplot(carprice.Processed,aes(x = enginelocation,y = price,fill=enginelocation))+
  geom_boxplot(outlier.colour = "blue",outlier.size = 1)+
  scale_y_continuous(name = "Price")+
  ggtitle(label = "Enginelocation vs Price")

# Enginetype vs Price
ggplot(carprice.Processed,aes(x = enginetype,y = price,fill=enginetype))+
  geom_boxplot(outlier.colour = "blue",outlier.size = 1)+scale_y_continuous(name = "Price")+
  ggtitle(label = "Enginetype vs Price")

# Aspiration vs Price
ggplot(carprice.Processed,aes(x = aspiration,y = price,fill=aspiration))+
  geom_boxplot(outlier.colour = "blue",outlier.size = 1)+
  scale_y_continuous(name = "Price")+
  ggtitle(label = "Aspiration vs Price")

# Fuelsystem vs Price
ggplot(carprice.Processed,aes(x = fuelsystem,y = price,fill=fuelsystem))+
  geom_boxplot(outlier.colour = "red",outlier.size = 1)+scale_y_continuous(name = "Price")+
  ggtitle(label = "Fuelsystem vs Price")

#----------------------------------------------#
#         DUMMY Variable creation              #
#----------------------------------------------#

# CarName - Split into two columns as mentioned in the requirements
carprice.Processed$CarName <- as.character(carprice.Processed$CarName)
carprice.Processed$CompanyName<- sapply(strsplit(as.character(carprice.Processed$CarName),' '),"[", 1)
#carprice$CarModelName<- sapply(strsplit(as.character(carprice$CarName),' '),"[", 2)

carprice.Processed$CarName<- NULL

# Nissan and nissan is same company should merge both 
carprice.Processed[which(carprice.Processed$CompanyName == "Nissan"),]$CompanyName <- "nissan"

# volkswagen , vokswagen ,  vw are same
carprice.Processed[which(carprice.Processed$CompanyName == "vokswagen"),]$CompanyName <- "volkswagen"
carprice.Processed[which(carprice.Processed$CompanyName == "vw"),]$CompanyName <- "volkswagen"

# porcshce and porsche
carprice.Processed[which(carprice.Processed$CompanyName == "porcshce"),]$CompanyName <- "porsche"

# maxda and mazda
carprice.Processed[which(carprice.Processed$CompanyName == "maxda"),]$CompanyName <- "mazda"

# alfa-romero is wrong , it should be alfa-romeo, it is not 
# significant as we will use Dummy variables for this
carprice.Processed[which(carprice.Processed$CompanyName == "alfa-romero"),]$CompanyName <- "alfa-romeo"

# toyouta and toyota are same 
carprice.Processed[which(carprice.Processed$CompanyName == "toyouta"),]$CompanyName <- "toyota"

# Analysis of Company name
ggplot(carprice.Processed,aes(x=CompanyName))+
  geom_bar(aes(fill = factor(drivewheel)))+
  xlab("CompanyName") + 
  ylab("Frequency")+
  ggtitle(paste("CompanyName Vs Drivewheel"))+
  geom_text(stat='count',aes(label=..count..),hjust=0)+coord_flip()

ggplot(carprice.Processed,aes(x = CompanyName,y = price,fill=CompanyName))+
  geom_boxplot(outlier.colour = "blue",outlier.size = 1)+
  scale_y_continuous(name = "Price")+
  ggtitle(label = "CompanyName vs Price")+ 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


dummy_CompanyName <- data.frame(model.matrix( ~CompanyName, data = carprice.Processed))

dummy_CompanyName <- dummy_CompanyName[,-1]
carprice.Processed <- cbind(carprice.Processed, dummy_CompanyName)
carprice.Processed$CompanyName<- NULL
remove(dummy_CompanyName)

#----------------------------------------#
#       symboling                        #
#----------------------------------------#
carprice.Processed$symboling <- factor(carprice.Processed$symboling)
summary(carprice.Processed$symboling)
dummy_symboling <- data.frame(model.matrix( ~symboling, data = carprice.Processed))
dummy_symboling <- dummy_symboling[,-1]
carprice.Processed <- cbind(carprice.Processed, dummy_symboling)
carprice.Processed$symboling<- NULL
remove(dummy_symboling)


#----------------------------------------#
#             Fuel Type                  #
#----------------------------------------#
summary(carprice.Processed$fueltype)
dummy_carprice <- data.frame(model.matrix( ~fueltype, data = carprice.Processed))
validation.FuelType<- data.frame(carprice.Processed$fueltype,dummy_carprice$fueltypegas)
carprice.Processed$fueltype <- dummy_carprice$fueltypegas

remove(dummy_carprice)
remove(validation.FuelType)


#----------------------------------------#
#             aspiration                 #
#----------------------------------------#
summary(carprice.Processed$aspiration)
dummy_aspiration <- data.frame(model.matrix(~aspiration, data= carprice.Processed))
validation.aspiration<- data.frame(carprice.Processed$aspiration,dummy_aspiration$aspirationturbo)

carprice.Processed$aspiration <- dummy_aspiration$aspirationturbo

remove(validation.aspiration)
remove(dummy_aspiration)


#----------------------------------------#
#             doorNumber                 #
#----------------------------------------#
summary(carprice.Processed$doornumber)
dummy_doornumber<- data.frame(model.matrix(~doornumber,data=carprice.Processed))

carprice.Processed$doornumber<- dummy_doornumber$doornumbertwo

remove(dummy_doornumber)


#----------------------------------------#
#             carbody                    #
#----------------------------------------#
summary(carprice.Processed$carbody)
dummy_carbody<- data.frame(model.matrix(~carbody,data=carprice.Processed))
dummy_carbody <- dummy_carbody[,-1]
carprice.Processed <- cbind(carprice.Processed, dummy_carbody)
carprice.Processed$carbody<- NULL
remove(dummy_carbody)


#----------------------------------------#
#             drivewheel                 #
#----------------------------------------#
summary(carprice.Processed$drivewheel)
dummy_drivewheel<- data.frame(model.matrix(~drivewheel,data=carprice.Processed))
dummy_drivewheel <- dummy_drivewheel[,-1]
carprice.Processed <- cbind(carprice.Processed, dummy_drivewheel)
carprice.Processed$drivewheel<- NULL
remove(dummy_drivewheel)


#----------------------------------------#
#             enginelocation             #
#----------------------------------------#
summary(carprice.Processed$enginelocation)
dummy_enginelocation<- data.frame(model.matrix(~enginelocation,data=carprice.Processed))
carprice.Processed$enginelocation<- dummy_enginelocation$enginelocationrear
remove(dummy_enginelocation)


#----------------------------------------#
#             enginetype                 #
#----------------------------------------#
summary(carprice.Processed$enginetype)
dummy_enginetype<- data.frame(model.matrix(~enginetype,data=carprice.Processed))
dummy_enginetype <- dummy_enginetype[,-1]
carprice.Processed <- cbind(carprice.Processed, dummy_enginetype)
carprice.Processed$enginetype<- NULL
remove(dummy_enginetype)

#----------------------------------------#
#             cylindernumber             #
#----------------------------------------#
summary(carprice.Processed$cylindernumber)
dummy_cylindernumber<- data.frame(model.matrix(~cylindernumber,data=carprice.Processed))
dummy_cylindernumber <- dummy_cylindernumber[,-1]
carprice.Processed <- cbind(carprice.Processed, dummy_cylindernumber)
carprice.Processed$cylindernumber<- NULL
remove(dummy_cylindernumber)


#----------------------------------------#
#            fuelsystem                  #
#----------------------------------------#
summary(carprice.Processed$fuelsystem)
dummy_fuelsystem<- data.frame(model.matrix(~fuelsystem,data=carprice.Processed))
dummy_fuelsystem <- dummy_fuelsystem[,-1]
carprice.Processed <- cbind(carprice.Processed, dummy_fuelsystem)
carprice.Processed$fuelsystem<- NULL
remove(dummy_fuelsystem)


# Now we should remove the variable carID as it is not significant for Price 
carprice.Processed$car_ID<- NULL

#---------------------------------------------------------#
#    Model Creation Steps: Data Seeding                   #
#---------------------------------------------------------#

# training and testing data 70-30 

set.seed(100)
train_indices <- sample(1:nrow(carprice.Processed), .7*nrow((carprice.Processed)))
carprice.Training <- carprice.Processed[train_indices,]
carprice.Testing  <- carprice.Processed[-train_indices,]
dim(carprice.Training)
dim(carprice.Testing)


#-------------------------------------------------------------------#
#    Model 1 and 1.1: Use all the variables to predict Price        #
#        Multiple R-squared:  0.9819, Adjusted R-squared:  0.9691   #
#-------------------------------------------------------------------#
model.1 = lm(price~.,data=carprice.Training)
summary(model.1)

# vif(model.1) #  there are aliased coefficients in the model

# find the column which are perfectly collinear
insignificantColumns = attributes(alias(model.1)$Complete)$dimnames[[1]]
insignificantColumns

insignificantIndex<- !(carprice.Training %in%insignificantColumns)
`%ni%` <- Negate(`%in%`)
carprice.Training<-subset(carprice.Training,select = names(carprice.Training) %ni% insignificantColumns)

model.1.1 = lm(price~.,data=carprice.Training)
sort(vif(model.1.1))
summary(model.1.1)

stepAIC(model.1.1,direction = "both")


#------------------------------------------------------------------#
# Model 2: Removed all the variable suggested by stepAIC function  #
#           and only use the target variable suggested by AIC      #
#        Multiple R-squared:  0.981,	Adjusted R-squared:  0.9739  #
#------------------------------------------------------------------#
model.2<- lm(formula = price ~ aspiration + enginelocation + carlength + 
     carwidth + curbweight + enginesize + stroke + peakrpm + citympg + 
     CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
     CompanyNamejaguar + CompanyNamemazda + CompanyNamemercury + 
     CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
     CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
     CompanyNamesaab + CompanyNamesubaru + CompanyNametoyota + 
     CompanyNamevolkswagen + symboling.1 + symboling0 + symboling3 + 
     carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
     drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
     fuelsystem2bbl + fuelsystemmpfi, data = carprice.Training)
summary(model.2)
sort(vif(model.2),decreasing = T)

#------------------------------------------------------------------#
# Model 3:                                                         #
#     - Remove    : fuelsystemmpfi                                 #
#     - Criteria : high p-value = 0.228988                         #
#     - Multiple R-squared:  0.9808,	Adjusted R-squared:  0.9737  #
#------------------------------------------------------------------#
model.3<- lm(formula = price ~ aspiration + enginelocation + carlength + 
               carwidth + curbweight + enginesize + stroke + peakrpm + citympg + 
               CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
               CompanyNamejaguar + CompanyNamemazda + CompanyNamemercury + 
               CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
               CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
               CompanyNamesaab + CompanyNamesubaru + CompanyNametoyota + 
               CompanyNamevolkswagen + symboling.1 + symboling0 + symboling3 + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
               fuelsystem2bbl , data = carprice.Training)

summary(model.3)
# Multiple R-squared:  0.9808,	Adjusted R-squared:  0.9737  
sort(vif(model.3),decreasing = T)


#------------------------------------------------------------------#
# Model 4:                                                         #
#     - Remove    : citympg                                        #
#     - Criteria : high p-value = 0.379526                         #
#     - Multiple R-squared:  0.9806,	Adjusted R-squared:  0.9738  #
#------------------------------------------------------------------#
model.4<- lm(formula = price ~ aspiration + enginelocation + carlength + 
               carwidth + curbweight + enginesize + stroke + peakrpm +
               CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
               CompanyNamejaguar + CompanyNamemazda + CompanyNamemercury + 
               CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
               CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
               CompanyNamesaab + CompanyNamesubaru + CompanyNametoyota + 
               CompanyNamevolkswagen + symboling.1 + symboling0 + symboling3 + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
               fuelsystem2bbl , data = carprice.Training)

summary(model.4)
sort(vif(model.4),decreasing = T)


#------------------------------------------------------------------#
# Model 5:                                                         #
#     - Remove    : carlength                                      #
#     - Criteria : high p-value = 0.272289                         #
#     - Multiple R-squared:  0.9804,	Adjusted R-squared:  0.9737  #
#------------------------------------------------------------------#
model.5<- lm(formula = price ~ aspiration + enginelocation +
               carwidth + curbweight + enginesize + stroke + peakrpm +
               CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
               CompanyNamejaguar + CompanyNamemazda + CompanyNamemercury + 
               CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
               CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
               CompanyNamesaab + CompanyNamesubaru + CompanyNametoyota + 
               CompanyNamevolkswagen + symboling.1 + symboling0 + symboling3 + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
               fuelsystem2bbl , data = carprice.Training)

summary(model.5)
sort(vif(model.5),decreasing = T)

#------------------------------------------------------------------#
# Model 6:                                                         #
#     - Remove    : fuelsystem2bbl                                 #
#     - Criteria : high p-value = 0.199946                         #
#     - Multiple R-squared:  0.9801,	Adjusted R-squared:  0.9736  #
#------------------------------------------------------------------#
model.6<- lm(formula = price ~ aspiration + enginelocation +
               carwidth + curbweight + enginesize + stroke + peakrpm +
               CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
               CompanyNamejaguar + CompanyNamemazda + CompanyNamemercury + 
               CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
               CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
               CompanyNamesaab + CompanyNamesubaru + CompanyNametoyota + 
               CompanyNamevolkswagen + symboling.1 + symboling0 + symboling3 + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, 
                data = carprice.Training)

summary(model.6)
sort(vif(model.6),decreasing = T)

#------------------------------------------------------------------#
# Model 7:                                                         #
#     - Remove    : symboling0                                     #
#     - Criteria : high p-value = 0.157982                         #
#     - Multiple R-squared:  0.9797,	Adjusted R-squared:  0.9733  #
#------------------------------------------------------------------#
model.7<- lm(formula = price ~ aspiration + enginelocation +
               carwidth + curbweight + enginesize + stroke + peakrpm +
               CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
               CompanyNamejaguar + CompanyNamemazda + CompanyNamemercury + 
               CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
               CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
               CompanyNamesaab + CompanyNamesubaru + CompanyNametoyota + 
               CompanyNamevolkswagen + symboling.1 + symboling3 + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, 
             data = carprice.Training)

summary(model.7)
sort(vif(model.7),decreasing = T)


#------------------------------------------------------------------#
# Model 8:                                                         #
#     - Remove    : symboling.1                                    #
#     - Criteria : high p-value = 0.287003                         #
#     - Multiple R-squared:  0.9795,	Adjusted R-squared:  0.9733  #
#------------------------------------------------------------------#
model.8<- lm(formula = price ~ aspiration + enginelocation +
               carwidth + curbweight + enginesize + stroke + peakrpm +
               CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
               CompanyNamejaguar + CompanyNamemazda + CompanyNamemercury + 
               CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
               CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
               CompanyNamesaab + CompanyNamesubaru + CompanyNametoyota + 
               CompanyNamevolkswagen + symboling3 + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, 
             data = carprice.Training)

summary(model.8)
sort(vif(model.8),decreasing = T)

#------------------------------------------------------------------#
# Model 9:                                                         #
#     - Remove    : CompanyNamemercury                             #
#     - Criteria : high p-value = 0.135561                         #
#     - Multiple R-squared:  0.9791,	Adjusted R-squared:  0.973   #
#------------------------------------------------------------------#
model.9<- lm(formula = price ~ aspiration + enginelocation +
               carwidth + curbweight + enginesize + stroke + peakrpm +
               CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
               CompanyNamejaguar + CompanyNamemazda +  
               CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
               CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
               CompanyNamesaab + CompanyNamesubaru + CompanyNametoyota + 
               CompanyNamevolkswagen + symboling3 + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, 
             data = carprice.Training)

summary(model.9)
sort(vif(model.9),decreasing = T)

#------------------------------------------------------------------#
# Model 10:                                                        #
#     - Remove    : symboling3                                     #
#     - Criteria : high p-value = 0.137079                         #
#     - Multiple R-squared:  0.9786,	Adjusted R-squared:  0.9727  #
#------------------------------------------------------------------#
model.10<- lm(formula = price ~ aspiration + enginelocation +
               carwidth + curbweight + enginesize + stroke + peakrpm +
               CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
               CompanyNamejaguar + CompanyNamemazda +  
               CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
               CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
               CompanyNamesaab + CompanyNamesubaru + CompanyNametoyota + 
               CompanyNamevolkswagen + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, 
             data = carprice.Training)

summary(model.10)
sort(vif(model.10),decreasing = T)

#------------------------------------------------------------------#
# Model 11:                                                        #
#     - Remove    : cylindernumberfive                             #
#     - Criteria : high p-value = 0.093797                         #
#     - Multiple R-squared:  0.9781,	Adjusted R-squared:  0.9722  #
#------------------------------------------------------------------#
model.11<- lm(formula = price ~ aspiration + enginelocation +
                carwidth + curbweight + enginesize + stroke + peakrpm +
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                CompanyNamejaguar + CompanyNamemazda +  
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                CompanyNamesaab + CompanyNamesubaru + CompanyNametoyota + 
                CompanyNamevolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypeohc + enginetyperotor , 
              data = carprice.Training)

summary(model.11)
sort(vif(model.11),decreasing = T)
 

#------------------------------------------------------------------#
# Model 12:                                                        #
#     - Remove    : curbweight                                     #
#     - Criteria  : VIF -> 19.155142                               #
#                   p-value -> 0.038025 (*)                        #
#     - Multiple R-squared:  0.9772,	Adjusted R-squared:  0.9714  #
#------------------------------------------------------------------#
model.12<- lm(formula = price ~ aspiration + enginelocation +
                carwidth + carbodyhardtop + enginesize + stroke + peakrpm +
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                CompanyNamejaguar + CompanyNamemazda +  
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                CompanyNamesaab + CompanyNamesubaru + CompanyNametoyota + 
                CompanyNamevolkswagen + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypeohc + enginetyperotor , 
              data = carprice.Training)

summary(model.12)
sort(vif(model.12),decreasing = T)

#------------------------------------------------------------------#
# Model 13:                                                        #
#     - Remove    : carbodywagon                                   #
#     - Criteria  : VIF     - 6.726513                             #
#                   p-value - 018144 (*)                           #
#     - Multiple R-squared:  0.9765,	Adjusted R-squared:  0.971   #
#------------------------------------------------------------------#
model.13<- lm(formula = price ~ aspiration + enginelocation +
                carwidth + curbweight + enginesize + stroke + peakrpm +
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                CompanyNamejaguar + CompanyNamemazda +  
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                CompanyNamesaab + CompanyNamesubaru + CompanyNametoyota + 
                CompanyNamevolkswagen + 
                carbodyhatchback +
                drivewheelrwd + enginetypeohc + enginetyperotor , 
              data = carprice.Training)

summary(model.13)
sort(vif(model.13),decreasing = T)

#------------------------------------------------------------------#
# Model 14:                                                        #
#     - Remove    : carbodyhatchback                               #
#     - Criteria  :                                                #
#                   p-value - 0.198217                             #
#     - Multiple R-squared:  0.9762,	Adjusted R-squared:  0.9709  #
#------------------------------------------------------------------#
model.14<- lm(formula = price ~ aspiration + enginelocation +
                carwidth + curbweight + enginesize + stroke + peakrpm +
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                CompanyNamejaguar + CompanyNamemazda +  
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                CompanyNamesaab + CompanyNamesubaru + CompanyNametoyota + 
                CompanyNamevolkswagen + 
                drivewheelrwd + enginetypeohc + enginetyperotor , 
              data = carprice.Training)

summary(model.14)
sort(vif(model.14),decreasing = T)


#------------------------------------------------------------------#
# Model 15:                                                        #
#     - Remove    : curbweight                                     #
#     - Criteria  : VIF  - 13.094006                               #
#                   pvalue - 0.017011 (*)                          #
#     - Multiple R-squared:  0.975,	Adjusted R-squared:  0.9697    #
#------------------------------------------------------------------#
model.15<- lm(formula = price ~ aspiration + enginelocation +
                carwidth + enginesize + stroke + peakrpm +
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                CompanyNamejaguar + CompanyNamemazda +  
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                CompanyNamesaab + CompanyNamesubaru + CompanyNametoyota + 
                CompanyNamevolkswagen + 
                drivewheelrwd + enginetypeohc + enginetyperotor , 
              data = carprice.Training)

summary(model.15)
sort(vif(model.15),decreasing = T)

#------------------------------------------------------------------#
# Model 16:                                                        #
#     - Remove    : CompanyNameporsche                             #
#     - Criteria  : VIF -       4.819894                           #
#                   p-value -  0.041874                            #
#     - Multiple R-squared:  0.9741,	Adjusted R-squared:  0.9688  #
#------------------------------------------------------------------#
model.16<- lm(formula = price ~ aspiration + enginelocation +
                carwidth + enginesize + stroke + peakrpm +
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                CompanyNamejaguar + CompanyNamemazda +  
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNamerenault + 
                CompanyNamesaab + CompanyNamesubaru + CompanyNametoyota + 
                CompanyNamevolkswagen + 
                drivewheelrwd + enginetypeohc + enginetyperotor , 
              data = carprice.Training)

summary(model.16)
sort(vif(model.16),decreasing = T)

#------------------------------------------------------------------#
# Model 17:                                                        #
#     - Remove    : enginetypeohc                                  #
#     - Criteria  : VIF - 4.227312                                 #
#                   p-value - 0.005826 (**)                        #
#     - Multiple R-squared:  0.9724,	Adjusted R-squared:  0.967   #
#------------------------------------------------------------------#
model.17<- lm(formula = price ~ aspiration + enginelocation +
                carwidth + enginesize + stroke + peakrpm +
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                CompanyNamejaguar + CompanyNamemazda +  
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNamerenault + 
                CompanyNamesaab + CompanyNamesubaru + CompanyNametoyota + 
                CompanyNamevolkswagen + 
                drivewheelrwd + enginetyperotor , 
              data = carprice.Training)

summary(model.17)
sort(vif(model.17),decreasing = T)



#------------------------------------------------------------------#
# Model 18:                                                        #
#     - Remove    : CompanyNamesaab                                #
#     - Criteria  :                                                #
#                   p-value - 0.051805 (**)                        #
#     - Multiple R-squared:  0.9715,	Adjusted R-squared:  0.9662  #
#------------------------------------------------------------------#
model.18<- lm(formula = price ~ aspiration + enginelocation +
                carwidth + enginesize + stroke + peakrpm +
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                CompanyNamejaguar + CompanyNamemazda +  
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNamerenault + 
                CompanyNamesubaru + CompanyNametoyota + 
                CompanyNamevolkswagen + 
                drivewheelrwd + enginetyperotor , 
              data = carprice.Training)

summary(model.18)
sort(vif(model.18),decreasing = T)

#------------------------------------------------------------------#
# Model 19:                                                        #
#     - Remove    : CompanyNamehonda                               #
#     - Criteria  : VIF - 2.658700                                 #
#                   p-value - 0.00143 (**)                         #
#     - Multiple R-squared:  0.9689,	Adjusted R-squared:  0.9635  #
#------------------------------------------------------------------#
model.19<- lm(formula = price ~ aspiration + enginelocation +
                carwidth + enginesize + stroke + peakrpm +
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +
                CompanyNamejaguar + CompanyNamemazda +  
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNamerenault + 
                CompanyNamesubaru + CompanyNametoyota + 
                CompanyNamevolkswagen + 
                drivewheelrwd + enginetyperotor , 
              data = carprice.Training)

summary(model.19)
sort(vif(model.19),decreasing = T)

#------------------------------------------------------------------#
# Model 20:                                                        #
#     - Remove    : CompanyNamevolkswagen                          #
#     - Criteria  :                                                #
#                   p-value - 0.063525                             #
#     - Multiple R-squared:  0.968,	Adjusted R-squared:  0.9628    #
#------------------------------------------------------------------#
model.20<- lm(formula = price ~ aspiration + enginelocation +
                carwidth + enginesize + stroke + peakrpm +
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +
                CompanyNamejaguar + CompanyNamemazda +  
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNamerenault + 
                CompanyNamesubaru + CompanyNametoyota + 
                drivewheelrwd + enginetyperotor , 
              data = carprice.Training)

summary(model.20)
sort(vif(model.20),decreasing = T)


#------------------------------------------------------------------#
# Model 21:                                                        #
#     - Remove    : CompanyNamepeugeot                             #
#     - Criteria  :                                                #
#                   p-value - 0.08007                              #
#     - Multiple R-squared:  0.9672,	Adjusted R-squared:  0.9622  #
#------------------------------------------------------------------#
model.21<- lm(formula = price ~ aspiration + enginelocation +
                carwidth + enginesize + stroke + peakrpm +
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +
                CompanyNamejaguar + CompanyNamemazda +  
                CompanyNamemitsubishi + CompanyNamenissan + 
                CompanyNameplymouth + CompanyNamerenault + 
                CompanyNamesubaru + CompanyNametoyota + 
                drivewheelrwd + enginetyperotor , 
              data = carprice.Training)

summary(model.21)
sort(vif(model.21),decreasing = T)

#------------------------------------------------------------------#
# Model 22:                                                        #
#     - Remove    : CompanyNamerenault                             #
#     - Criteria  :                                                #
#                   p-value - 0.08405                              #
#     - Multiple R-squared:  0.9664,	Adjusted R-squared:  0.9615  #
#------------------------------------------------------------------#
model.22<- lm(formula = price ~ aspiration + enginelocation +
                carwidth + enginesize + stroke + peakrpm +
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +
                CompanyNamejaguar + CompanyNamemazda +  
                CompanyNamemitsubishi + CompanyNamenissan + 
                CompanyNameplymouth +
                CompanyNamesubaru + CompanyNametoyota + 
                drivewheelrwd + enginetyperotor , 
              data = carprice.Training)

summary(model.22)
sort(vif(model.22),decreasing = T)

#------------------------------------------------------------------#
# Model 23:                                                        #
#     - Remove    : drivewheelrwd                                  #
#     - Criteria  : VIF -  2.612486                                #
#                   p-value 0.00189 (**)                           #
#     - Multiple R-squared:  0.9637,	Adjusted R-squared:  0.9587  #
#------------------------------------------------------------------#
model.23<- lm(formula = price ~ aspiration + enginelocation +
                carwidth + enginesize + stroke + peakrpm +
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +
                CompanyNamejaguar + CompanyNamemazda +  
                CompanyNamemitsubishi + CompanyNamenissan + 
                CompanyNameplymouth +
                CompanyNamesubaru + CompanyNametoyota + 
                enginetyperotor , 
              data = carprice.Training)

summary(model.23)
sort(vif(model.23),decreasing = T)

#------------------------------------------------------------------#
# Model 24:                                                        #
#     - Remove    : CompanyNamenissan                              #
#     - Criteria  :                                                #
#                   p-value 0.019244                               #
#     - Multiple R-squared:  0.962,	Adjusted R-squared:  0.9572    #
#------------------------------------------------------------------#
model.24<- lm(formula = price ~ aspiration + enginelocation +
                carwidth + enginesize + stroke + peakrpm +
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +
                CompanyNamejaguar + CompanyNamemazda +  
                CompanyNamemitsubishi +
                CompanyNameplymouth +
                CompanyNamesubaru + CompanyNametoyota + 
                enginetyperotor , 
              data = carprice.Training)

summary(model.24)
sort(vif(model.24),decreasing = T)


#------------------------------------------------------------------#
# Model 25:                                                        #
#     - Remove    : CompanyNamemazda                               #
#     - Criteria  :                                                #
#                   p-value 0.048661 *                             #
#     - Multiple R-squared:  0.9609,	Adjusted R-squared:  0.9562  #
#------------------------------------------------------------------#
model.25<- lm(formula = price ~ aspiration + enginelocation +
                carwidth + enginesize + stroke + peakrpm +
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +
                CompanyNamejaguar +  
                CompanyNamemitsubishi +
                CompanyNameplymouth +
                CompanyNamesubaru + CompanyNametoyota + 
                enginetyperotor , 
              data = carprice.Training)

summary(model.25)
sort(vif(model.25),decreasing = T)


#------------------------------------------------------------------#
# Model 26:                                                        #
#     - Remove    : CompanyNametoyota                              #
#     - Criteria  :                                                #
#                   p-value 0.066536 *                             #
#     - Multiple R-squared:  0.9598,	Adjusted R-squared:  0.9554  #
#------------------------------------------------------------------#
model.26<- lm(formula = price ~ aspiration + enginelocation +
                carwidth + enginesize + stroke + peakrpm +
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +
                CompanyNamejaguar +  
                CompanyNamemitsubishi +
                CompanyNameplymouth +
                CompanyNamesubaru +
                enginetyperotor , 
              data = carprice.Training)

summary(model.26)
sort(vif(model.26),decreasing = T)

#------------------------------------------------------------------#
# Model 27:                                                        #
#     - Remove    : CompanyNamedodge                               #
#     - Criteria  :                                                #
#                   p-value 0.064487                               #
#     - Multiple R-squared:  0.9587,	Adjusted R-squared:  0.9545  #
#------------------------------------------------------------------#
model.27<- lm(formula = price ~ aspiration + enginelocation +
                carwidth + enginesize + stroke + peakrpm +
                CompanyNamebmw + CompanyNamebuick + 
                CompanyNamejaguar +  
                CompanyNamemitsubishi +
                CompanyNameplymouth +
                CompanyNamesubaru +
                enginetyperotor , 
              data = carprice.Training)

summary(model.27)
sort(vif(model.27),decreasing = T)


#------------------------------------------------------------------#
# Model 28:                                                        #
#     - Remove    : CompanyNameplymouth                            #
#     - Criteria  :                                                #
#                   p-value 0.087666                               #
#     - Multiple R-squared:  0.9578,	Adjusted R-squared:  0.9539  #
#------------------------------------------------------------------#
model.28<- lm(formula = price ~ aspiration + enginelocation +
                carwidth + enginesize + stroke + peakrpm +
                CompanyNamebmw + CompanyNamebuick + 
                CompanyNamejaguar +  
                CompanyNamemitsubishi +
                CompanyNamesubaru +
                enginetyperotor , 
              data = carprice.Training)

summary(model.28)
sort(vif(model.28),decreasing = T)


#------------------------------------------------------------------#
# Model 29:                                                        #
#     - Remove    : CompanyNamemitsubishi                          #
#     - Criteria  :                                                #
#                   p-value 0.005632                               #
#     - Multiple R-squared:  0.9552,	Adjusted R-squared:  0.9514  #
#------------------------------------------------------------------#
model.29<- lm(formula = price ~ aspiration + enginelocation +
                carwidth + enginesize + stroke + peakrpm +
                CompanyNamebmw + CompanyNamebuick + 
                CompanyNamejaguar +  
                CompanyNamesubaru +
                enginetyperotor , 
              data = carprice.Training)

summary(model.29)
sort(vif(model.29),decreasing = T)

#------------------------------------------------------------------#
# Model 30:                                                        #
#     - Remove    : peakrpm                                        #
#     - Criteria  : peakrpm is not very much significant as the    #
#                   Beta value (Estimate) is very low = 1.706      #
#                   as compared to other variables                 #
#     - Multiple R-squared:  0.9477,	Adjusted R-squared:  0.9437  #
#------------------------------------------------------------------#
model.30<- lm(formula = price ~ aspiration + enginelocation +
                carwidth + enginesize + stroke  +
                CompanyNamebmw + CompanyNamebuick + 
                CompanyNamejaguar +  
                CompanyNamesubaru +
                enginetyperotor , 
              data = carprice.Training)


summary(model.30)

#---------------------------------------------------------#
#    Model Testing and Accuracy of the Prediction         #
#---------------------------------------------------------#

Predict_Car.Price <- predict(model.30,carprice.Testing)
carprice.Testing$Predicted_price <- Predict_Car.Price

# Accuracy of the predictions
# Calculate correlation
r <- cor(carprice.Testing$price,carprice.Testing$Predicted_price)
# calculate R squared by squaring correlation
rsquared <- r^2

# chack the r
print(r)
# check R-squared
print(rsquared)


###########################
##-----------------------##
##    Conclusion         ##
##-----------------------##
###########################

# ------------------------------------------------------------------------------------------#
#     Management should foucs on the following factor variables                             #
#     1. enginelocation - with highest priority                                             #
#     2. CarWidth                                                                           #
#     3. aspiration                                                                         #
#     4. stroke (-ve)                                                                       #
#     5. enginetyperotor                                                                    #
#     6. The Brand Value - [ Company bmw, Company buick, Company jaguar, Company subaru ]   #
#                         We have noticed that the Brand                                    #
#                         value is having very high impact on the price                     #
#                         So, Initial phase of the market penetration                       #
#                         Company should focus on the goodwill of the company               #
#                         to create the market value - may be quality matketing             #
#                         could be one possible option                                      #
# ------------------------------------------------------------------------------------------#
