library(reshape2)
library(ggplot2)

churn<- readxl::read_excel("churn.xlsx")

# x<- churn%>% 
#     is.na %>% 
#   melt %>%
#   ggplot(data = .,
#               aes(x = X2,
#                   y = X1))%>%
#   geom_raster(aes(fill = value)) +
#   scale_fill_grey(name = "",
#                   labels = c("Present","Missing")) +
#   theme_minimal() + 
#   theme(axis.text.x  = element_text(angle=45, vjust=0.5))+
#   labs(x = "Variables in Dataset",
#        y = "Rows / observations")
library(VIM)
library(mice)
data(churn, package = "VIM")
md.pattern(churn)
data(sleep, package = "VIM")
md.pattern(churn)
