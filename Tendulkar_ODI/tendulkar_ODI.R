# read 
tendulkar_ODI<- read.csv("tendulkar_ODI.csv",na.strings = "-")

# tendulkar_ODI$test<- as.numeric(as.character(tendulkar_ODI$Runs))
# 
# tendulkar_ODI$OnlyRuns<- as.numeric(as.character(tendulkar_ODI$Runs))
# mgsub <- function(pattern, replacement, x, ...) {
#   if (length(pattern)!=length(replacement)) {
#     stop("pattern and replacement do not have the same length.")
#   }
#   result <- x
#   for (i in 1:length(pattern)) {
#     result <- gsub(pattern[i], replacement[i], result, ...)
#   }
#   result
# }

mgsub <- function(pattern, replacement, x, ...) {
  n = length(pattern)
  if (n != length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result = x
  for (i in 1:n) {
    result[grep(pattern[i], x, ...)] = replacement[i]
  }
  return(result)
}


#tendulkar_ODI$OnlyRuns <- gsub("DNB","-1",as.character(tendulkar_ODI$Runs))
tendulkar_ODI$OnlyRuns <- mgsub(c("DNB","TDNB"),c("-1","-1"),as.character(tendulkar_ODI$Runs))

tendulkar_ODI$OnlyRuns<-sapply(strsplit(as.character(tendulkar_ODI$OnlyRuns), "\\*"), head)
tendulkar_ODI$OnlyRuns<- as.numeric(tendulkar_ODI$OnlyRuns)

# create bins with specified numbers range 
tendulkar_ODI$RunBin<- 
  cut(tendulkar_ODI$OnlyRuns,c(0,10,20,30,40,max(tendulkar_ODI$OnlyRuns)))


library(ggplot2)

ggplot(tendulkar_ODI,aes(x=factor(RunBin)))+ geom_histogram(stat = "count")

# Plot a histogram of the number of 4s hit by Tendulkar. What is the most common value of the variable X, 
# where X represents the number of 4s hit by him?
ggplot(tendulkar_ODI,aes(x=factor(tendulkar_ODI$X4s)))+ geom_bar(stat = "count")







