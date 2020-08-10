#Assesment Programming skills needed
library(dslabs)
data(heights)
heights

#1
nrow(heights)

#2
heights[777,]

#5
max(heights$height)
which.min(heights$height)

#6
mean(heights$height)
median(heights$height)

#7
ind<-which(heights$sex=="Male")
length(ind)/length(heights$height)
sum(heights$sex=="Male")/length(heights$height)

#8
sum(heights$sex=="Female" & heights$height>78)
