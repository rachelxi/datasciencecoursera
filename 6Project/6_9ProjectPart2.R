# Load the data and perform basic processing
data(ToothGrowth)
names(ToothGrowth)
# Where len is a numeric vector of tooth length, supp is a factor 
# vector of Supplement type (VC or OJ), does is a numeric vector for
# dose in milligrams
head(ToothGrowth)
dim(ToothGrowth)
# Perform exploratory analysis
quartz()
p <- ggplot(ToothGrowth,aes(x=dose,y=len,colour=supp))+geom_boxplot()
p+theme_bw()+facet_grid(dose~supp)+labs(title="Tooth Growth 
                                        of 10 Guinea Pigs")
dev.off()
# See the help files for the dataset
# Get the summary
library(plyr)
ddply(ToothGrowth,.(supp,dose),summarize,
      mean=mean(len),var=var(len),sd=sd(len))
# Data Processing
inter <- split(ToothGrowth,ToothGrowth$dose)
inter2 <- split(ToothGrowth,ToothGrowth$supp)
suppOJ <- inter2[[1]]
suppVC <- inter2[[2]]
# Perform hypothesis test and confidence level included for 
# the six groups
# t.test for supplement under dose=0.5, 1, 2mg
t.test(len~supp,data=inter[[1]],paired=FALSE)
t.test(len~supp,data=inter[[2]],paired=FALSE)
t.test(len~supp,data=inter[[3]],paired=FALSE)
# t.test for different doeses with same supplement=OJ or VC
t.test(suppOJ$dose,suppOJ$len,paired=FALSE)
t.test(suppVC$dose,suppVC$len,paired=FALSE)

