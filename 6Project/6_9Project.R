lambda <- 0.2
n <- 40
# Create the random exponential distribution of the average 
# 40 distributions per simulation and run 1000 simulations to 
# create the original data
myexp <- NULL
for(i in 1:1000)myexp <- rbind(myexp,rexp(n,lambda))
# Explore the sample mean vector of the simulations
means <- NULL
for(i in 1:1000)means <- c(means,mean(myexp[i,]))
# Plot the histogram of the means of simulations
hist(means,breaks=20, col="lightblue")
abline(v=1/lambda,col="red",lwd=5)
abline(v=mean(means),col="green",lwd=5)
# Figure out the actual population mean
mean(means)
# Figure out the sample variance of the simulations 
variance <- NULL
for(i in 1:1000)variance <- c(variance,var(myexp[i,]))
# Plot the histogram of the sample variance
hist(variance,breaks=20, col="lightblue")
abline(v=(1/lambda)^2,col="red",lwd=5)
abline(v=mean(variance),col="green",lwd=5)
# Figure out the population variance of the means
var(means)
# Figure out the theoretical population variance
(1/lambda)^2/n
# Compare the the difference between the distribution of a 
# large collection of random exponentials and the distribution of a 
# large collection of averages of 40 exponentials
quartz()
par(mfrow=c(1,2))
hist(rexp(1000,0.2),breaks=20,col="lightgray",
     xlab="index",main="Histogram of 1000 random exponentials")
hist(means,breaks=20,col="lightblue",
     main="Histogram of 1000 averages of 40 exponentials")
dev.off()
# Normalise the data to see whether it satisfies a normal 
# distribution
new <- (means-(1/lambda))/(1/lambda)
# Plot the histogram of the adjusted data and compare it with a 
# standard normal distibution line
hist(new,breaks=20,col="lightblue",xlab="normalized means",
     main="Histogram of normalized means",prob=TRUE)
curve(dnorm(x, mean=mean(new), sd=sd(new)), col="red", 
      lwd=3, add=TRUE)