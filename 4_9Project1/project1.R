first <- read.table("household_power_consumption.txt",header=TRUE,sep=";")
first$Date <- strptime(first$Date,format="%d/%m/%Y")
first$Date <- as.Date(first$Date)
index1 <- subset(first,first$Date=="2007-02-01")
index2 <- subset(first,first$Date=="2007-02-02")
project1 <- rbind(index1,index2)
project1$Global_active_power <- as.character(project1$Global_active_power)
project1$Global_active_power <- as.numeric(project1$Global_active_power)
project1$Sub_metering_1 <- as.numeric(as.character(project1$Sub_metering_1))
project1$Sub_metering_2 <- as.numeric(as.character(project1$Sub_metering_2))
project1$Sub_metering_3 <- as.numeric(as.character(project1$Sub_metering_3))
project1$Voltage <- as.numeric(as.character(project1$Voltage))
project1$Global_reactive_power <- as.numeric(
  as.character(project1$Global_reactive_power))
## Creating Plot 1
png(filename="plot1.png")
hist(project1$Global_active_power,main="Global Active Power",
     xlab="Global Active Power (kilowatts)",col="red")
dev.off()
## Creating Plot 2
conca=paste(project1$Date,project1$Time)
totime = strptime(conca,"%Y-%m-%d %H:%M:%S")
Sys.setlocale("LC_TIME", "English")
png(filename="plot2.png")
plot(totime,project1$Global_active_power,type="l",xlab="",
     ylab="Global Active Power (kiowatts)")
dev.off()
## Creating Plot 3
png(filename="plot3.png")
plot(totime,project1$Sub_metering_1,type="l",xlab="",
     ylab="Energy sub metering")
lines(totime,project1$Sub_metering_2,col="red")
lines(totime,project1$Sub_metering_3,col="blue")
legend("topright",lty=1, col=c("black","red","blue"),
       legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
dev.off()
## Creating Plot 4
png(filename="plot4.png")
par(mfrow=c(2,2))
plot(totime,project1$Global_active_power,type="l",ylab="Global Active Power")
plot(totime,project1$Voltage,type="l",ylab="Voltage")
plot(totime,project1$Sub_metering_1,type="l",ylab="Energy sub metering")
lines(totime,project1$Sub_metering_2,col="red")
lines(totime,project1$Sub_metering_3,col="blue")
legend("topright",lty=1, col=c("black","red","blue"),
      legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), bty="n")
plot(totime,project1$Global_reactive_power,type="l",ylab="Global Reactive Power")
dev.off()