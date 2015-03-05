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