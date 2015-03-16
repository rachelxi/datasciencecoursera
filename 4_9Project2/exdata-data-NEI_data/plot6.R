setwd("~/Datascience/4_9Project2/exdata-data-NEI_data")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
Match1 <- grep("Motor",SCC$Short.Name,fixed=TRUE)
New1 <- as.character(SCC$SCC[Match1])
New2 <- as.character(SCC$Short.Name[Match1])
Indicator <- cbind(New1,New2)
city<- split(NEI,NEI$fips)
for(i in 1:3263){
  if(city[[i]]$fips[1]=="24510")
    baltimore<- city[[i]]
  end
  if(city[[i]]$fips[1]=="06037")
    LA<- city[[i]]
  end
}
EM99 <- 0
EM02 <- 0
EM05 <- 0
EM08 <- 0
for(i in 1:138){
  inter <- grep(Indicator[i,1],baltimore$SCC)
  inter1 <- inter[which(baltimore$year[inter]==1999)]
  EM99 <-sum(baltimore$Emissions[inter1])+EM99
  inter1 <- inter[which(baltimore$year[inter]==2002)]
  EM02 <-sum(baltimore$Emissions[inter1])+EM02
  inter1 <- inter[which(baltimore$year[inter]==2005)]
  EM05 <-sum(baltimore$Emissions[inter1])+EM05
  inter1 <- inter[which(baltimore$year[inter]==2008)]
  EM08 <-sum(baltimore$Emissions[inter1])+EM08
  inter <- NULL
  inter1 <- NULL
}
EMtotBal <- c(EM99,EM02,EM05,EM08)
EM99 <- 0
EM02 <- 0
EM05 <- 0
EM08 <- 0
for(i in 1:138){
  inter <- grep(Indicator[i,1],LA$SCC)
  inter1 <- inter[which(LA$year[inter]==1999)]
  EM99 <-sum(LA$Emissions[inter1])+EM99
  inter1 <- inter[which(LA$year[inter]==2002)]
  EM02 <-sum(LA$Emissions[inter1])+EM02
  inter1 <- inter[which(LA$year[inter]==2005)]
  EM05 <-sum(LA$Emissions[inter1])+EM05
  inter1 <- inter[which(LA$year[inter]==2008)]
  EM08 <-sum(LA$Emissions[inter1])+EM08
  inter <- NULL
  inter1 <- NULL
}
EMtotLA <- c(EM99,EM02,EM05,EM08)
year <- c(1999,2002,2005,2008)
png(filename="plot6.png")
rng<-range(EMtotBal,EMtotLA)
par(mfrow=c(1,2))
xlabel=format(EMtotBal,digits=3)
xlabel=as.numeric(xlabel)
plot(year,EMtotBal,type="b",main="Emissions, Motor Vehicle, Baltimore",
     xlab="year",ylab="",xaxt='n',yaxt='n',ylim=rng)
mtext("total emissions/tons",side=2,line=5)
axis(2, at=EMtotBal,labels=xlabel, las=3)
axis(1, at=year,labels=year, las=2)
xlabel=format(EMtotLA,digits=3)
xlabel=as.numeric(xlabel)
plot(year,EMtotLA,type="b",main="Emissions, Motor Vehicle, LA",
     xlab="year",ylab="",xaxt='n',yaxt='n',ylim=rng)
mtext("total emissions/tons",side=2,line=3)
axis(2, at=EMtotLA,labels=xlabel, las=3)
axis(1, at=year,labels=year, las=2)
dev.off()