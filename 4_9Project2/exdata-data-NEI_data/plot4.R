setwd("~/Datascience/4_9Project2/exdata-data-NEI_data")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
Match1 <- grep("Coal",SCC$Short.Name,fixed=TRUE)
New1 <- as.character(SCC$SCC[Match1])
New2 <- as.character(SCC$Short.Name[Match1])
Match2 <- grep("Comb",New2,fixed=TRUE)
Indicator <- cbind(New1[Match2],New2[Match2])
EM99 <- 0
EM02 <- 0
EM05 <- 0
EM08 <- 0
for(i in 1:91){
  inter <- grep(Indicator[i,1],NEI$SCC)
  inter1 <- inter[which(NEI$year[inter]==1999)]
  EM99 <-sum(NEI$Emissions[inter1])+EM99
  inter1 <- inter[which(NEI$year[inter]==2002)]
  EM02 <-sum(NEI$Emissions[inter1])+EM02
  inter1 <- inter[which(NEI$year[inter]==2005)]
  EM05 <-sum(NEI$Emissions[inter1])+EM05
  inter1 <- inter[which(NEI$year[inter]==2008)]
  EM08 <-sum(NEI$Emissions[inter1])+EM08
  inter <- NULL
  inter1 <- NULL
}
year <- c(1999,2002,2005,2008)
EMtot <- c(EM99,EM02,EM05,EM08)
xlabel=format(EMtot,digits=0)
xlabel=as.numeric(xlabel)
png(filename="plot4.png")
par(mar=c(5.1,6.1,4.1,2.1))
plot(year,EMtot,type="b",main="Total Emission of Coal Combustion Resources in US",
     xlab="year",ylab="",xaxt='n',yaxt='n')
mtext("total emissions/tons",side=2,line=5)
axis(2, at=EMtot,labels=xlabel, las=2)
axis(1, at=year,labels=year, las=2)
dev.off()