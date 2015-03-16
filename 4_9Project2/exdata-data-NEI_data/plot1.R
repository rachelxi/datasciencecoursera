setwd("~/Datascience/4_9Project2/exdata-data-NEI_data")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
EM <- split(NEI,NEI$year)
EMtot=rep(0,4)
for(i in 1:4){
    EMtot[i]=sum(EM[[i]]$Emissions)
}
year=c(1999,2002,2005,2008)
xlabel=format(EMtot,digits=0)
xlabel=as.numeric(xlabel)
png(filename="plot1.png")
par(mar=c(5.1,6.1,4.1,2.1))
plot(year,EMtot,type="b",main="Total Emission of PM2.5 in US",
     xlab="year",ylab="",xaxt='n',yaxt='n')
mtext("total emissions/tons",side=2,line=5)
axis(2, at=EMtot,labels=xlabel, las=2)
axis(1, at=year,labels=year, las=2)
dev.off()