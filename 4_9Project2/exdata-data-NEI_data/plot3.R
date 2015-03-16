setwd("~/Datascience/4_9Project2/exdata-data-NEI_data")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
city<- split(NEI,NEI$fips)
for(i in 1:3263){
  if(city[[i]]$fips[1]=="24510")
    baltimore<- city[[i]]
  end
}
library(ggplot2)
png(filename="plot3.png")
p<-ggplot(baltimore,aes(x=year,y=Emissions))+geom_point()
p+theme_bw()+facet_grid(.~type)
dev.off()