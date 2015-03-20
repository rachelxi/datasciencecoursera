setwd("~/Documents/Datascience/5Assessment2")
data<-read.csv(bzfile("repdata-data-StormData.csv.bz2"))
## Clean damage event types data
Event <- c("ASTRONOMICAL LOW TIDE","AVALANCHE","BLIZZARD",
         "COASTAL FLOOD","COLD/WIND CHILL","DEBRIS FLOW",
         "DENSE FOG","DENSE SMOKE","DROUGHT","DUST DEVIL",
         "DUST STORM","EXCESSIVE HEAT","EXTREME COLD/WIND CHILL",
         "FLASH FLOOD","FLOOD","FREEZING FOG","FROST/FREEZE",
         "FUNNEL CLOUD","HAIL","HEAT","HEAVY RAIN","HEAVY SNOW",
         "HIGH SURF","HIGH WIND","HURRICANE/TYPHOON","ICE STORM",
         "LAKESHORE FLOOD","LAKE-EFFECT SNOW","LIGHTNING",
         "MARINE HAIL","MARINE HIGH WIND","MARINE STRONG WIND",
         "MARINE THUNDERSTORM","RIP CURRENT","SEICHE","SLEET",
         "STORM TIDE","STORM WIND","THUNDERSTORM WIND","TORNADO",
         "TROPICAL DEPRESSION","TROPICAL STORM","TSUNAMI",
         "VOLCANIC ASH","WATERSPOUT","WILDFIRE","WINTER STORM",
         "WINTER WEATHER")
index <- NULL
INDICATOR <- rep(0,length(data$EVTYPE))
for(i in 1:48){
    inter <- which(data$EVTYPE==Event[i])
    INDICATOR[inter] <- Event[i]
    index <- c(index,inter)
    inter <- NULL
}
data1 <- cbind(data[,2],INDICATOR,data[,23:28])
StormData <- data1[index,]
colnames(StormData) <- c("BGN_DATE","EVTYPE","FATALITIES","INJURIES",
                         "PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")
YEAR <- rep(0,length(StormData$BGN_DATE))
inter <- grep(195,StormData$BGN_DATE)
YEAR[inter] <- 1950
inter <- grep(196,StormData$BGN_DATE)
YEAR[inter] <- 1960 
inter <- grep(197,StormData$BGN_DATE)
YEAR[inter] <- 1970 
inter <- grep(198,StormData$BGN_DATE)
YEAR[inter] <- 1980 
inter <- grep(199,StormData$BGN_DATE)
YEAR[inter] <- 1990 
inter <- grep(200,StormData$BGN_DATE)
YEAR[inter] <- 2000 
inter <- grep(201,StormData$BGN_DATE)
YEAR[inter] <- 2010 
StormData <- cbind(StormData,YEAR)
## Q1 fatalities and injuries related
tapply(StormData$FATALITIES,StormData$EVTYPE,sum)
max(tapply(StormData$FATALITIES,StormData$EVTYPE,sum),na.rm=TRUE)
tapply(StormData$INJURIES,StormData$EVTYPE,sum)
max(tapply(StormData$INJURIES,StormData$EVTYPE,sum),na.rm=TRUE)
index <- which(StormData$EVTYPE=="TORNADO")
Tornado <- StormData[index,]
tapply(Tornado$FATALITIES,Tornado$YEAR,sum)
tapply(Tornado$INJURIES,Tornado$YEAR,sum)
Fatalities <- tapply(Tornado$FATALITIES,Tornado$YEAR,sum)
Injuries <- tapply(Tornado$INJURIES,Tornado$YEAR,sum)
Interval <- c(1950,1960,1970,1980,1990,2000,2010)
png(file ="plot1.png", bg ="transparent")
par(mfrow=c(2,1))
barplot(Fatalities,col="blue",xlab="year",ylab="No. of people",
        main="Fatalities due to Tornado, US, 1950-2011",
        names.arg=Interval)
barplot(Injuries,col="green",xlab="year",ylab="No. of people",
        main="Injuries due to Tornado, US, 1950-2011",
        names.arg=Interval)
dev.off()
## Q2 Property Damage and Crop Damage
PROPERTY <- rep(0,length(StormData$PROPDMG))
CROP <- rep(0,length(StormData$CROPDMG))
for(i in 1:length(PROPERTY)){
    inter1 <- StormData$PROPDMGEXP[i]
    inter2 <- StormData$PROPDMG[i]
    if(inter1=="K"){PROPERTY[i] <- inter2*1e3}
    if(inter1=="M"){PROPERTY[i] <- inter2*1e6}
    if(inter1=="B"){PROPERTY[i] <- inter2*1e9}
    else{PROPERTY[i] <- inter2}
}
for(i in 1:length(CROP)){
    inter1 <- StormData$CROPDMGEXP[i]
    inter2 <- StormData$CROPDMG[i]
    if(inter1=="K"){CROP[i] <- inter2*1e3}
    if(inter1=="M"){CROP[i] <- inter2*1e6}
    if(inter1=="B"){CROP[i] <- inter2*1e9}
    else{CROP[i] <- inter2}
}
StormData <- cbind(StormData,PROPERTY,CROP)
tapply(StormData$PROPERTY,StormData$EVTYPE,sum)
max(tapply(StormData$PROPERTY,StormData$EVTYPE,sum),na.rm=TRUE)
tapply(StormData$CROP,StormData$EVTYPE,sum)
max(tapply(StormData$CROP,StormData$EVTYPE,sum),na.rm=TRUE)
index <- which(StormData$EVTYPE=="HURRICANE/TYPHOON")
Hur_Typ <- StormData[index,]
index <- which(StormData$EVTYPE=="HAIL")
Hail <- StormData[index,]
tapply(Hur_Typ$PROPERTY,Hur_Typ$YEAR,sum)
tapply(Hail$CROP,Hail$YEAR,sum)
inter <- tapply(Hur_Typ$PROPERTY,Hur_Typ$YEAR,sum)
Property <- c(0,0,0,0,0,inter,0)
names(Property)<-Interval
Crop <- tapply(Hail$CROP,Hail$YEAR,sum)
png(file ="plot2.png", bg ="transparent")
par(mfrow=c(2,1))
barplot(Property/1e6,col="blue",xlab="year",ylab="Million Dollars",
        main="Property loss due to Hurricane/Typhoon, US, 1950-2011",
        names.arg=Interval)
barplot(Crop/1e3,col="green",xlab="year",ylab="Thousand Dollars",
        main="Crop loss due to Hail, US, 1950-2011",
        names.arg=Interval)
dev.off()
## Optional: All fatalities, injuries, damage
tapply(StormData$FATALITIES,StormData$YEAR,sum)
tapply(StormData$INJURIES,StormData$YEAR,sum)
tapply(StormData$PROPERTY,StormData$YEAR,sum)
tapply(StormData$CROP,StormData$YEAR,sum)
Fatalities <- tapply(StormData$FATALITIES,StormData$YEAR,sum)
Injuries <- tapply(StormData$INJURIES,StormData$YEAR,sum)
Property <- tapply(StormData$PROPERTY,StormData$YEAR,sum)
Crop <- tapply(StormData$CROP,StormData$YEAR,sum)
Health <- rbind(Fatalities,Injuries)
Economy <- rbind(Property,Crop)
png(file ="plot3.png", bg ="transparent")
par(mfrow=c(2,1))
barplot(Health,xlab="year",ylab="No. of people",
        main="Population Health due to Weather, US, 1950-2011",
        legend=rownames(Health),col=c("blue","red"),
        args.legend=list(x="topleft"))
barplot(Economy/1e6,xlab="year",ylab="Million dollars",
        main="Economic loss due to Weather, US, 1950-2011",
        legend=rownames(Economy),col=c("blue","red"),
        args.legend=list(x="topleft"))
dev.off()