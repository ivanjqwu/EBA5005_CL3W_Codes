
highlevel<-read.csv("highleveldata.csv")
highlevel<-highlevel[,-1]
highlevel<-highlevel[,-1]
highlevel<-highlevel[,-1]
#summary(highlevel)
class(highlevel)
class(highlevel$Transaction_Time)
highlevel<-highlevel[-17048,]

hldata<-highlevel[,4:8]

hldata<-hldata[,-grep("Transaction_Cycle.day.|Followers",colnames(hldata))]

hldata$Transaction_Time<-as.Date(hldata$Transaction_Time,"%Y-%m-%d")
class(hldata$Transaction_Time)
highlevel
totalpricedata<-highlevel[,c(4,25)]
totalpricedata$Transaction_Time<-as.Date(totalpricedata$Transaction_Time,"%Y-%m-%d")

library(plyr)

meanmonthPrice_PerSquare<-aggregate(. ~ cut(hldata$Transaction_Time, "1 month"), hldata, mean)
summonthtotalprice<-aggregate(. ~ cut(totalpricedata$Transaction_Time, "1 month"), totalpricedata, sum)

names(summonthtotalprice) <- c("Time","Area","sum_total_price")

write.csv(summonthtotalprice, "monthsumtotalprice.csv")
names(meanmonthPrice_PerSquare) <- c("Time","total_price ","Price_PerSquare","Area")

meanmonthPrice_PerSquare$`Average_total_price `<-meanmonthPrice_PerSquare$Price_PerSquare*meanmonthPrice_PerSquare$Area
meanmonthPrice_PerSquare



tspp<-ts(meanmonthPrice_PerSquare$Price_PerSquare,frequency=12, start=c(2015,7))
plot(tspp)

#write.csv(meanmonthPrice_PerSquare, "meanmonth.csv")

d1<-diff(meanmonthPrice_PerSquare$Price_PerSquare)
plot.ts(d1,frequency=12, start=c(2015,7))
acf(d1)
pacf(d1)

