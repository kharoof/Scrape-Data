

sale.date <- as.Date(as.character(data[,1]), format="%d/%m/%Y")

sale.data <- data.frame(address=data$Address,sale.date=sale.date, county=data$County, post.code=data$Postal.Code, price=data$Price, desc=data$Description.of.Property)


for (i in 1:6) {
  if (i != 2) {sale.data[,i] <- as.character(sale.data[,i])}
}

sale.data$price <- as.numeric(gsub(",", "",gsub("???", "", sale.data$price)))


sale.data[,5] <- as.numeric(sale.data[,5])
sale.data[,3] <- as.factor(sale.data[,3])
sale.data[,4] <- as.factor(sale.data[,4])

library(lubridate)
sale.data$year <- year(sale.data$sale.date)
sale.data$month <- month(sale.data$sale.date)
wyckham <- sale.data[grep("[wW][yi]ckham [pP]oint",sale.data$address),]
graph.prices <- aggregate(price~year+month, data=wyckham,  FUN=mean)
graph.prices$date <- as.Date(paste0("15/", graph.prices$month,"/", graph.prices$year), format="%d/%m/%Y")
wyckham.data <- data.frame(graph.prices, location="Wyckham Point")
wyckham.data <- wyckham.data[order(wyckham.data[,4]),]

harbourview <- sale.data[grep("[Ha]arbour [vV]iew",sale.data$address),]
graph.prices <- aggregate(price~year+month, data=harbourview,  FUN=mean)
graph.prices$date <- as.Date(paste0("15/", graph.prices$month,"/", graph.prices$year), format="%d/%m/%Y")
harbourview.data <- data.frame(graph.prices, location="Harbour View")
harbourview.data <- harbourview.data[order(harbourview.data[,4]),]

southmede <- sale.data[grep("[sS]outhmede",sale.data$address),]
graph.prices <- aggregate(price~year+month, data=southmede,  FUN=mean)
graph.prices$date <- as.Date(paste0("15/", graph.prices$month,"/", graph.prices$year), format="%d/%m/%Y")
southmede.data <- data.frame(graph.prices, location="Southmede")
southmede.data <- southmede.data[order(southmede.data[,4]),]


holly <- sale.data[grepl("[hH]olly",sale.data$address)&grepl("[rR]ockfield",sale.data$address),]
graph.prices <- aggregate(price~year+month, data=holly,  FUN=mean)
graph.prices$date <- as.Date(paste0("15/", graph.prices$month,"/", graph.prices$year), format="%d/%m/%Y")
holly.data <- data.frame(graph.prices, location="The Holly")
holly.data <- holly.data[order(holly.data[,4]),]

oaks <- sale.data[grepl("[oO]ak",sale.data$address)&grepl("[rR]ockfield",sale.data$address),]
graph.prices <- aggregate(price~year+month, data=oaks,  FUN=mean)
graph.prices$date <- as.Date(paste0("15/", graph.prices$month,"/", graph.prices$year), format="%d/%m/%Y")
oaks.data <- data.frame(graph.prices, location="The Oaks")
oaks.data <- oaks.data[order(oaks.data[,4]),]



sweepstakes <- sale.data[grep("[sS]weepstake",sale.data$address),]
graph.prices <- aggregate(price~year+month, data=sweepstakes,  FUN=mean)
graph.prices$date <- as.Date(paste0("15/", graph.prices$month,"/", graph.prices$year), format="%d/%m/%Y")
sweepstakes.data <- data.frame(graph.prices, location="The Sweepstakes")
sweepstakes.data <- sweepstakes.data[order(sweepstakes.data[,4]),]

trimbleston <- sale.data[grep("[tT]rimbleston",sale.data$address),]
graph.prices <- aggregate(price~year+month, data=trimbleston,  FUN=mean)
graph.prices$date <- as.Date(paste0("15/", graph.prices$month,"/", graph.prices$year), format="%d/%m/%Y")
trimbleston.data <- data.frame(graph.prices, location="Trimbleston")
trimbleston.data <- trimbleston.data[order(trimbleston.data[,4]),]




building.data = rbind(wyckham.data,harbourview.data, southmede.data,  holly.data, oaks.data)#, trimbleston.data,sweepstakes.data)

#average
graph.prices <- aggregate(price~year+month, data=building.data, FUN=mean)
graph.prices$date <- as.Date(paste0("15/", graph.prices$month,"/", graph.prices$year), format="%d/%m/%Y")
average.data <- data.frame(graph.prices, location="Average")
average.data <- average.data[order(average.data[,4]),]

plot.data <- rbind(building.data, average.data)

library(ggplot2)
library(scales)
ggplot(average.data, aes(y=price, x=date, col=location))+geom_smooth(se=F)+geom_point()+ scale_y_continuous(labels = comma)+labs(title="Average House Prices")+theme_bw()
ggplot(plot.data, aes(y=price, x=date, col=location))+geom_smooth(se=F)+geom_point()+ scale_y_continuous(labels = comma)+labs(title="Average House Prices")+theme_bw()

