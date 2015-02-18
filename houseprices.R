data <- rbind(read.csv(file="http://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2013-Dublin.csv/$FILE/PPR-2013-Dublin.csv"),
read.csv(file="http://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2012-Dublin.csv/$FILE/PPR-2012-Dublin.csv"),
read.csv(file="http://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2011-Dublin.csv/$FILE/PPR-2011-Dublin.csv"),
read.csv(file="http://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2010-Dublin.csv/$FILE/PPR-2010-Dublin.csv"))
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

dundrum <- sale.data[grep("[dD]undrum",sale.data$address),]
z <- xtabs(price~year+month, data=dundrum)/xtabs(~year+month, data=dundrum)
average.price <- ts(c(z[1,],z[2,],z[3,],z[4,]), start=c(2010,1), freq=12)
graph.prices <- aggregate(price~year+month, data=dundrum,  FUN=mean)
graph.prices$date <- as.Date(paste0("15/", graph.prices$month,"/", graph.prices$year), format="%d/%m/%Y")
dundrum.data <- data.frame(graph.prices, location="Dundrum")



milltown <- sale.data[grep("[mM]illtown",sale.data$address),]
z <- xtabs(price~year+month, data=dundrum)/xtabs(~year+month, data=milltown)
average.price <- ts(c(z[1,],z[2,],z[3,],z[4,]), start=c(2010,1), freq=12)
graph.prices <- aggregate(price~year+month, data=milltown,  FUN=mean)
graph.prices$date <- as.Date(paste0("15/", graph.prices$month,"/", graph.prices$year), format="%d/%m/%Y")
milltown.data <- data.frame(graph.prices, location="Milltown")

terenure <- sale.data[grep("[tT]erenure",sale.data$address),]
z <- xtabs(price~year+month, data=terenure)/xtabs(~year+month, data=milltown)
average.price <- ts(c(z[1,],z[2,],z[3,],z[4,]), start=c(2010,1), freq=12)
graph.prices <- aggregate(price~year+month, data=terenure,  FUN=mean)
graph.prices$date <- as.Date(paste0("15/", graph.prices$month,"/", graph.prices$year), format="%d/%m/%Y")
terenure.data <- data.frame(graph.prices, location="Terenure")

dunlaoghaire <- sale.data[grep("aoghai",sale.data$address),]
z <- xtabs(price~year+month, data=dunlaoghaire)/xtabs(~year+month, data=milltown)
average.price <- ts(c(z[1,],z[2,],z[3,],z[4,]), start=c(2010,1), freq=12)
graph.prices <- aggregate(price~year+month, data=dunlaoghaire,  FUN=mean)
graph.prices$date <- as.Date(paste0("15/", graph.prices$month,"/", graph.prices$year), format="%d/%m/%Y")
dunlaoghaire.data <- data.frame(graph.prices, location="Dun Laoghaire")

plot.data = rbind(dunlaoghaire.data,milltown.data, terenure.data, dundrum.data)
#ggplot(graph.prices, aes(y=price, x=date, col=post.code))+geom_line()
#condition <- c(grep("TRUE",graph.prices$post.code==""),grep("16", graph.prices$post.code),grep("14", graph.prices$post.code),grep("16", graph.prices$post.code),grep("6", graph.prices$post.code))
#plot.data <- graph.prices[condition,]
library(scales)

ggplot(plot.data, aes(y=price, x=date, col=location))+geom_smooth(se=FALSE)+ scale_y_continuous(labels = comma)+labs(title="Average House Prices")+theme_bw()
