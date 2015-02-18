##Read data from the property price register
url_2014 <- "https://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2014.csv/$FILE/PPR-2014.csv"
library(RCurl)
data_text <- getURL(url_2014)
data_2014 <- read.csv(text=data_text, stringsAsFactors=F)

url_2013 <- "https://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2013.csv/$FILE/PPR-2013.csv"
library(RCurl)
data_text <- getURL(url_2013)
data_2013 <- read.csv(text=data_text, stringsAsFactors=F)

url_2012 <- "https://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2012.csv/$FILE/PPR-2012.csv"
library(RCurl)
data_text <- getURL(url_2012)
data_2012 <- read.csv(text=data_text, stringsAsFactors=F)

url_2011 <- "https://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2011.csv/$FILE/PPR-2011.csv"
library(RCurl)
data_text <- getURL(url_2011)
data_2011 <- read.csv(text=data_text, stringsAsFactors=F)

url_2010 <- "https://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2010.csv/$FILE/PPR-2010.csv"
library(RCurl)
data_text <- getURL(url_2010)
data_2010 <- read.csv(text=data_text, stringsAsFactors=F)

data <- rbind(data_2014, data_2013, data_2012, data_2011, data_2010)


                                        #Clean and format the data
names(data) <- c("date.of.sale", "address", "post.code", "county", "price", "full.market.value", "vat.exclusive","property.description", "property.size")

library(data.table)
data <- data.table(data)


data[,date.of.sale:=as.Date(date.of.sale, "%d/%m/%Y")]
data[,county:=as.factor(county)]
                                        #Remove Euro Symbol
data[,price:=gsub("[^[:alnum:]///' ]","",price)]
data[,price:=as.numeric(price)/100]


library(ggplot2)
library(scales)
library(ggthemes)
library(lubridate)




plot.location <- function(x, return.data=FALSE,county="Dublin") {
  x <- as.list(x)
  data.filter <- apply(do.call(cbind,lapply(x,function(x) grepl(x,data$address,ignore.case=T))),1,all) & data$county==county
  if(sum(data.filter)==0) {stop("No Locations Found, Try again...")}
  tmp.data = data[data.filter]
  print(county)


  if(return.data==TRUE) {
    return(tmp.data)
  }
  else {
    return(  ggplot(tmp.data,aes(x=date.of.sale, y=price))+
           geom_point()+  geom_smooth()+
           scale_y_continuous(labels=comma, limits=c(0,1000000))+ 
           theme_economist_white()+ggtitle(paste(do.call(paste,x), ", County = ", county))
           )

  }
}

#Automatically search county = Dublin
plot.location("Dundrum")
#Specify County
plot.location("Rathkeale", county="Limerick")
#See data behind the graph
edit(plot.location("Rathkeale", county="Limerick", return.data=T))

#Specify multiple search paramaters
plot.location(list("Dundrum","Holly"), county="Dublin")

