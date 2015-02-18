rm(list=ls())

setwd("~/Documents/R/Property Prices")

#New Download
source("getDaftData.R")
#Clean the data
source("cleanDaftData.R")

#Save downloaded data
write.csv(data.housing, file=paste0("housing-data-",Sys.Date(), ".csv"), row.names=F)
save(data, file=paste0("raw-housing-data-",Sys.Date(), ".R"))

#Clean up workspace
rm(list=setdiff(ls(), "data.housing"))


str(data.housing)


#load data
library(data.table)
data <- read.csv("./data/housing-data-2014-07-22.csv", sep=",", header=T, stringsAsFactors=F)


##add spacing after numbers
data$address <- gsub("([0-9])([[:alpha:]]{1})","\\1 \\2",data$address)
##add spacing after commas
data$address <- gsub("(,)([[:alpha:]]{1})","\\1 \\2",data$address)
##Fix Dun Laoghaire
data$address <- gsub("([dD]un)([Ll])","\\1 \\2",data$address)

data <- data.table(data)
data[,date:=as.Date(date, "%y-%m-%d")]

data[grepl("House[^/]",description),type:="House"]
data[grepl("Apartment",description),type:="Apartment"]
data[,type:=as.factor(type)]

library(ggplot2)
library(scales)
library(ggthemes)
library(lubridate)




plot.location <- function(x, return.data=FALSE) {
  x <- as.list(x)
  data.filter <- apply(do.call(cbind,lapply(x,function(x) grepl(x,data$address,ignore.case=T))),1,all) 
  if(sum(data.filter)==0) {stop("No Locations Found, Try again...")}
  tmp.data = data[data.filter]
  
  
  if(return.data==TRUE) {
    return(tmp.data)
  }
  else {
    return(  ggplot(tmp.data,aes(x=date, y=price, colour=type))+
               geom_point()+ geom_smooth(se=FALSE) + 
               scale_y_continuous(labels=comma, limits=c(0,1000000))+ 
               theme_economist_white()+ggtitle(paste(do.call(paste,x)))
    )
    
  }
}

#Automatically search county = Dublin
plot.location("Dundrum")

#Specify multiple search paramaters
plot.location(list("Dundrum","Holly", "Dublin"))

plot.location(list("Dundrum", "Dublin"))
plot.location(list("Terenure", "Dublin"))
plot.location(list("Rathfarnham", "Dublin"))
plot.location(list("Templeogue", "Dublin"))
plot.location(list("Tallaght", "Dublin"))
plot.location(list("Marino", "Dublin"))
plot.location(list("Goatstown", "Dublin"))
plot.location(list("Sandyford", "Dublin"))
plot.location(list("Leopardstown", "Dublin"))
plot.location(list("Foxrock", "Dublin"))
plot.location(list("wicklow"))
plot.location(list("wicklow", "greystones"))
plot.location(list("wicklow", "bray"))
plot.location(list("wicklow", "enniskerry"))
edit(plot.location(list("limerick", "rathkeale"), return.data=T))

plot.location(list("Dundrum","Wyckham", "Point", "Dublin"))

plot.location(list("Dun Laoghaire","Dublin"))

#See data behind the plot
edit(plot.location(list("Dundrum","Holly", "Dublin"), return.data=T))

edit(plot.location(list("Dundrum","Wyckham", "Point", "Dublin"), return.data=T))

edit(plot.location(list("Dun Laoghaire" ,"Dublin"), return.data=T))


library(gridExtra)
p1 <- plot.location(list("Dalkey", "Dublin"))
p2 <- plot.location(list("Dundrum", "Dublin"))
p3 <- plot.location(list("Goatstown", "Dublin"))
p4 <- plot.location(list("Dun Laoghaire", "Dublin"))
p5 <- plot.location(list("Milltown", "Dublin"))
p6 <- plot.location(list("Greystones", "Wicklow"))
p7 <- plot.location(list("Enniskerry", "Wicklow"))
grid.arrange(p1,p2,p3,p4,p5,p6,p7)

p1 <- plot.location(list("Wyckham", "Point"))
p2 <- plot.location(list("Holly", "Rockfield"))
p2 <- plot.location(list("Rockfield", "Dundrum"))
grid.arrange(p1,p2)
