library(RCurl)


#Property Price Register
########################
#Returns a data.frame which needs to be cleaned

#Data is stored as csv files

data_url <- c(
   "https://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2015-Dublin.csv/$FILE/PPR-2015-Dublin.csv"
  ,"https://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2014-Dublin.csv/$FILE/PPR-2014-Dublin.csv"
  ,"https://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2013-Dublin.csv/$FILE/PPR-2013-Dublin.csv"
  ,"https://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2012-Dublin.csv/$FILE/PPR-2012-Dublin.csv"
  ,"https://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2011-Dublin.csv/$FILE/PPR-2011-Dublin.csv"
  ,"https://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2010-Dublin.csv/$FILE/PPR-2010-Dublin.csv"
)

#Read the data from the csv files and store in a data.frame
raw_csv_data <- lapply(data_url, function(x) getURL(x))

#Convert the raw csv to a data.frame
csv_data <- lapply(1:length(raw_csv_data), function(x) read.csv(text=unlist(raw_csv_data[x]), stringsAsFactors=F))

#Combine the data.frame
csv_data <- do.call(rbind, csv_data)


