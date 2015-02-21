#Use XML htmlTreeParse to extract data from webpages,
#Use XML xpathSApply or htmltab to parse the data
#Cleaning up of the data is left for later
library(XML)
library(RCurl)
library(htmltab)
source("functions.R")

#########################################################################################################
## Property Price Register data from daft.ie website
#########################################################################################################

##Function that wraps and contains xpathSApply methods specific to this webpage.
##This extracts the info of interest from each webpage
parseDaftPropertyPriceRegResults <- function(url_html){
  daft_data <- NULL
  address = xpathSApply(url_html, "//span[@class='priceregister-address']", xmlValue)
  details = xpathSApply(url_html, "//span[@class='priceregister-dwelling-details']", xmlValue)
  daft_data <- rbind(daft_data, data.frame(address,details))
  daft_data$url_html <- docName(url_html)
  return(daft_data)
}

##First scrape raw HTML using the XML package.

##Automatically determine the number of pages to loop through
num_pages <- getNumber(getLastLink("http://www.daft.ie/priceregister", "Last"))

##Use the XML package to return the html from each page and store this as a list which we will later parse (this will take a long time)
raw_html_data <- lapply(1:num_pages, function(x) htmlTreeParse(paste0("http://www.daft.ie/priceregister/?pagenum=",x),useInternalNodes=T))

##Extract from the raw html the data that we wish to keep, This will be cleaned later
daft_property_price_register <- do.call(rbind,lapply(raw_html_data, parseDaftPropertyPriceRegResults))
#########################################################################################################

#########################################################################################################
##Munster Rugby match data from  http://www.munsterrugby.ie
#########################################################################################################

##Function that wraps and contains xpathSApply methods specific to this webpage.
##This extracts the info of interest from each webpage
parseMunsterResults <- function(url_html){
  ##Match Data
  final_score <- xpathSApply(url_html, "//td[@class='field_Score']", xmlValue)
  home_team <- xpathSApply(url_html, "//td[@class='field_HomeDisplay']", xmlValue)
  away_team <- xpathSApply(url_html, "//td[@class='field_AwayDisplay']", xmlValue)
  date <- xpathSApply(url_html, "//td[@class='field_DateShort']", xmlValue)
  time <- xpathSApply(url_html, "//td[@class='field_Time H:M']", xmlValue)
  venue <- xpathSApply(url_html, "//td[@class='field_VenName']", xmlValue)
  attendance <- xpathSApply(url_html, "//td[@class='field_BroadcastAttend']", xmlValue)
  match.data <- data.frame(final_score,home_team,away_team,date,time,venue,attendance)
  match.data$url_html <- docName(url_html)
  return(match.data)
}

##Use the XML package to return the html from each page and store this as a list which we will later parse (this will take a long time)
raw_html_data <- lapply(1995:2014, function(season) htmlTreeParse(paste0('http://www.munsterrugby.ie/rugby/munster_first_team_comprehensive_fixtures.php?includeref=6177&season=',season,"-",season+1),useInternalNodes=T))

##Extract from the raw html the data that we wish to keep, This will be cleaned later
munster <- do.call(rbind,lapply(raw_html_data, parseMunsterResults))
#########################################################################################################

#########################################################################################################
## Six Nations match data from http://www.rbs6nations.com
#########################################################################################################

##Function that uses the htmltab function to get results that are stored in tables
##This extracts the info of interest from each webpage
parseSixNationsResults <- function(url_html){
  ##On the rbs website the results are stored in a table with class list but there is a spanning column that contains
  ##the word round which we exclude
  results <- htmltab(doc=url_html, which="//td[@class='list']", body="//tr[not(@class = 'group')]")
  ##Choose which data to keep
  keep = c("Date", "Time (Local)", "Home", "Away", "Score", "Venue")
  results <- results[-1,keep]
  results$url <- docName(url_html)
  row.names(results) <- NULL
  return(results)
}

#########################
## Ireland specific
#########################
##Use the XML package to return the html from each page and store this as a list which we will later parse (this will take a long time)
raw_html_data <- lapply(1882:2014, function(season) htmlTreeParse(paste0('http://www.rbs6nations.com/en/ireland/ireland_matchcentre.php?includeref=436&season=',season,"-",season+1),useInternalNodes=T))

##Extract from the raw html the data that we wish to keep, This will be cleaned later
ireland <- do.call(rbind,lapply(raw_html_data, parseSixNationsResults))

#########################
## All Teams
#########################

##Use the XML package to return the html from each page and store this as a list which we will later parse (this will take a long time)
raw_html_data <- lapply(1882:2014, function(season) htmlTreeParse(paste0('http://www.rbs6nations.com/en/matchcentre/index.php?includeref=428&season=',season,"-",season+1),useInternalNodes=T))
##Extract from the raw html the data that we wish to keep, This will be cleaned later
six_nations <- do.call(rbind,lapply(raw_html_data, parseSixNationsResults))

#########################################################################################################

#########################################################################################################
##Daft Rentals
#########################################################################################################

##Function that wraps and contains xpathSApply methods specific to this webpage.
##This extracts the info of interest from each webpage
parseDaftRentalResults <- function(url_html){
  ##Get HTML table containing the results
  daft_data <- NULL
  name = xpathSApply(url_html, "//h2", xmlValue)
  ##There are some ads that appear as headings, remove these (valid listings show number followed by full stop so we use grep to search for these and keep these only)
  name <- name[grepl("[0-9]\\.",name)]
  price = xpathSApply(url_html, "//strong[@class='price']", xmlValue)
  ##Add a condition to check if the webpage contains any listings at all and exit if not
  if ( length(xpathSApply(url_html, "//strong[@class='price']",xmlValue)) == 0 ) {
    return(daft_data)
  }
  info = xpathSApply(url_html, "//ul[@class='info']", xmlValue)
  daft_data <- rbind(daft_data, data.frame(name, price, info))
  daft_data$url_html <- docName(url_html)
  return(daft_data)
}

##Use the XML package to return the html from each page and store this as a list which we will later parse (this will take a long time)
raw_html_data <- lapply(seq(0,2000,10), function(x) htmlTreeParse(paste0("http://www.daft.ie/dublin-city/houses-to-rent/?offset=",x), useInternalNodes=T))

##Extract from the raw html the data that we wish to keep, This will be cleaned later
daft <- do.call(rbind,lapply(raw_html_data, parseDaftRentalResults))

#########################################################################################################

#########################################################################################################
##Property Price Register
#########################################################################################################


##List the links where the data is stored as csv files
data_url <- c(
"https://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2015-Dublin.csv/$FILE/PPR-2015-Dublin.csv"
,"https://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2014-Dublin.csv/$FILE/PPR-2014-Dublin.csv"
,"https://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2013-Dublin.csv/$FILE/PPR-2013-Dublin.csv"
,"https://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2012-Dublin.csv/$FILE/PPR-2012-Dublin.csv"
,"https://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2011-Dublin.csv/$FILE/PPR-2011-Dublin.csv"
,"https://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2010-Dublin.csv/$FILE/PPR-2010-Dublin.csv"
)

##Use RCurl to read the data from the csv files and store in a data.frame
raw_csv_data <- lapply(data_url, function(x) getURL(x))

##As we have not read html we can just convert the raw csv to a data.frame using read.csv
csv_data <- lapply(1:length(raw_csv_data), function(x) read.csv(text=unlist(raw_csv_data[x]), stringsAsFactors=F))

##Combine the list of data.frames together
csv_data <- do.call(rbind, csv_data)

#########################################################################################################
