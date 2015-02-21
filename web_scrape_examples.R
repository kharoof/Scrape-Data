library(XML)


#Property Price Register data from daft.ie website
##################################################
#Returns HTML which needs to be parsed and cleaned


source("functions.R")

#Number of pages to loop through
num_pages <- getNumber(getLastLink("http://www.daft.ie/priceregister", "Last"))

#Return the html from each page to a variable (this will take a long time)
raw_html_data <- lapply(1:num_pages, function(x) htmlTreeParse(paste0("http://www.daft.ie/priceregister/?pagenum=",x),useInternalNodes=T))


parseDaftPropertyPriceRegResults <- function(url_html){
  #Get HTML table containing the results
  daft_data <- NULL
address = xpathSApply(url_html, "//span[@class='priceregister-address']", xmlValue)
details = xpathSApply(url_html, "//span[@class='priceregister-dwelling-details']", xmlValue)
daft_data <- rbind(daft_data, data.frame(address,details))
daft_data$url_html <- docName(url_html)
return(daft_data)
}

#Use XML to extract data from webpages,
#Use XML xpathSApply or htmltab to parse the data
#Cleaning up the data is left for later
#Munster Rugby, http://www.munsterrugby.ie
########################################
parseMunsterResults <- function(url_html){
#Get HTML table containing the results
#On the rbs website the results are stored in a table with class list but there is a spanning column that contains
#the word round which we exclude
#Player Data
#home_player_data <- htmltab(doc=url_html, which=1)
#away_player_data <- htmltab(doc=url_html, which=2)
#Match Data
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
#Use the XML package to parse the website and return the html from each page to a variable (running this will take a long time)
#Its important to note that this function relies on the webpages to parse to be in sequental order
library(XML)
raw_html_data <- lapply(1995:2014, function(season) htmlTreeParse(paste0('http://www.munsterrugby.ie/rugby/munster_first_team_comprehensive_fixtures.php?includeref=6177&season=',season,"-",season+1),useInternalNodes=T))
#Parse the html
munster <- do.call(rbind,lapply(raw_html_data, parseMunsterResults))
#Six Nations, http://www.rbs6nations.com
########################################
#Function to parse results from the six nations webpage
#Using Xpath and the XML library is one way but the htmltab package should eb more general
parseSixNationsResults <- function(url_html){
#Get HTML table containing the results
#On the rbs website the results are stored in a table with class list but there is a spanning column that contains
#the word round which we exclude
library(htmltab)
results <- htmltab(doc=url_html, which="//td[@class='list']", body="//tr[not(@class = 'group')]")
#Choose which data to keep
keep = c("Date", "Time (Local)", "Home", "Away", "Score", "Venue")
results <- results[-1,keep]
results$url <- docName(url_html)
row.names(results) <- NULL
return(results)
}

#Ireland
########
#Use the XML package to parse the website and return the html from each page to a variable (running this will take a long time)
#Its important to note that this function relies on the webpages to parse to be in sequental order
raw_html_data <- lapply(1882:2014, function(season) htmlTreeParse(paste0('http://www.rbs6nations.com/en/ireland/ireland_matchcentre.php?includeref=436&season=',season,"-",season+1),useInternalNodes=T))
#Parse the html
ireland <- do.call(rbind,lapply(raw_html_data, parseSixNationsResults))
#All Teams
##########
#Use the XML package to parse the website and return the html from each page to a variable (running this will take a long time)
#Its important to note that this function relies on the webpages to parse to be in sequental order
raw_html_data <- lapply(1882:2014, function(season) htmlTreeParse(paste0('http://www.rbs6nations.com/en/matchcentre/index.php?includeref=428&season=',season,"-",season+1),useInternalNodes=T))
#Parse the html
six_nations <- do.call(rbind,lapply(raw_html_data, parseSixNationsResults))

#Parse the html
daft <- do.call(rbind,lapply(raw_html_data, parseDaftPropertyPriceRegResults))


#Daft Rentals
##############
parseDaftRentalResults <- function(url_html){
  #Get HTML table containing the results
  daft_data <- NULL
  name = xpathSApply(url_html, "//h2", xmlValue)
  #There are some adds that appear as headings, remove these (valid adverts show number followed by full stop)
  name <- name[grepl("[0-9]\\.",name)]
  price = xpathSApply(url_html, "//strong[@class='price']", xmlValue)
  #Add a condition to check if the webpage contains any listings and exit if not
  if ( length(xpathSApply(url_html, "//strong[@class='price']",xmlValue)) == 0 ) {
    return(daft_data)
  }
  info = xpathSApply(url_html, "//ul[@class='info']", xmlValue)
  daft_data <- rbind(daft_data, data.frame(name, price, info))
  daft_data$url_html <- docName(url_html)
  return(daft_data)
}

#Loop through a number of webpages and scrape the html data
raw_html_data <- lapply(seq(0,2000,10), function(x) htmlTreeParse(paste0("http://www.daft.ie/dublin-city/houses-to-rent/?offset=",x), useInternalNodes=T))
#Parse the html
daft <- do.call(rbind,lapply(raw_html_data, parseDaftRentalResults))


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
