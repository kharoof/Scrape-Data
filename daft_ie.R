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
