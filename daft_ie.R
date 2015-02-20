library(XML)


#Property Price Register data
#Returns HTML which needs to be parsed and cleaned


source("functions.R")

#Number of pages to loop through
num_pages <- getNumber(getLastLink("http://www.daft.ie/priceregister", "Last"))

#Return the html from each page to a variable (this will take a long time)
raw_html_data <- lapply(1:num_pages, function(x) htmlTreeParse(paste0("http://www.daft.ie/priceregister/?pagenum=",x),useInternalNodes=T))


parseDaftResults <- function(url_html){
  #Get HTML table containing the results
  daft_data <- NULL
address = xpathSApply(url_html, "//span[@class='priceregister-address']", xmlValue)
details = xpathSApply(url_html, "//span[@class='priceregister-dwelling-details']", xmlValue)
daft_data <- rbind(daft_data, data.frame(address,details))
daft_data$url_html <- docName(url_html)
return(daft_data)
}

#Parse the html
daft <- do.call(rbind,lapply(raw_html_data, parseDaftResults))
