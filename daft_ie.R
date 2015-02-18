library(XML)


#Property Price Register data
#Returns HTML which needs to be parsed and cleaned


#Number of pages to loop through
num_pages <- getNumber(getLastLink("http://www.daft.ie/priceregister", "Last"))

#Return the html from each page to a variable (this will take a long time)
raw_html_data <- lapply(1:num_pages, function(x) htmlTreeParse(paste0("http://www.daft.ie/priceregister/?pagenum=",x),useInternalNodes=T))

              