
##Convert first letter of each word to upper case
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
{s <- substring(s, 2); if(strict) tolower(s) else s},
sep = "", collapse = " " )
sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}




#A useful method of getting all the links on a page is to look for the last navigation option

#' Query a base webpage to find the last link

getLastLink <- function(base_url="http://www.daft.ie/priceregister", last_text="Last") {
  library("XML")
  #Get the html from the base site
  html = htmlTreeParse(base_url, useInternalNodes=T)
  
  #Get the links from the page
  links_text <- xpathSApply(html, "//a",xmlValue)
  links_url <- xpathSApply(html, "//a",xmlGetAttr, "href")
  
  #Get the last link
  last_link_url <- links_url[grep(last_text,links_text)]
  
  return(paste0(base_url,last_link_url))
  
}

#Utility function to extract a number from a text string

getNumber <- function(text) {
  library(stringr)
  return(as.numeric(str_extract(text, "[0-9]+")))
}

#Utility function to extract a base url assuming it ends in a number

getPath <- function(text) {
  library(stringr)
  return(str_extract(text, "[^0-9]+"))
}


