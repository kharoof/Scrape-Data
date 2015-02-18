library("XML")


#Get Number of Pages agailable on the Daft website
html = htmlTreeParse("http://www.daft.ie/priceregister/", useInternalNodes=T)

last.link.location <- grep("Last",xpathSApply(html, "//a",xmlValue))

last.link.path <- unlist(xpathSApply(html, "//a",xmlGetAttr, "href")[last.link.location])

library(stringr)

#Extract number from the link
last.link.number <- as.numeric(str_extract(last.link.path, "[0-9]+"))

#Loop throught all the links and extract the data (this will take some time)
data <- lapply(1:last.link.number, function(x) htmlTreeParse(paste0("http://www.daft.ie/priceregister/?pagenum=",x),useInternalNodes=T))

              