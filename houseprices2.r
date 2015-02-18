library("XML")

#Get the data
property <- data.frame()
for (i in 1:2978) {
#for (i in 1:2) {
  filename <- paste0("http://www.daft.ie/priceregister/?pagenum=",i)
  html <- htmlTreeParse(filename, useInternalNodes = T)
address = xpathSApply(html, "//span[@class='priceregister-address']", xmlValue)
details = xpathSApply(html, "//span[@class='priceregister-dwelling-details']", xmlValue)
property <- rbind(property, data.frame(address,details))
}
property[,1] <- as.character(property[,1])
property[,2] <- as.character(property[,2])

