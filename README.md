# Scrape-Data
This repo contains some basic examples showing how to scrape data from the Web using R

R is an ideal solution for scraping small amounts of data from the web. If you wish to analyse football results or house prices for example you need data in order to perform the statistical analysis. R is not the quickest or most functional way to scrape data from the web (for advanced solutions see python) but its easy to use and does the job!

You will always encounter a trade off between spending time to scrape data and just getting some quick numbers for your analysis. Sometimes its easiest and quickest to just copy and paste data from a webpage in order to get the data that you need but for any scraping that involves more than a few copy and pastes R might be just what your looking for. Before embarking on developing a website scraping script always check if the website has the data available as a csv download. A number of sites provide this option.

There are several packages and methods available to scrape data from the web. This repository contains tailored functions for several websites. Examples are shown that make use of the packages XML and RCurl. Depending on the webpage your scraping you might need to use a different method. 

XML and RCurl are two packages that allow you to scrape raw data from the web. You basically provide a webpage there are functions that then retrieve the raw html from the link.

XML also provides a function xpathSApply which parses the html and extracts data. You can specify the structure of the data to extract. For example you can tell it to extract all the data contained in \<td\> tags. This is easiest to understand with some examples.

There is also a very clever package htmltab which parses data (similar to xpathSApply) and automatically finds tables in the html and returns the table as a data.table. Most of the time when your extracting data this is exactly what you need.  
