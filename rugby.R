#Munster Rugby, http://www.munsterrugby.ie
########################################
#Returns HTML which needs to be parsed and cleaned

#Return the html from each page to a variable (this will take a long time)
raw_html_data <- lapply(1995:2014, function(season) htmlTreeParse(paste0('http://www.munsterrugby.ie/rugby/munster_first_team_comprehensive_fixtures.php?includeref=6177&season=',season,"-",season+1),useInternalNodes=T))
#Parse the html
munster <- do.call(rbind,lapply(raw_html_data, parseMunsterResults))


#Six Nations, http://www.rbs6nations.com
########################################
#Returns HTML which needs to be parsed and cleaned

#Ireland
########
#Scrape the html from a website
raw_html_data <- lapply(1882:2014, function(season) htmlTreeParse(paste0('http://www.rbs6nations.com/en/ireland/ireland_matchcentre.php?includeref=436&season=',season,"-",season+1),useInternalNodes=T))
#Parse the html
ireland <- do.call(rbind,lapply(raw_html_data, parseSixNationsResults))

#All Teams
##########
#Scrape the html from a website
raw_html_data <- lapply(1882:2014, function(season) htmlTreeParse(paste0('http://www.rbs6nations.com/en/matchcentre/index.php?includeref=428&season=',season,"-",season+1),useInternalNodes=T))
#Parse the html
six_nations <- do.call(rbind,lapply(raw_html_data, parseSixNationsResults))
