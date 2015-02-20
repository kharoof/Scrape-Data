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
#Clean the results, for use on Six Nations Page
#Format dates, add team and opponent fields and pts diff win lose etc...
cleanResults <- function(data, team){
  data = cbind(data,data.frame(matrix(as.numeric(unlist(strsplit(data$Score, "-|v"))), ncol=2, byrow=T)))
  names(data) <- c("date", "time.local", "home.team", "away.team", "score", "venue","url", "home.score", "away.score")
  data$date = as.Date(data$date, "%d/%m/%Y")
  data$opponent <- ifelse(data$home.team==team, data$away.team, data$home.team)
  data$home.away <- ifelse(data$home.team==team, "Home", "Away")
  data$pts.diff <- ifelse(data$home.team==team, data$home.score - data$away.score,data$away.score - data$home.score)
  library(lubridate)
  data$year <- as.ordered(year(data$date))
  data$month <- as.ordered(month(data$date))
  data$win.lose <- as.factor(ifelse(data$pts.diff>0 , "Win",ifelse(data$pts.diff<0 , "Lose","Draw" ) ))
  data$team <- team
  keep = c("date", "year", "month","time.local", "team", "opponent", "pts.diff", "home.away", "win.lose", "url")
  data <- data[,keep]
  return(data)
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
