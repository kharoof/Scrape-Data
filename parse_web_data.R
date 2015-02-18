#Function to parse results from a webpage
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



parseMunsterResults <- function(url_html){
  #Get HTML table containing the results
  #On the rbs website the results are stored in a table with class list but there is a spanning column that contains
  #the word round which we exclude
  library(htmltab)
  #Player Data
  #home_player_data <- htmltab(doc=url_html, which=1)
  #away_player_data <- htmltab(doc=url_html, which=2)
  #Match Data
  final_score <- xpathSApply(url_html, "//div[@class='score']", xmlValue)[1]
  half_time_score <- xpathSApply(url_html, "//div[@class='hidden halftime']", xmlValue)
  home_team <- xpathSApply(url_html, "//div[@class='home']", xmlValue)[1]
  away_team <- xpathSApply(url_html, "//div[@class='away']", xmlValue)[1]
  date <- xpathSApply(url_html, "//div[@class='date']", xmlValue)
  time <- xpathSApply(url_html, "//div[@class='time']", xmlValue)
  venue <- xpathSApply(url_html, "//div[@class='venue']", xmlValue)[1]
  other <- xpathSApply(url_html, "//span[@class='name']", xmlValue)
  match.data <- data.frame(c(final_score,half_time_score,home_team,away_team,date,time,venue,other,docName(url_html)))
  return(match.data)
}

