library(XML)
library(RCurl)


url.list <- c( 19264,11870,11864,12506,12503,12664,
              12703,18246,18412,173,198,180,225,165,
              175,201,214,199,212,121,213,289,168,160,
              260,285,162,182,122,140,263,
              242,275,290,193,245,247,211,278,
              196,231,176,305,167,181,236
)


match_data <- NULL

for (url in url.list) {

url <- paste0("http://www.munsterrugby.ie/rugby/", url)
  
html3 <- htmlTreeParse(url, useInternalNodes = T)


#Player Data
player_number <- xpathSApply(html3, "//td[@class='pos posstart']", xmlValue)
sub_number <- xpathSApply(html3, "//td[@class='pos possub']", xmlValue)
player_number <- c(player_number[1:15],sub_number[1:8], player_number[16:30], sub_number[9:16])

player_name <- xpathSApply(html3, "//td[@class='name namestart']", xmlValue)
sub_name <- xpathSApply(html3, "//td[@class='name namesub']", xmlValue)
player_name <- c(player_name[1:15],sub_name[1:8], player_name[16:30], sub_name[9:16])

player_try <- xpathSApply(html3, "//td[@class='score scoretry scorestart scoretrystart']", xmlValue)
sub_try <- xpathSApply(html3, "//td[@class='score scoretry scoresub scoretrysub']", xmlValue)
player_try <- c(player_try[1:15],sub_try[1:8], player_try[16:30], sub_try[9:16])
player_try <- as.numeric(gsub("T", "", player_try))
player_try[is.na(player_try)] <- 0

player_conv <- xpathSApply(html3, "//td[@class='score scoreconv scorestart scoreconvstart']", xmlValue)
sub_conv <- xpathSApply(html3, "//td[@class='score scoreconv scoresub scoreconvsub']", xmlValue)
player_conv <- c(player_conv[1:15],sub_conv[1:8], player_conv[16:30], sub_conv[9:16])
player_conv <- as.numeric(gsub("C", "", player_conv))
player_conv[is.na(player_conv)] <- 0

player_penalty <- xpathSApply(html3, "//td[@class='score scorepen socrestart scorepenstart']", xmlValue)
sub_penalty <- xpathSApply(html3, "//td[@class='score scorepen scoresub scorepensub']", xmlValue)
player_penalty <- c(player_penalty[1:15],sub_penalty[1:8], player_penalty[16:30], sub_penalty[9:16])
player_penalty <- as.numeric(gsub("P", "", player_penalty))
player_penalty[is.na(player_penalty)] <- 0

player_drop <- xpathSApply(html3, "//td[@class='score scoredrop scorestart scoredropstart']", xmlValue)
sub_drop <- xpathSApply(html3, "//td[@class='score scoredrop scoresub scoredropsub']", xmlValue)
player_drop <- c(player_drop[1:15],sub_drop[1:8], player_drop[16:30], sub_drop[9:16])
player_drop <- as.numeric(gsub("D", "", player_drop))
player_drop[is.na(player_drop)] <- 0

player_data <- data.frame(player_number, player_name, player_try, player_conv, player_drop, player_penalty, stringsAsFactors=F)

final_score <- xpathSApply(html3, "//div[@class='score']", xmlValue)[1]
half_time_score <- xpathSApply(html3, "//div[@class='hidden halftime']", xmlValue)
home_team <- xpathSApply(html3, "//div[@class='home']", xmlValue)[1]
away_team <- xpathSApply(html3, "//div[@class='away']", xmlValue)[1]



#Match Data
final_score <- xpathSApply(html3, "//div[@class='score']", xmlValue)[1]
final_score <- gsub("\t|\n|\r", "", final_score)
final_score <- as.numeric(unlist(strsplit(final_score,"- ")))
home_ft_score <- final_score[1]
away_ft_score <- final_score[2]

half_time_score <- xpathSApply(html3, "//div[@class='hidden halftime']", xmlValue)
half_time_score <- gsub("\t|\n|\r|HT:", "", half_time_score)
half_time_score <- as.numeric(unlist(strsplit(half_time_score,"- ")))
home_ht_score <- half_time_score[1]
away_ht_score <- half_time_score[2]

home_team <- xpathSApply(html3, "//div[@class='home']", xmlValue)[1]
home_team <- gsub("\n|\t", "", home_team)
away_team <- xpathSApply(html3, "//div[@class='away']", xmlValue)[1]
away_team <- gsub("\n|\t", "", away_team)
date <- xpathSApply(html3, "//div[@class='date']", xmlValue)
date <- gsub("\n|\t|th|rd|st|nd", "", date)
date <- xpathSApply(html3, "//div[@class='date']", xmlValue)
time <- xpathSApply(html3, "//div[@class='time']", xmlValue)
time <- gsub("\n|\t", "", time)
date <- paste(date, time)
date <- as.Date(date, "%d %B %Y %H:%M")

date <- xpathSApply(html3, "//div[@class='date']", xmlValue)
time <- xpathSApply(html3, "//div[@class='time']", xmlValue)
venue <- xpathSApply(html3, "//div[@class='venue']", xmlValue)[1]
venue <- gsub("\n|\t", "", venue)
#HT Score, Ref Name, attendance
date <- xpathSApply(html3, "//div[@class='date']", xmlValue)
time <- xpathSApply(html3, "//div[@class='time']", xmlValue)
venue <- xpathSApply(html3, "//div[@class='venue']", xmlValue)[1]
other <- xpathSApply(html3, "//span[@class='name']", xmlValue)
ref_name <- other[3]
attendance <- as.numeric(gsub(",", "", other[2]))
home_away <- ifelse(home_team=="Munster Rugby", "H", "A")
pts_diff <- ifelse(home_team=="Munster Rugby", home_ft_score - away_ft_score, away_ft_score - home_ft_score)
win_lose_draw <- ifelse(pts_diff > 0 , "W", ifelse(pts_diff==0, "D", "L"))
win_lose <- ifelse(pts_diff > 0 , "W", ifelse(pts_diff==0, NA, "L"))
library(lubridate)
season <- ifelse(month(date) >8, paste(year(date), year(date)+1, sep="-"), paste(year(date)-1, year(date), sep="-"))


home_team_data <- player_data[1:23,]
away_team_data <- player_data[24:43,]

library(plyr)
H <- ddply(home_team_data, .(), summarise, tries = sum(player_try), conv=sum(player_conv), pen=sum(player_penalty), drop=sum(player_drop))[,2:5]
names(H) <- paste0("home_team_", names(H))
A <- ddply(away_team_data, .(), summarise, tries = sum(player_try), conv=sum(player_conv), pen=sum(player_penalty), drop=sum(player_drop))[,2:5]
names(A) <- paste0("away_team_", names(A))

match_data_tmp <- data.frame(date, venue, ref_name, attendance,
                         home_team, home_ft_score, home_ht_score,
                         away_team, away_ft_score, away_ht_score,
                         home_away, pts_diff, win_lose, win_lose_draw, A, H, url, season,
                         stringsAsFactors=F
                         )

player_data$team <- c(rep(home_team, 23), rep(away_team, 23))


match_data <- rbind(match_data, match_data_tmp)

}


write.csv(match_data, file="./munster_european_cup.csv", row.names=F)
