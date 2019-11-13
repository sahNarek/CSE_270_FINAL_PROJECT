library(rvest)
library(curl)
library(dplyr)
library(magrittr)
library(stringr)

ucl_games <- "https://www.worldfootball.net/schedule/champions-league-"

game_dates <- function(df_clean){
  dates <- c()
  for(index in 1:nrow(df_clean)){
    if(df_clean[index,]$LEG == "1st(1st leg)"){
      date_string <- df_clean[index+2,]$HOMETEAM
      date <- df_clean[index+2,]$HOMETEAM
    }
    else if(df_clean[index,]$LEG == "2nd (2nd leg)"){
      date <- df_clean[index+1,]$AWAYTEAM
    }
    else{
      date <- NA
    }
    dates <- c(dates, date)
  }
  return(dates)
}

clean_data <- function(data, season, round, comp) {
  df_clean <- data.frame(COMP = comp, SEASON = season, ROUND = round,
                         LEG = data$X1, HOMETEAM = data$X2,
                         AWAYTEAM = data$X4,
                         RESULT = data$X5, stringsAsFactors = F )
  dates <- game_dates(df_clean)
  df_clean <- df_clean %>%
    filter(!(LEG == ""))
  dates <- dates[!is.na(dates)]
  df_clean$DATE <- dates
  return(df_clean)
}

get_seasons <- function(start, end){
  seasons <- c()
  for(year in start:end){
    season <- paste(year, year+1, sep = "-")
    seasons <- c(seasons, season)
  }
  return(seasons)
}
rounds <- list(R16 = "achtelfinale",
               R8 = "viertelfinale",
               R4 = "halbfinale",
               RF = "finale",
               RF1 = "endspiel")

get_df <- function(url){
  table <- read_html(url) %>%
    html_node(css = ".standard_tabelle") %>%
    html_table(header = FALSE)
  return(as.data.frame(table))
}


clear_one_game <- function(data, season, round, comp, leg) {
  result <- data.frame(COMP = comp, SEASON = season, ROUND = round, LEG = leg,
                       HOMETEAM = data$X3, AWAYTEAM = data$X5, RESULT = data$X6,
                       DATE = paste(data$X1,data$X2, sep = ": "))
  return(result)
}


get_ucl_data <- function(seasons, round, game_url = ucl_games, comp) {
  result <- c()
  for(season in seasons){
    ucl_data <- paste(game_url, season, "-", round, sep = "")
    if(season == "2008-2009" && round != "achtelfinale"){
      ucl_data <- paste(ucl_data, "2", sep = "_")
      print("the url")
      print(ucl_data)
    }
    round_data <- get_df(ucl_data)
    if(round == "finale" | round == "endspiel"){
      data_cleaned <- clear_one_game(data = round_data, season = season,
                                     round = round,comp = comp,leg = "FINAL")
      result <- rbind(result, data_cleaned)
    }
    else{
      data_cleaned <- clean_data(round_data, season = season, round = round, comp = comp)
      result <- rbind(result, data_cleaned)
    }
  }
  return(result)
}



get_all_games <- function(){
  seas_07_18 <- get_seasons(2007, 2018)
  seas_94_02 <- get_seasons(1994, 2002)
  seas_02_06 <- get_seasons(2003, 2006)
  
  data_16 <- get_ucl_data(seasons = seas_07_18, round = rounds$R16, comp = "UCL")
  data_8 <- get_ucl_data(seasons = seas_07_18, round = rounds$R8, comp = "UCL")
  data_4 <- get_ucl_data(seasons = seas_07_18, round = rounds$R4, comp = "UCL")
  data_2 <- get_ucl_data(seasons = seas_07_18, round = rounds$RF, comp = "UCL")
  
  ucl_07_18 <- rbind(data_16 ,data_8 ,data_4, data_2)
  
  data_16_2 <- get_ucl_data(seasons = seas_02_06, round = rounds$R16, comp = "UCL")
  data_8_2 <- get_ucl_data(seasons = seas_02_06, round = rounds$R8, comp = "UCL")
  data_4_2 <- get_ucl_data(seasons = seas_02_06, round = rounds$R4, comp = "UCL")
  data_16_2 <- get_ucl_data(seasons = seas_02_06, round = rounds$RF1, comp = "UCL")
  
  ucl_02_06 <- rbind(data_16_2, data_8_2, data_4_2, data_2_2)
  
  data_8_3 <- get_ucl_data(seasons = seas_94_02, round = rounds$R8, comp = "UCL")
  data_4_3 <- get_ucl_data(seasons = seas_94_02, round = rounds$R4, comp = "UCL")
  data_2_3 <- get_ucl_data(seasons = seas_94_02, round = rounds$RF1, comp = "UCL")
  
  ucl_94_02 <- rbind(data_8_3, data_4_3, data_2_3)
  
  ucl_94_18 <- rbind(ucl_94_02, ucl_02_06, ucl_07_18)
  
  return(ucl_94_18)
}


ucl_po_94_18 <- get_all_games()

save(ucl_po_94_18, file = "ucl_po_1994_2018.rda")
saveRDS(ucl_po_94_18, file = "ucl_po_1994_2018.rds")
write.csv(ucl_po_94_18, file = "ucl_po_1994_2018.csv")
