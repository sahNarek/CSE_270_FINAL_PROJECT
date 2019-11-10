library(rvest)
library(curl)
library(dplyr)
library(magrittr)
library(stringr)

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
ucl_games <- "https://www.worldfootball.net/schedule/champions-league-"
seasons1 <- get_seasons(2007, 2018)
seasons2 <- get_seasons(1994, 2006)

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

final_url <- "https://www.worldfootball.net/schedule/champions-league-2008-2009-finale_2/"

data_16 <- get_ucl_data(seasons = seasons1, round = rounds$R16, comp = "UCL")
data_8 <- get_ucl_data(seasons = seasons1, round = rounds$R8, comp = "UCL")
data_4 <- get_ucl_data(seasons = seasons1, round = rounds$R4, comp = "UCL")
data_2 <- get_ucl_data(seasons = seasons1, round = rounds$RF, comp = "UCL")

#The structure of the url for 2008-2009 final game was a little different
#to keep the harmony let's handle this case seperately

add_missing_final <- function(data, final_url){
  data <- data %>%
    filter(!(HOMETEAM == ""), !(AWAYTEAM == ""), 
           !(HOMETEAM == "-"), !(AWAYTEAM == "-"))
  final_2008_2009 <- get_df(final_url)
  final_2008_2009 <- clear_one_game(data = final_2008_2009, season = "2008-2009", round = "finale",
                                    comp = "UCL", leg = "FINAL")
  data <- rbind(data, final_2008_2009) 
  data <- data[c(1,12,3:11),]
  rownames(data) <- 1:nrow(data)
  return(data)
}

data_2 <- add_missing_final(data = data_2, final_url = final_url)
ucl_po_2008_2019_scrapped <- rbind(data_16, data_8, data_4, data_2)


data_8_2 <- get_ucl_data(seasons = seasons2, round = rounds$R8, comp = "UCL")
data_4_2 <- get_ucl_data(seasons = seasons2, round = rounds$R4, comp = "UCL")
data_2_2 <- get_ucl_data(seasons = seasons2, round = rounds$RF1, comp = "UCL")

ucl_po_1994_2007_scrapped <- rbind(data_8_2, data_4_2, data_2_2)

ucl_po_1994_2019_scrapped <- rbind(ucl_po_2008_2019_scrapped, 
                                   ucl_po_1994_2007_scrapped)

save(ucl_po_1994_2019_scrapped, file = "ucl_po_1994_2019.rda")
saveRDS(ucl_po_1994_2019_scrapped, file = "ucl_po_1994_2019.rds")
write.csv(ucl_po_1994_2019_scrapped, file = "ucl_po_1994_2019.csv")
