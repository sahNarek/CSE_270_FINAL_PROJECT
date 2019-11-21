packages_list <- c("rvest","curl","dplyr","magrittr","stringr")
install_or_call <- function(list = packages_list){
  installed <- installed.packages()[,"Package"]
  for( package in packages_list ){
    if(!(package %in% installed)){
      install.packages(package)
    }
    do.call(library, list(package))
  }
}
install_or_call()

load("../data/et_agr_94_18.rda")

url <- "https://www.worldfootball.net/report/champions-league"

get_seasons <- function(start, end){
  seasons <- c()
  for(year in start:end){
    season <- paste(year, year+1, sep = "-")
    seasons <- c(seasons, season)
  }
  return(seasons)
}

seas_07_18 <- get_seasons(2007, 2018)
seas_94_06 <- get_seasons(1994, 2006)

et_f <- et_agr %>%
  filter((LEG == "2" && TYPE == "ET" ) | (LEG == "F"))

goals_from_score <- function(result) {
  result <- str_split(result , pattern = " : ")[[1]]
  hg <- result[1]
  ag <- result[2]
  return(list(HG = as.numeric(hg), AG = as.numeric(ag)))
}

get_minute <- function(text){
  minute <- str_extract(text,"\\(?[0-9]+\\)?")
  minute <- as.numeric(minute)
  return(minute)
}

who_scored <- function(data){
  scoring_teams <- c()
  for(i in 1:nrow(data)){
    event <- data[i,]
    if(i == 1){
      team <- ifelse(event$HG == 0, event$AWAYTEAM, event$HOMETEAM)
    }
    else{
      prev <- data[i-1,]
      hgs <- event$HG - prev$HG
      ags <- event$AG - prev$AG
      team <- ifelse(hgs == 0, event$AWAYTEAM, event$HOMETEAM)
    }
    scoring_teams <- rbind(scoring_teams, team)
  }
  return(scoring_teams)
}

get_round <- function(round, finale = T){
  result <- c()
  if(round == "R16"){
    result = "achtelfinale"
  }
  else if(round == "R8"){
    result = "viertelfinale"
  }
  else if(round == "R4"){
    result = "halbfinale"
  }
  else if(round == "RF" && !finale){
    result = "endspiel"
  }
  else {
    result = "finale"
  }
  return(result)
}

get_game_url <- function(hteam, ateam, season, round, url = url) {
  
  hlower <- tolower(hteam)
  hurl <- str_replace_all(hlower, pattern = " ", replacement = "-")
  hurl <- iconv(hurl, to='ASCII//TRANSLIT')
  
  if(hteam == "Bayern München"){
    hurl <- "bayern-muenchen"  
  }
  
  alower <- tolower(ateam)
  aurl <- str_replace_all(alower, pattern = " ", replacement = "-")
  aurl <- iconv(aurl, to='ASCII//TRANSLIT')
  
  if(ateam == "Bayern München"){
    aurl <- "bayern-muenchen"  
  }
  
  f_round <- T
  if(season %in% seas_94_06){
    f_round <- F
  }
  round_url <- get_round(round, finale = f_round)
  game_url <- paste(url, season, round_url, hurl, aurl, sep = "-")
  return(game_url)
}

scrap_game <- function(url){
  result = tryCatch({
    table <- read_html(url) %>%
      html_node(css = ".e4~ .standard_tabelle") %>%
      html_table()
    table <- as.data.frame(table)
    table <- table[2:nrow(table),]
    return(table)
  }, warning = function(cond) {
    message(paste("URL caused a warning:", url))
    message("Here's the original warning message:")
    message(cond)
    # Choose a return value in case of warning
    return(NULL)
  }, error = function(cond) {
    message(paste("URL does not seem to exist:", url))
    message("Here's the original error message:")
    message(cond)
    return(NULL)
  }, finally={
    message(paste("Processed URL:", url))
  })
  return(result)
}

ft_goals <- function(url, hteam, ateam, leg_id){
  table <- scrap_game(url)
  if(is.data.frame(table)){
    table$HOMETEAM <- hteam
    table$AWAYTEAM <- ateam
    table <- table %>%
      rowwise() %>%
      mutate(HG = goals_from_score(X1)$HG,
             AG = goals_from_score(X1)$AG,
             MINUTE = get_minute(X2))
    ws <- who_scored(table)
    table$WS <- ws
    table <- table %>%
      select(HOMETEAM, AWAYTEAM, HG, AG, MINUTE, WS)
    table$LEG_ID <- leg_id
    return(table)
  }
}

game <- et_f[1,]



get_et_minutes <- function(data) {
  result <- c()
  for(i in 1:nrow(data)){
    game <- data[i,]
    game_url <- get_game_url(hteam = game$HOMETEAM, ateam = game$AWAYTEAM, 
                             season = game$SEASON, round = game$ROUND, url = url)
    game_data <- ft_goals(url = game_url, hteam = game$HOMETEAM, 
                          ateam = game$AWAYTEAM,leg_id = game$LEG_ID)
    result <- rbind(result, game_data)
  }
  return(result)
}

et_minutes <- get_et_minutes(et_f)

length(unique(et_minutes$LEG_ID))

problems <- et_f %>%
  filter(!(LEG_ID %in% unique(et_minutes$LEG_ID))) %>%
  rowwise() %>%
  mutate(url = get_game_url(HOMETEAM, AWAYTEAM, SEASON, ROUND, url = url))


fix_problems <- function(data) {
  result <- data
  result[1,]$url <- paste(result[1,]$url,"sk",sep = "-") 
  for(i in 2:nrow(data)){
    result[i,]$url <- str_replace_all(result[i,]$url, 
                                    pattern = fixed("endspiel"), replacement = "finale")
  }
  return(result)
}

problems <- fix_problems(data = problems)

get_missing_data <- function(data = problems){
  result <- c()
  for(i in 1:nrow(data)){
    game <- data[i,]
    game_url <- game$url
    game_data <- ft_goals(url = game_url, hteam = game$HOMETEAM, 
                          ateam = game$AWAYTEAM,leg_id = game$LEG_ID)
    result <- rbind(result, game_data)
  }
  return(result)
}

missing <- get_missing_data()

et_minutes <- rbind(et_minutes, missing)

et_minutes <- et_minutes %>% 
  filter(!(is.na(WS)))

save(et_minutes, file =  "../data/et_minutes.rda")
write.csv(et_minutes, file =  "../data/et_minutes.csv")
