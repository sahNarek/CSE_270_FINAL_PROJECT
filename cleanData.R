packages_list <- c("ggplot2", "dplyr", "stringr", "remotes")
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
#remotes::install_github("vapniks/mergeutils")
library("mergeutils")
load("data/ucl_po_1994_2019.rda")


et_games <- ucl_po_1994_2019_scrapped %>%
  filter(str_detect(RESULT, pattern = "[a-zA-Z]"))

get_first_games <- function(data = et_games, source = ucl_po_1994_2019_scrapped){
  result <- c()
  for(game in 1:nrow(data)){
    et_game <- data[game,]
    first_game <- source %>%
      filter(COMP == et_game$COMP, SEASON == et_game$SEASON, ROUND == et_game$ROUND,
             AWAYTEAM == et_game$HOMETEAM, HOMETEAM == et_game$AWAYTEAM)
    result <- rbind(result, first_game)
  }
  return(as.data.frame(result))
}

first_rounds <- get_first_games()
et_games <- rbind(et_games, first_rounds)


exclude_et_games <- function(data = ucl_po_1994_2019_scrapped, et = et_games ){
  data$ID = "ALL_GAMES"
  et$ID = "ET_GAMES"
  games <- rbind(data, et)
  dup_rows <- dupsBetweenGroups(games, "ID")
  games <- cbind(games, dup = dup_rows)
  games <- games %>%
    filter(dup == F) %>%
    select(-c(ID,dup))
  return(games)
}


games <- exclude_et_games()

a <- str_split("2:0 (0:0)", pattern = " ")
a[[1]][2]

str_extract_all(a[[1]][2], pattern = "[0-9, :]")[[1]]

get_scores_for_halfs <- function(score) {
  halfs <- str_split(score, pattern = " ")
  fh <- halfs[[1]][2]
  ft <- halfs[[1]][1]
  fh_goals <- str_extract_all(fh, pattern = "[0-9, :]")
  ft_goals <- str_extract_all(ft, pattern = "[0-9, :]")
  return( list(FTHG = as.numeric(ft_goals[[1]][1]),
              FTAG = as.numeric(ft_goals[[1]][3]),
              HTHG = as.numeric(fh_goals[[1]][1]),
              HTAG = as.numeric(fh_goals[[1]][3])))
}
a <-  get_scores_for_halfs("5:5 (4:2)")

games <- games %>%
  rowwise() %>%
  mutate(FTHG = get_scores_for_halfs(RESULT)$FTHG, 
         FTAG = get_scores_for_halfs(RESULT)$FTAG,
         HTHG = get_scores_for_halfs(RESULT)$HTHG, 
         HTAG = get_scores_for_halfs(RESULT)$HTAG) %>%
  select(COMP, SEASON, ROUND, LEG, DATE, HOMETEAM, AWAYTEAM, 
         FTHG, FTAG, HTHG, HTAG)

str(games)

get_away_goal_games <- function(data = games) {
  result <- c()
  for(i in seq(from = 1, to = nrow(data), by = 2)){
    f_leg <- data[i,]
    s_leg <- data[i+1,]
    if(f_leg$FTHG + s_leg$FTAG == f_leg$FTAG + s_leg$FTHG){
      result <- rbind(result, f_leg, s_leg)
    }
  }
  return(result)
}

aways <- get_away_goal_games()
