packages_list <- c("ggplot2", "dplyr", "stringr")

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
load("data/ucl_po_1994_2018.rda")


get_first_games <- function(data = et_games, source = ucl_po_94_18){
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

get_score <- function(string){
  return(str_extract_all(string, pattern = "[0-9, :]"))
}

get_ha_goals <- function(score){
  score <- score[[1]]
  hg <- score[1]
  ag <- score[3]
  return(list(HG = as.numeric(hg),
              AG = as.numeric(ag)))
}

get_scores_for_halfs <- function(score) {
  halfs <- str_split(score, pattern = " ")
  ht <- halfs[[1]][2]
  ft <- halfs[[1]][1]
  
  ht_score <- get_score(ht)
  ft_score <- get_score(ft)
  
  ht_goals <- get_ha_goals(ht_score)
  ft_goals <- get_ha_goals(ft_score)
  
  return(data.frame(FTHG = ft_goals$HG, FTAG = ft_goals$AG,
              HTHG = ht_goals$HG, HTAG = ht_goals$AG,
              ETHG = NA, ETAG = NA, PTHG = NA, PTAG = NA))
}

get_et_score <- function(text){
  text1 <- str_remove_all(text, pattern = "[a-z]")
  text2 <- str_split(text1, pattern = " ")
  ht_r <- get_score(text2[[1]][2]) 
  ft_r <- get_score(text2[[1]][3])
  et_r <- get_score(text2[[1]][1])
  
  ht_goals <- get_ha_goals(ht_r)
  ft_goals <- get_ha_goals(ft_r)
  et_goals <- get_ha_goals(et_r)
  
  et_goals$HG <- et_goals$HG - ft_goals$HG
  et_goals$AG <- et_goals$AG - ft_goals$AG
  
  a <- data.frame(FTHG = ft_goals$HG, FTAG = ft_goals$AG , HTHG = ht_goals$HG, HTAG = ht_goals$AG,
                  ETHG = et_goals$HG, ETAG = et_goals$HG, PTHG = NA, PTAG = NA)
  return(a)
}

get_penalties_score <- function(text){
  text1 <- str_remove_all(text, pattern = "[a-z]")
  text2 <- str_split(text1, pattern = " ")
  ht_r <- get_score(text2[[1]][2]) 
  ft_r <- get_score(text2[[1]][3])
  et_r <- get_score(text2[[1]][4])
  pn_r <- get_score(text2[[1]][1])
  
  ht_goals <- get_ha_goals(ht_r)
  ft_goals <- get_ha_goals(ft_r)
  et_goals <- get_ha_goals(et_r)
  pn_goals <- get_ha_goals(pn_r)
  
  et_goals$HG <- et_goals$HG - ft_goals$HG
  et_goals$AG <- et_goals$AG - ft_goals$AG
  
  a <- data.frame(FTHG = ft_goals$HG, FTAG = ft_goals$AG , HTHG = ht_goals$HG, HTAG = ht_goals$AG,
                  ETHG = et_goals$HG, ETAG = et_goals$HG, PTHG = pn_goals$HG, PTAG = pn_goals$AG)
  return(a)
}

get_away_goal_games <- function(data = games) {
  result <- c()
  rows <- seq(from = 1, to = nrow(data), by = 2)
  for(i in rows){
    f_leg <- data[i,]
    s_leg <- data[i+1,]
    if(f_leg$FTHG + s_leg$FTAG == f_leg$FTAG + s_leg$FTHG){
      result <- rbind(result, f_leg, s_leg)
    }
  }
  return(result)
}

get_correct_goals <- function(score) {
  numbers <- str_remove_all(score, pattern = "[a-z]")
  scores <- str_split(numbers, pattern = " ")
  len <- length(scores[[1]])
  result <- c()
  
  if(len == 5){
    result <- get_penalties_score(scores)
  }
  else if(len == 4){
    result <- get_et_score(scores)
  }
  else{
    result <- get_scores_for_halfs(scores)
  }
  return(result)
}

get_right_date <- function(leg, date){
  leg_string <- paste(leg,": ",sep = "")
  date_nl <- sub(x = date, pattern = leg_string, replacement = "", fixed = T )
  
  real_date <- str_split(date_nl , pattern = " ")[[1]][1]
  return(as.Date(real_date, format = "%d/%m/%Y"))
}

clean_scores <- function(data){
  return(data %>%
    rowwise() %>%
    mutate(FTHG = get_correct_goals(RESULT)$FTHG, 
           FTAG = get_correct_goals(RESULT)$FTAG,
           HTHG = get_correct_goals(RESULT)$HTHG, 
           HTAG = get_correct_goals(RESULT)$HTAG,
           ETHG = get_correct_goals(RESULT)$ETHG,
           ETAG = get_correct_goals(RESULT)$ETAG,
           PTHG = get_correct_goals(RESULT)$PTHG,
           PTAG = get_correct_goals(RESULT)$PTAG) %>%
    select(COMP, SEASON, ROUND, LEG, DATE, HOMETEAM, AWAYTEAM, 
           FTHG, FTAG, HTHG, HTAG, ETHG, ETAG, PTHG, PTAG))
}

get_no_et_games <- function(data = et_games){
  first_rounds <- get_first_games(data = et_games)
  et_games_1 <- rbind(data, first_rounds) 
  games <- ucl_po_94_18 %>%
    anti_join(et_games_1)
  games <- clean_scores(games)
  
  games <- games %>%
    mutate(DATE = get_right_date(LEG, DATE))
  games$LEG <- factor(games$LEG, levels = unique(games$LEG), labels = c("1", "2", "F"))
  
  final_games <- games %>%
    filter(LEG == "F")
  games <- games %>%
    anti_join(final_games)
  return(games)
}

get_et_games <- function(data = ucl_po_94_18) {
  et_games <- data %>%
    filter(str_detect(RESULT, pattern = "[a-zA-Z]"))
  
  et_games <- et_games %>%
    filter(!(SEASON == "2004-2005" & AWAYTEAM == "AC Milan" 
             & HOMETEAM == "Inter" & ROUND == "viertelfinale"))
  et_games <- clean_scores(et_games)
  et_games$LEG <- factor(et_games$LEG, levels = unique(et_games$LEG), labels = c("2", "F"))
  return(et_games)
}

get_et_def <- function(data = ucl_po_94_18){
  et_games <- data %>%
    filter(str_detect(RESULT, pattern = "[a-zA-Z]"))
  
  et_games <- et_games %>%
    filter(!(SEASON == "2004-2005" & AWAYTEAM == "AC Milan" 
             & HOMETEAM == "Inter" & ROUND == "viertelfinale"))
  return(et_games)
}


et_games <- get_et_games(data = ucl_po_94_18)
et_fr <- get_first_games(data = et_games, source = ucl_po_94_18)
et_fr <- clean_scores(et_fr)

et_def <- get_et_def()
games <- get_no_et_games(data = et_def)

aways <- get_away_goal_games(data = games)

str(aways)


et_aw_games <- rbind(et_games, et_fr)


f_leg <- aways %>%
  filter(LEG == "1")

s_leg <- aways %>%
  filter(LEG == "2")

identical(f_leg$HOMETEAM, s_leg$AWAYTEAM)

save(away_et, file = "data/et_games_94_18.rda")
