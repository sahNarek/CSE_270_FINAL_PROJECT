packages_list <- c("ggplot2", "dplyr", "stringr","hablar")
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
                  ETHG = et_goals$HG, ETAG = et_goals$AG, PTHG = NA, PTAG = NA)
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

combine_legs <- function(f_leg, s_leg, st_id, type){
  result <- c()
  for(i in 1:nrow(f_leg)){
    f_game <- f_leg[i,]
    s_game <- s_leg[i,]
    
    leg_id <- paste(type, st_id, sep = "")
    
    f_game$LEG_ID <- leg_id
    s_game$LEG_ID <- leg_id
    
    f_game$TYPE <- type
    s_game$TYPE <- type
    
    result <- rbind(result, f_game, s_game)
    st_id <- st_id + 1
  }
  return(result)
}

finals_leg_ids <- function(final_games, st_id, type = "F"){
  result <- c()
  for(i in 1:nrow(final_games)){
    game <- final_games[i,]
    leg_id <- paste(type,st_id, sep = "")
    game$LEG_ID <- leg_id
    game$TYPE <- type
    result <- rbind(result, game)
    st_id <- st_id + 1
  }
  return(result)
}

legs_winners <- function(data) {
  result <- c()
  et_away <- data %>%
    filter(LEG != "F")
  seq1 <- seq(from = 1, to = nrow(et_away)-1, by = 2)
  for(i in seq1){
    game1 <- data[i,]
    game2 <- data[i+1,]
    
    type <- game1$TYPE
    leg_id <- game1$LEG_ID

    team1 <- game1$HOMETEAM
    team2 <- game1$AWAYTEAM

    t1_hg <- game1$FTHG
    t2_hg <- game2$FTHG + game2$ETHG 
    
    t1_ag <- game2$FTAG + game2$ETAG
    t2_ag <- game1$FTAG
    
    t1_g <- t1_hg + t1_ag
    t2_g <- t2_hg + t2_ag
    
    if(type == "AGR"){
      winner <- ifelse(t1_ag > t2_ag, team1, team2)
    }
    else if (type == "ET"){
      t1_g = t1_g + game2$PTAG
      t2_g = t2_g + game2$PTHG
      winner <- ifelse(t1_g > t2_g, team1, team2)
    }
    else if(type == "G"){
      winner <- ifelse(t1_g > t2_g, team1, team2)
    }
    leg1_date <- game1$DATE
    leg2_date <- game2$DATE
    
    leg_result <- data.frame(COMP = game1$COMP, SEASON = game1$SEASON, ROUND = game1$ROUND,
                             TEAM1 = team1, TEAM2 = team2, WINNER = winner,
                             L1D = leg1_date, L2D = leg2_date, TYPE = type,LEG_ID = leg_id, 
                             T1G = t1_g, T2G = t2_g, G1_ID = game1$GAME_ID,
                             G2_ID = game2$GAME_ID)
    result <- rbind(result, leg_result)
  }
  return(result)
}


et_games <- get_et_games(data = ucl_po_94_18)
et_fr <- get_first_games(data = et_games, source = ucl_po_94_18)
et_fr <- clean_scores(et_fr)
et_fr <- et_fr %>%
  mutate(DATE = get_right_date(LEG, DATE))
et_fr$LEG <- factor(et_fr$LEG, levels = unique(et_fr$LEG), labels = c("1"))

et_def <- get_et_def()

games <- get_no_et_games(data = et_def)
aways <- get_away_goal_games(data = games)

games <- games %>%
  anti_join(aways)

et_games <- et_games %>%
  mutate(DATE = get_right_date(LEG, DATE))
et_games$LEG <- factor(et_games$LEG, levels = unique(et_games$LEG), labels = c("2", "F"))

et_no_finals <- et_games %>%
  filter(LEG != "F")

et_finals <- et_games %>%
  filter(LEG == "F" )

f_leg <- aways %>%
  filter(LEG == "1")

s_leg <- aways %>%
  filter(LEG == "2")

et_finals <- et_finals %>%
  distinct(SEASON, ROUND, HOMETEAM, AWAYTEAM, DATE, .keep_all = T)


#agr = away goal rule
et_legs <- combine_legs(f_leg = et_fr, s_leg = et_no_finals,1 , type = "ET")
agr_legs <- combine_legs(f_leg = f_leg, s_leg = s_leg, st_id = 1, type = "AGR")
final_legs <- finals_leg_ids(final_games = et_finals, st_id = 1)


games_1 <- games %>% 
  filter(LEG == 1)
games_2 <- games %>%
  filter(LEG == 2)

games <- combine_legs(f_leg = games_1, s_leg = games_2, st_id = 1, type = "G")

et_agr <- rbind(et_legs, agr_legs, final_legs)
et_agr$TYPE <- factor(et_agr$TYPE, levels = unique(et_agr$TYPE), labels = c("ET","AGR","F"))
et_agr[is.na(et_agr)] <- 0

et_ids <- seq(from = 1, to = nrow(et_agr), by = 1)
et_agr$GAME_ID <- et_ids 

games_ids <- seq(from = 101, to = 100 + nrow(games), by = 1)
games$GAME_ID <- games_ids

games[is.na(games)] <- 0

legs_info1 <- legs_winners(data = et_agr)
legs_info2 <- legs_winners(data = games)

legs_info <- rbind(legs_info1, legs_info2)

save(et_agr, file = "data/et_agr_94_18.rda")
write.csv(et_agr, file = "data/et_agr_94_18.csv")
save(games, file = "data/no_et_games.rda")
write.csv(games, file = "data/no_et_games.csv")
save(legs_info, file = "data/legs_info.rda")
write.csv(legs_info, file = "data/legs_info.csv")


