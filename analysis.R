load("data/no_et_games.rda")
load("data/et_agr_94_18.rda")
load("data/legs_info.rda")
load("data/et_minutes.rda")
load("data/po_elos.rda")

packages_list <- c("ggplot2","dplyr", "scales",
                   "plotly", "stringr", "elo")


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


by_id <- function(data, var, id){
  return(data[data[var] == id,])
}

all_games <- rbind(games, et_agr)

legs_summary <- legs_info %>%
  group_by(TYPE) %>%
  summarise(COUNT = n())

pie <- plot_ly(legs_summary, labels = ~TYPE, values = ~COUNT, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste(TYPE),
             marker = list(line = list(color = '#FFFFFF', width = 1)),
             showlegend = F) %>%
  layout(title = "DETERMINING LEG'S WINNER IN UCL PO ROUNDS",
         xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
         yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
pie

et_minutes <- et_minutes %>% 
  mutate(ROUND = by_id(all_games, "GAME_ID", GAME_ID)$ROUND)

box <- plot_ly(et_minutes, x = ~MINUTE, color = ~ROUND, type = "box") %>%
  layout(title = "GOAL SCORING MINUTES IN ALL ROUNDS OF PO",
         yaxis = list(showgrid = F, zeroline = F, showticklabels = F),
         xaxis = list(showgrid = T, zeroline = F, showticklabels = T))
box

all_games <- all_games %>%
  mutate(FTR = ifelse(FTHG > FTAG, "H", ifelse(FTHG < FTAG, "A", "D")),
         HTG = (FTHG + ETHG + PTHG),
         ATG = (FTAG + ETAG + PTAG),
         GR = ifelse(HTG > ATG, "H", ifelse(HTG < ATG, "A", "D")))

games_by_team <- function(data = all_games, team, field) {
  var <- "HOMETEAM"
  
  if(field == "A"){
    var <- "AWAYTEAM"
  }
  
  games <- data[data[var] == team,]
  return(length(games))
}

games_in_et <- function(data, team){
  h_games <- data %>%
    filter(HOMETEAM == team && TYPE == "ET" && LEG == 2)
  a_games <- data %>%
    filter(AWAYTEAM == team && TYPE == "ET" && LEG == 2)
  return(nrow(h_games) + nrow(a_games))
}

standings <- function(data) {
  as_ht <- data %>% 
    group_by(HOMETEAM) %>%
    summarise(W = sum(FTR == "H"),
              L = sum(FTR == "A"),
              D = sum(FTR == "D"),
              HTGF = sum(HTHG),
              HTGA = sum(HTAG),
              FTGF = sum(FTHG),
              FTGA = sum(FTAG),
              ETGF = sum(ETHG),
              ETGA = sum(ETAG))
  
  as_at <- data %>%
    group_by(AWAYTEAM) %>%
    summarise(W = sum(FTR == "A"),
              L = sum(FTR == "H"),
              D = sum(FTR == "D"),
              HTGF = sum(HTAG),
              HTGA = sum(HTHG),
              FTGF = sum(FTAG),
              FTGA = sum(FTHG),
              ETGF = sum(ETAG),
              ETGA = sum(ETHG))
  
  table <- data.frame(TEAM = as_ht$HOMETEAM, W = as_ht$W + as_at$W,
                     D = as_ht$D + as_at$D, L = as_ht$L + as_at$L, FTGF = as_ht$FTGF + as_at$FTGF,
                     FTGA = as_ht$FTGA + as_at$FTGA, ETGF = as_ht$ETGF + as_at$ETGF, 
                     ETGA = as_ht$ETGA + as_at$ETGA, HTGF = as_ht$HTGF + as_at$HTGF,
                     HTGA = as_ht$HTGA + as_at$HTGA)
  table$M <- table$W + table$D + table$L
  table$POINTS <- 3 * table$W + table$D
  table$STGF <- table$FTGF - table$HTGF
  table$STGA <- table$FTGA - table$HTGA
  table$EGM <- sapply(table$TEAM, games_in_et, data = et_agr)
  return(table %>%
           select(TEAM, M, POINTS ,everything()) %>%
           arrange(desc(POINTS)))
}

cl_po_k <- function(table){
  table <- table %>%
    filter(!(W == 0))
  table$FTGD <- table$FTGF - table$FTGA 
  table$ETGD <- table$ETGF - table$ETGA
  table$WPCT <- (table$W + round(table$D / 3)) / (table$M)
  table$RATIO <- table$FTGF / table$FTGA
  k_model <- lm(log(WPCT) ~ 0 + log(RATIO), data = table)
  k <- coefficients(k_model)
  table$EWPCT <- table$FTGF ^ k / ((table$FTGF)^k + (table$FTGA)^k)
  return(table)
}

table <- standings(data = all_games)
box_1 <- plot_ly(table, x = ~M, type = "box") %>%
  layout(title = "NUMBER OF PO MATCHES BY TEAMS",
         yaxis = list(showgrid = F, zeroline = F, showticklabels = F),
         xaxis = list(showgrid = T, zeroline = F, showticklabels = T))

box_1

wpct_table <- cl_po_k(table)
cor.test(wpct_table$WPCT, wpct_table$FTGD)


# model <- lm(log(WPCT) ~ 0 + log(RATIO), data = table)
# coefficients(model)


# save(table, file = "data/po_table.rda")
# write.csv(table, file = "data/po_table.csv")

p <- ggplot(wpct_table, aes(x = EWPCT, y = WPCT, text = TEAM)) +
    geom_point() + 
    geom_abline(intercept = 0, slope = 1, col = "red")

p <- ggplotly(p)%>%
  layout(title = "EXPECTED WIN PERCENTAGE VS ACTUAL",
         yaxis = list(showgrid = F, zeroline = F, showticklabels = F),
         xaxis = list(showgrid = T, zeroline = F, showticklabels = T))

lucky_teams <- wpct_table %>%
  filter(WPCT > EWPCT)

unlucky_teams <- wpct_table %>%
  filter(WPCT < EWPCT)

expected_teams <- wpct_table %>%
  filter(WPCT == EWPCT)

wpct_table_1 <- wpct_table %>%
  filter(M >= 4)

p1 <- ggplot(wpct_table_1, aes(x = EWPCT, y = WPCT, text = TEAM)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, col = "red")

p1 <- ggplotly(p1)%>%
  layout(title = "EXPECTED WIN PERCENTAGE VS ACTUAL",
         yaxis = list(showgrid = F, zeroline = F, showticklabels = F),
         xaxis = list(showgrid = T, zeroline = F, showticklabels = T))
p1


lucky_teams_1 <- wpct_table_1 %>%
  filter(WPCT > EWPCT)

unlucky_teams_1 <- wpct_table_1 %>%
  filter(WPCT < EWPCT)

expected_teams_1 <- wpct_table_1 %>%
  filter(WPCT == EWPCT)

lucky_teams %>%
  anti_join(lucky_teams_1)

all_time_elos <- elo.run(score(FTHG, FTAG) ~ adjust(HOMETEAM, 200) + AWAYTEAM + 
          k(30 * (all_games$FTHG - all_games$FTAG)), data = all_games)

sort(final.elos(all_time_elos), decreasing = T)

make_final_elos_df <- function(final_elos){
  final_elos <- as.data.frame(final_elos)
  final_elos$TEAM <- rownames(final_elos)
  colnames(final_elos) <- c("ELO", "TEAM")
  rownames(final_elos) <- 1:length(final_elos$TEAM)
  final_elos <- final_elos[c("TEAM", "ELO")]
  return(final_elos)
}


get_elos <- function(final_elos, def_elo = 1500){
  starting_elos <- c()
  if(length(final_elos) == 0){
    starting_elos = def_elo
  }
  else{
    starting_elos <- (def_elo) + (final_elos - def_elo)/2
  }
  return(starting_elos)
}


calculate_relative_elos <- function(data){
  relative_df <- c()
  final_elos <- c()
  for(season in unique(data$SEASON)){
    starting_elos <- get_elos(final_elos, def_elo = 1500)
    season_data <- data[data$SEASON == season,]
    season_elos <- elo.run(score(FTHG, FTAG) ~ adjust(HOMETEAM, 200) + AWAYTEAM + 
                             k(30 * (all_games$FTHG - all_games$FTAG)), data = all_games,
                           initial.elos = starting_elos)
    season_final_elos <- final.elos(season_elos)
    season_elos <- data.frame(SEASON = season,season_elos)
    relative_df <- rbind(relative_df, season_elos)
    final_elos <- season_final_elos
  }
  final_elos <- make_final_elos_df(final_elos)
  return(list(relative = relative_df, elos_df = final_elos))
} 


custom_elos <- calculate_relative_elos(data = games_no_rares)

custom_elos$elos_df <- custom_elos$elos_df %>%
  filter(TEAM %in% wpct_table_1$TEAM) %>%
  arrange(desc(ELO))
custom_elos$elos_df



gold_minutes <- et_minutes %>% 
  filter(MINUTE > 90) %>%
  group_by(LEG_ID) %>%
  summarise(MINUTE = min(MINUTE))

et_minutes %>%
  inner_join(gold_minutes, by = c("MINUTE", "LEG_ID"))


gold_goal <- function(data) {
  gold_minutes <- data %>% 
    filter(MINUTE > 90) %>%
    group_by(LEG_ID) %>%
    summarise(MINUTE = min(MINUTE))
  winners <- data %>%
    inner_join(gold_minutes, by = c("MINUTE", "LEG_ID"))
  return(winners)
}

get_elos_by_date <- function(source, game_date, game_team) {
  elos <- source %>% 
    filter(team == game_team , date <= game_date ) %>%
    arrange(desc(date))
  return(elos[1,]$rating)
}

neutral_games <- function(data) {
  agr_legs <- data %>%
    filter(TYPE == "AGR")
  elo_teams <- unique(po_elos$team)
  result <- c()

  for(i in 1:nrow(agr_legs)){
    leg <- agr_legs[i,]
    team1 <- as.character(leg$TEAM1)
    team2 <- as.character(leg$TEAM2)
    l2date <- leg$L2D
    if(team1 %in% elo_teams && team2 %in% elo_teams){
      elo1 <- get_elos_by_date(source = po_elos, game_date = l2date, game_team = team1)
      elo2 <- get_elos_by_date(source = po_elos, game_date = l2date, game_team = team2)
      wins.1 = elo.prob(elo.A = elo1,
                        elo.B = elo2)
      wins.2 = 1 - wins.1
      game <- data.frame(TEAM1 = team1, TEAM2 = team2, WINS.1 = wins.1, WINS.2 = wins.2,
                         LEG_ID = leg$LEG_ID, ACTUAL_WINNER = leg$WINNER)
      result <- rbind(result, game)
    }
  }
  result$TEAM1 <- as.character(result$TEAM1)
  result$TEAM2 <- as.character(result$TEAM2)
  result <- result %>%
    mutate(NG_WINNER = ifelse(WINS.1 > WINS.2, TEAM1, TEAM2))
  return(result)
}



simulated_games <- neutral_games(data = legs_info)



golden_goal_winners <- gold_goal(data = et_minutes)
actual_winners <- legs_info %>%
  filter(LEG_ID %in% golden_goal_winners$LEG_ID)


