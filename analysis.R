load("data/no_et_games.rda")
load("data/et_agr_94_18.rda")
load("data/legs_info.rda")
load("data/et_minutes.rda")
load("data/po")

packages_list <- c("ggplot2","dplyr", "scales",
                   "plotly", "stringr")


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

bplot <- ggplot(data = et_minutes, aes(x = "", y = MINUTE)) + 
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 150))

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
              FTGF = sum(FTHG),
              FTGA = sum(FTAG),
              ETGF = sum(ETHG),
              ETGA = sum(ETAG))
  
  as_at <- data %>%
    group_by(AWAYTEAM) %>%
    summarise(W = sum(FTR == "A"),
              L = sum(FTR == "H"),
              D = sum(FTR == "D"),
              FTGF = sum(FTAG),
              FTGA = sum(FTHG),
              ETGF = sum(ETAG),
              ETGA = sum(ETHG))
  
  table <- data.frame(TEAM = as_ht$HOMETEAM, W = as_ht$W + as_at$W,
                     D = as_ht$D + as_at$D, L = as_ht$L + as_at$L, FTGF = as_ht$FTGF + as_at$FTGF,
                     FTGA = as_ht$FTGA + as_at$FTGA, ETGF = as_ht$ETGF + as_at$ETGF, 
                     ETGA = as_ht$ETGA + as_at$ETGA)
  table$M <- table$W + table$D + table$L
  table$POINTS <- 3 * table$W + table$D
  table$EGM <- sapply(table$TEAM, games_in_et, data = et_agr)
  return(table %>%
           select(TEAM, M, POINTS ,everything()) %>%
           arrange(desc(POINTS)))
}

tables <- standings(data = all_games)

save(tables, file = "data/po_table.rda")
write.csv(tables, file = "data/po_table.csv")





