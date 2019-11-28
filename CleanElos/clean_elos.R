load("../data/po_table.rda")

packages_list <- c("dplyr", "stringr")

elo_ratings <- read.csv("all_time_ratings.csv", stringsAsFactors = F)
teams_countries <- c("England","Spain","Germany","Italy","France","Portugal",
                     "Russia", "Netherlands", "Greece", "Ukraine", "Switzerland",
                     "Cyprus", "Belgium")

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


cleaning_elos <- function(data,subset){
  elo_ratings <- data %>%
    filter(team %in% subset)
  
  cl_teams <- as.data.frame(subset)
  el_teams <- as.data.frame(unique(data$team)) 
  
  colnames(cl_teams) <- "TEAM"
  colnames(el_teams) <- "TEAM"
  
  cl_teams <- cl_teams %>%
    anti_join(el_teams)
  
  return(list(elos = elo_ratings,
              missing_teams = cl_teams))
}

dates <- as.Date(elo_ratings$date, format = "%Y/%m/%d")
dates1 <- format(as.Date(dates), "%Y-%m-%d")
elo_ratings$date <- dates1

elo_ratings <- elo_ratings %>%
  filter(date >= "1994-01-01", country %in% teams_countries)

cl_po_teams <- tables$TEAM

#THE WORST CODE OF ALL TIME
find_teams_elos <- function(){
  elo_ratings_1 <- cleaning_elos(data = elo_ratings, subset = cl_po_teams)$elos
  elo_matched_1 <- data.frame(TEAM = unique(elo_ratings_1$team), ELO.TEAM = unique(elo_ratings_1$team))
  missing_teams <- cleaning_elos(data = elo_ratings, subset = cl_po_teams)$missing_teams
  
  pattern = "AFC|ACF|FC|CF|SSC|AJ|KAA|VfL|SL|AC|AS|[0-9]|\\."
  teams <- trimws(sapply(missing_teams$TEAM, str_remove_all, pattern = pattern))
  missing_teams$ELO.TEAM <- teams
  
  elo_ratings_2 <- cleaning_elos(data = elo_ratings, subset = missing_teams$ELO.TEAM)$elos
  missing_teams_2 <- cleaning_elos(data = elo_ratings, subset = missing_teams$ELO.TEAM)$missing_teams
  
  elos_matched_2 <- missing_teams %>%
    filter((ELO.TEAM %in% unique(elo_ratings_2$team)))
  
  missing_teams_3 <- missing_teams %>%
    filter(!(ELO.TEAM %in% unique(elo_ratings_2$team)))
  
  
  missing_teams_3 <- missing_teams_3[-c(5,7,9,14,15,16,17,18,20,21),]
  missing_teams_3$ELO.TEAM <- c("PSG","Internazionale","Olympique Lyonnais",
                                "Schalke 04","Bordeaux","PSV", "Zenit", 
                                "Olympiakos Piraeus", "APOEL", "Lazio", "Stuttgart")
  elo_ratings_3 <- cleaning_elos(data = elo_ratings, subset = missing_teams_3$ELO.TEAM)$elos
  
  elos_matched_3 <- missing_teams_3
  
  teams_elo_names <- rbind(elo_matched_1,elos_matched_2,elos_matched_3)
  elo_po_teams <- rbind(elo_ratings_1,elo_ratings_2,elo_ratings_3)
  
  return(list(names = teams_elo_names, elos = elo_po_teams))
}

change_team_name <- function(data, team){
    return((data %>% 
             filter(ELO.TEAM == team))$TEAM)
}

team_names <- find_teams_elos()$names
po_elos <- find_teams_elos()$elos

team_names$TEAM <- as.character(team_names$TEAM)
team_names$ELO.TEAM <- as.character(team_names$ELO.TEAM)

po_elos <- po_elos %>%
  arrange(team)
unique(po_elos$team)

team_names <- team_names %>%
  arrange(ELO.TEAM)
changed_names <- team_names$TEAM

po_elos$team <- factor(po_elos$team, labels = changed_names)
po_elos <- po_elos %>%
  arrange(desc(rating))


save(po_elos,file =  "../data/po_elos.rda")
write.csv(po_elos, "../data/po_elos.csv")
