

library(dplyr)
library(stringr)
library(tidyr)

nst <- read.csv(file.choose(), header = TRUE,
                stringsAsFactors = FALSE)

nst$id <- rep(1:(nrow(nst) / 2), rep_len(c(2, 2), (nrow(nst) / 2)))

nst %>%
  arrange(Team) %>%
  select(Game) -> games

games %>%
  separate(Game, into = c("date", "teams"), sep = " - ") %>%
  separate(teams, into = c("away", "home"), sep = ", ") -> games_split

games_split$away <- gsub(" ", "", games_split$away)
games_split$home <- gsub(" ", "", games_split$home) 

games_split$away_team <- str_extract(games_split$away, "[A-z][A-z]*")
games_split$away_score <- as.numeric(str_extract(games_split$away, "[0-9]"))
games_split$home_team <- str_extract(games_split$home, "[A-z][A-z]*")
games_split$home_score <- as.numeric(str_extract(games_split$home, "[0-9]"))

games_split %>%
  select(-c(away, home)) -> games_split

cbind(nst %>% arrange(Team) %>% select(-Game), games_split) -> df_split

print(df_split)
