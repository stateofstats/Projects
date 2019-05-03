

library(dplyr)
library(stringr)
library(tidyr)

nst <- read.csv(file.choose(), header = TRUE,
                stringsAsFactors = FALSE)

nst %>%
  arrange(Team) %>%
  select(Game) -> games

games %>%
  separate(Game, into = c("date", "teams"), sep = " - ") %>%
  separate(teams, into = c("away", "home"), sep = ", ") -> games_split

games_split$away <- gsub(" ", "", games_split$away)
games_split$home <- gsub(" ", "", games_split$home) 



