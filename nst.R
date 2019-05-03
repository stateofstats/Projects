

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

str_extract(games_split$away, "[0-9]")
str_extract(games_split$away, "[A-z][A-z]*")


