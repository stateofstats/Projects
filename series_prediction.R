


library(dplyr)
library(httr)
library(rvest)

curstats <- read.csv("2019_team_stats.csv", stringsAsFactors = FALSE, header = TRUE)

teams_ref <- read.csv("teams.csv", stringsAsFactors = FALSE, header = TRUE)
page <- read_html("https://www.hockey-reference.com/leagues/NHL_2019_games.html")

html_nodes(page, "table")

page %>%
  html_nodes("table") %>%
  .[2] %>%
  html_table(fill = TRUE) -> schedule_list

schedule <- schedule_list[[1]]

names(schedule)[c(3, 5, 6)] <- c("away_goals", "home_goals", "ot")

schedule %>%
  filter(Date >= "2019-05-09") -> schedule


##############

tmp_ids <- seq(from = 1111111, to = 9999999, by = 101010)

games <- data.frame(date = schedule$Date,
                    id = tmp_ids[1:nrow(schedule)],
                    home_team = schedule$Home,
                    home_score = 0,
                    away_team = schedule$Visitor,
                    away_score = 0,
                    stringsAsFactors = FALSE)

games$home_team <- ifelse(games$home_team == "St. Louis Blues", "St Louis Blues",
                          games$home_team)

games$away_team <- ifelse(games$away_team == "St. Louis Blues", "St Louis Blues",
                          games$away_team)

games %>%
  left_join(teams_ref, by = c("home_team" = "team")) %>%
  select(name) -> home_names

games %>%
  left_join(teams_ref, by = c("away_team" = "team")) %>%
  select(name) -> away_names

games[, 3] <- home_names
games[, 5] <- away_names

names(games)[c(3, 5)] <- c("home_team", "away_team")

games %>%
  select(home_team) -> home_games

games %>%
  select(away_team) -> away_games

home_games %>%
  left_join(y = curstats, by = c("home_team" = "team")) %>%
  arrange(desc(date)) -> curall

# max(cumstats$id[which(cumstats$team == games$home_team[1])])
# max(cumstats$id[which(cumstats$team == games$home_team[2])])

home_ids <- c()
away_ids <- c()

for(i in 1:nrow(games)) {
  
  home_ids[[i]] <- max(curstats$id[which(curstats$team == games$home_team[i])])
  away_ids[[i]] <- max(curstats$id[which(curstats$team == games$away_team[i])])
  
}

mr_ids <- data.frame(team = c(games$home_team, games$away_team),
                     id = c(home_ids, away_ids))

mr_ids %>%
  left_join(curstats %>% select(-date), by = c("team", "id")) -> games_stats

games %>%
  left_join(games_stats %>% select(-c(id,
                                      away_team,
                                      away_score,
                                      home_team,
                                      home_score)), by = c("home_team" = "team")) -> home_live_stats

games %>%
  left_join(games_stats %>% select(-c(id,
                                      away_team,
                                      away_score,
                                      home_team,
                                      home_score)), by = c("away_team" = "team")) -> away_live_stats

names(home_live_stats) <- gsub("c_", "home_c_", names(home_live_stats))
names(away_live_stats) <- gsub("c_", "away_c_", names(away_live_stats))

home_live_stats %>%
  left_join(away_live_stats %>%
              select(-c(date,
                        home_team, home_score,
                        away_team, away_score)), by = "id") %>%
  unique() %>%
  arrange(id) -> live_for_model

live_for_model$home_win <- ifelse(live_for_model$home_score > live_for_model$away_score, 1, 0)


X_info <- live_for_model %>%
  select(date, 
         id,
         home_team, home_score,
         away_team, away_score)

y_predict <- live_for_model[, 'home_win']
X_predict <- live_for_model %>% select(-c(home_win,
                                          date, 
                                          id,
                                          home_team, home_score,
                                          away_team, away_score))

live_pool <- catboost.load_pool(X_predict, label = y_predict)


live_preds <- catboost.predict(model, live_pool, prediction_type = "Probability")

sched_w_preds <- cbind(X_info, live_preds)

east <- sched_w_preds %>%
  filter(home_team == "Bruins" | away_team == "Bruins")

west <- sched_w_preds %>%
  filter(home_team == "Blues" | away_team == "Blues")

east$bruins_win_prob <- ifelse(east$home_team == "Bruins", east$live_preds, 1 - as.numeric(east$live_preds))

west$blues_win_prob <- ifelse(west$home_team == "Blues", west$live_preds, 1 - as.numeric(west$live_preds))

east %>%
  mutate(bruins_win = bruins_win_prob,
         loss = 1 - bruins_win_prob) %>%
  select(date, home_team, away_team, bruins_win, loss) -> east_probs

west %>%
  mutate(blues_win = blues_win_prob,
         loss = 1 - blues_win_prob) %>%
  select(date, home_team, away_team, blues_win, loss) -> west_probs

east_probs[1, 4:5] <- c(1, 0)


print("Bruins")
series_win_prob(east_probs %>%
                  select(bruins_win, loss))
print("Blues")
series_win_prob(west_probs %>%
                  select(blues_win, loss))






