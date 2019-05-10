



library(plyr)


myfiles <- list.files(pattern = ".csv")

nhl_preds <- ldply(myfiles, read.csv)

nhl_preds %>%
  filter(date != "2019-05-09") -> nhl_preds

schedule %>%
  filter(away_goals >= 0) -> nhl_results

cbind(nhl_preds, nhl_results) -> comb

comb$home_win_act <- ifelse(comb$home_goals > comb$away_goals, 1, 0)
comb$homw_win_pred <- ifelse(comb$live_preds >= 0.50, 1, 0)
comb$accuracy <- ifelse(comb$homw_win_pred == comb$home_win_act, 1, 0)

sum(comb$accuracy) / nrow(comb)

logLoss <- function(pred, actual) {
  -1*mean(log(pred[model.matrix(~ actual + 0) - pred > 0]))
}

logLoss(comb$live_preds, comb$home_win_act)

comb %>% 
  arrange(live_preds) %>%
  select(home_team, away_team, live_preds,
         home_goals, away_goals,
         accuracy)
