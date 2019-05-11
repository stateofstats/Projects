

library(dplyr)


series_win_prob <- function(probs) {
  
  x7 <- c(1, 1, 1, 1, 0, 0, 0)
  x6 <- c(1, 1, 1, 1, 0, 0)
  x5 <- c(1, 1, 1, 1, 0)
  x4 <- c(1, 1, 1, 1)
  
  expand.grid(rep(list(x7),length(x7))) -> perm7
  
  perm7 %>%
    unique() %>%
    mutate(check = rowSums(.)) %>%
    filter(check == 4 & Var7 == 1) -> perm7
  
  
  nums <- seq(1, 7, 1)
  colnames(perm7) <- c(paste0("game", nums), "check")
  
  
  
  
  perm7 %>%
    mutate(game1 = ifelse(game1 == 0, probs[1, 2], probs[1, 1]),
           game2 = ifelse(game2 == 0, probs[2, 2], probs[2, 1]),
           game3 = ifelse(game3 == 0, probs[3, 2], probs[3, 1]),
           game4 = ifelse(game4 == 0, probs[4, 2], probs[4, 1]),
           game5 = ifelse(game5 == 0, probs[5, 2], probs[5, 1]),
           game6 = ifelse(game6 == 0, probs[6, 2], probs[6, 1]),
           game7 = ifelse(game7 == 0, probs[7, 2], probs[7, 1]),
           prob = game1 * game2 * game3 * game4 * game5* game6 * game7
    ) -> prob_matrix_7
  
  win_in7 <- sum(prob_matrix_7$prob)
  
  
  
  expand.grid(rep(list(x6),length(x6))) -> perm6
  
  perm6 %>%
    unique() %>%
    mutate(check = rowSums(.)) %>%
    filter(check == 4 & Var6 == 1) -> perm6
  
  
  nums <- seq(1, 6, 1)
  colnames(perm6) <- c(paste0("game", nums), "check")
  
  
  perm6 %>%
    mutate(game1 = ifelse(game1 == 0, probs[1, 2], probs[1, 1]),
           game2 = ifelse(game2 == 0, probs[2, 2], probs[2, 1]),
           game3 = ifelse(game3 == 0, probs[3, 2], probs[3, 1]),
           game4 = ifelse(game4 == 0, probs[4, 2], probs[4, 1]),
           game5 = ifelse(game5 == 0, probs[5, 2], probs[5, 1]),
           game6 = ifelse(game6 == 0, probs[6, 2], probs[6, 1]),
           prob = game1 * game2 * game3 * game4 * game5* game6
    ) -> prob_matrix_6
  
  win_in6 <- sum(prob_matrix_6$prob)
  
  
  
  expand.grid(rep(list(x5),length(x5))) -> perm5
  
  perm5 %>%
    unique() %>%
    mutate(check = rowSums(.)) %>%
    filter(check == 4 & Var5 == 1) -> perm5
  
  
  nums <- seq(1, 5, 1)
  colnames(perm5) <- c(paste0("game", nums), "check")
  
  
  perm5 %>%
    mutate(game1 = ifelse(game1 == 0, probs[1, 2], probs[1, 1]),
           game2 = ifelse(game2 == 0, probs[2, 2], probs[2, 1]),
           game3 = ifelse(game3 == 0, probs[3, 2], probs[3, 1]),
           game4 = ifelse(game4 == 0, probs[4, 2], probs[4, 1]),
           game5 = ifelse(game5 == 0, probs[5, 2], probs[5, 1]),
           prob = game1 * game2 * game3 * game4 * game5
    ) -> prob_matrix_5
  
  win_in5 <- sum(prob_matrix_5$prob)
  
  
  
  expand.grid(rep(list(x4),length(x4))) -> perm4
  
  perm4 %>%
    unique() %>%
    mutate(check = rowSums(.)) %>%
    filter(check == 4 & Var4 == 1) -> perm4
  
  
  nums <- seq(1, 4, 1)
  colnames(perm4) <- c(paste0("game", nums), "check")
  
  
  perm4 %>%
    mutate(game1 = ifelse(game1 == 0, probs[1, 2], probs[1, 1]),
           game2 = ifelse(game2 == 0, probs[2, 2], probs[2, 1]),
           game3 = ifelse(game3 == 0, probs[3, 2], probs[3, 1]),
           game4 = ifelse(game4 == 0, probs[4, 2], probs[4, 1]),
           prob = game1 * game2 * game3 * game4
    ) -> prob_matrix_4
  
  win_in4 <- sum(prob_matrix_4$prob)
  
  
  print(paste0("win in 7: ", round(win_in7 * 100, 2),"%"))
  print(paste0("win in 6: ", round(win_in6 * 100, 2),"%"))
  print(paste0("win in 5: ", round(win_in5 * 100, 2),"%"))
  print(paste0("win in 4: ", round(win_in4 * 100, 2),"%"))
  print(paste0("win series: ", round((win_in4 + win_in5 + win_in6 + win_in7) * 100, 2), "%"))
  
  return(paste0(round((win_in4 + win_in5 + win_in6 + win_in7) * 100, 2), "%"))
  
}


