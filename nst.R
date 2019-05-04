

library(dplyr)
library(stringr)
library(tidyr)

nst <- read.csv(file.choose(), header = TRUE,
                stringsAsFactors = FALSE)

nst$id <- rep(1:(nrow(nst) / 2), rep_len(c(2, 2), (nrow(nst) / 2)))

names(nst) <- gsub("ï..Game", "Game", names(nst))

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

names(df_split) <- tolower(names(df_split))
names(df_split) <- gsub("[.]", "_60", names(df_split))

# change per 60 measurements to numeric
df_split$gf_60 <- as.numeric(df_split$gf_60)
df_split$hdgf_60 <- as.numeric(df_split$hdgf_60)
df_split$hdsh_60 <- as.numeric(df_split$hdsh_60)
df_split$hdsv_60 <- as.numeric(df_split$hdsv_60)
df_split$mdgf_60 <- as.numeric(df_split$mdgf_60)
df_split$mdsh_60 <- as.numeric(df_split$mdsh_60)
df_split$mdsv_60 <- as.numeric(df_split$mdsv_60)
df_split$ldgf_60 <- as.numeric(df_split$ldgf_60)


# change team names to short names
teams_ref <- read.csv("teams.csv", stringsAsFactors = FALSE, header = TRUE)

left_join(df_split, teams_ref) %>%
  select(name) -> names

df_split$team <- names$name

df_split %>%
  group_by(team) %>%
  mutate(
    # counting stats
    c_toi = cumsum(toi),
    c_cf = cumsum(cf),
    c_ca = cumsum(ca),
    c_ff = cumsum(ff),
    c_fa = cumsum(fa),
    c_sf = cumsum(sf),
    c_sa = cumsum(sa),
    c_gf = cumsum(gf),
    c_ga = cumsum(ga),
    c_xgf = cumsum(xgf),
    c_xga = cumsum(xga),
    c_scf = cumsum(scf),
    c_sca = cumsum(sca),
    c_hdcf = cumsum(hdcf),
    c_hdca = cumsum(hdca),
    c_hdsf = cumsum(hdsf),
    c_hdsa = cumsum(hdsa),
    c_hdgf = cumsum(hdgf),
    c_hdga = cumsum(hdga),
    c_mdcf = cumsum(mdcf),
    c_mdca = cumsum(mdca),
    c_mdsf = cumsum(mdsf),
    c_mdsa = cumsum(mdsa),
    c_mdgf = cumsum(mdgf),
    c_mdga = cumsum(mdga),
    c_ldcf = cumsum(ldcf),
    c_ldca = cumsum(ldca),
    c_ldsf = cumsum(ldsf),
    c_ldsa = cumsum(ldsa),
    c_ldgf = cumsum(ldgf),
    c_ldga = cumsum(ldga),
    
    # percent share stats
    c_cf_per = c_cf / (c_cf + c_ca),
    c_ff_per = c_ff / (c_ff + c_fa),
    c_sf_per = c_sf / (c_sf + c_sa),
    c_gf_per = c_gf / (c_gf + c_ga),
    c_xgf_per = c_xgf / (c_xgf + c_xga),
    c_scf_per = c_scf / (c_scf + c_sca),
    c_hdcf_per = c_hdcf / (c_hdcf + c_hdca),
    c_hdsf_per = c_hdsf / (c_hdsf + c_hdsa),
    c_hdgf_per = c_hdgf / (c_hdgf + c_hdga),
    c_hdsh_per = c_hdgf / c_hdsf,
    c_hdsv_per = 1 - (c_hdga / c_hdsa),
    c_mdcf_per = c_mdcf / (c_mdcf + c_mdca),
    c_mdsf_per = c_mdsf / (c_mdsf + c_mdsa),
    c_mdgf_per = c_mdgf / (c_mdgf + c_mdga),
    c_mdsh_per = c_mdgf / c_mdsf,
    c_mdsv_per = 1 - (c_mdga / c_mdsa),
    c_ldcf_per = c_ldcf / (c_ldcf + c_ldca),
    c_ldsf_per = c_ldsf / (c_ldsf + c_ldsa),
    c_ldgf_per = c_ldgf / (c_ldgf + c_ldga),
    c_ldsh_per = c_ldgf / c_ldsf,
    c_ldsv_per = 1 - (c_ldga / c_ldsa),
    c_sh_per = c_gf / c_sf,
    c_sv_per = 1 - (c_ga / c_sa),
    c_pdo = c_sh_per + c_sv_per,
    
    # per 60 stats
    c_cf_60 = (c_cf / c_toi) * 60,
    c_ca_60 = (c_ca / c_toi) * 60,
    c_ff_60 = (c_ff / c_toi) * 60,
    c_fa_60 = (c_fa / c_toi) * 60,
    c_sf_60 = (c_sf / c_toi) * 60,
    c_sa_60 = (c_sa / c_toi) * 60,
    c_gf_60 = (c_gf / c_toi) * 60,
    c_ga_60 = (c_ga / c_toi) * 60,
    c_xgf_60 = (c_xgf / c_toi) * 60,
    c_xga_60 = (c_xga / c_toi) * 60,
    c_scf_60 = (c_scf / c_toi) * 60,
    c_sca_60 = (c_sca / c_toi) * 60,
    c_hdcf_60 = (c_hdcf / c_toi) * 60,
    c_hdca_60 = (c_hdca / c_toi) * 60,
    c_hdsf_60 = (c_hdsf / c_toi) * 60,
    c_hdsa_60 = (c_hdsa / c_toi) * 60,
    c_hdgf_60 = (c_hdgf / c_toi) * 60,
    c_hdga_60 = (c_hdga / c_toi) * 60,
    c_mdcf_60 = (c_mdcf / c_toi) * 60,
    c_mdca_60 = (c_mdca / c_toi) * 60,
    c_mdsf_60 = (c_mdsf / c_toi) * 60,
    c_mdsa_60 = (c_mdsa / c_toi) * 60,
    c_mdgf_60 = (c_mdgf / c_toi) * 60,
    c_mdga_60 = (c_mdga / c_toi) * 60,
    c_ldcf_60 = (c_ldcf / c_toi) * 60,
    c_ldca_60 = (c_ldca / c_toi) * 60,
    c_ldsf_60 = (c_ldsf / c_toi) * 60,
    c_ldsa_60 = (c_ldsa / c_toi) * 60,
    c_ldgf_60 = (c_ldgf / c_toi) * 60,
    c_ldga_60 = (c_ldga / c_toi) * 60
    
    
  ) %>%
  select(team, starts_with("c_"),
         date, id,
         away_team, away_score,
         home_team, home_score) -> cumstats

cumstats %>%
  group_by(id) %>%
  select(date, id,
         away_team, away_score,
         home_team, home_score) %>%
  arrange(id) %>%
  gather("category", "value", -c(date, id)) %>%
  arrange(id) %>%
  unique() %>%
  spread(category, c(value)) %>%
  mutate(home_win = ifelse(home_score > away_score, 1, 0)) -> results

results %>%
  select(date, id, away_score, away_team) -> aways
results %>%
  select(date, id, home_score, home_team) -> homes

homes %>%
  left_join(cumstats %>%
              select(-c(
                date, home_team, home_score,
                away_team, away_score
              )), by = c("home_team" = "team", "id")) -> home_stats

aways %>%
  left_join(cumstats %>%
              select(-c(
                date, away_team, away_score,
                home_team, home_score
              )), by = c("away_team" = "team", "id")) -> away_stats

names(home_stats) <- gsub("c_", "home_c_", names(home_stats))
names(away_stats) <- gsub("c_", "away_c_", names(away_stats))

home_stats %>%
  left_join(away_stats %>%
              select(-date), by = "id") %>%
  arrange(id) -> final_df

final_df$home_win <- ifelse(final_df$home_score > final_df$away_score, 1, 0)

final_df <- final_df %>%
  select(date, id, 
         home_team, home_score,
         away_team, away_score,
         home_win,
         everything())

write.csv(final_df, "2019.csv", row.names = FALSE)

