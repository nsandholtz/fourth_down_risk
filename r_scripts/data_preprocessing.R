source("./r_scripts/utils.R")
source("./r_scripts/constant.R")

pbp_dat <- nflfastR::load_pbp(2014:2022)
pbp_dat <- nfl4th::add_4th_probs(pbp_dat)

# Clean data ---------------------------------
clean_pbp_dat = clean_data(pbp_dat, 
                            ydstogo_bin = ydstogo_bin ,
                            yardline_100_bin = yardline_100_bin,
                            max_ydstogo = MAX_ydstogo,
                            max_yardline_100 = MAX_yardline_100) |> 
  assign_fourth_down_decision()

clean_pbp_sub <- clean_pbp_dat |> 
  mutate(season = str_sub(game_id, 1, 4) |> as.numeric(),
         coach_name = ifelse(posteam == home_team, home_coach, away_coach),
         max_wp = pmax(go_wp, fg_wp, punt_wp, na.rm = T)) |> 
  select(game_id, 
         season,
         home_team,
         away_team,
         posteam,
         coach_name,
         season_type:play_type, 
         penalty,
         third_down_converted,
         ep, 
         epa,
         wp, 
         wp_group,
         wpa,
         fourth_down_decision, 
         fourth_down_converted, 
         go_wp,
         fg_wp,
         punt_wp,
         max_wp,
         ydstogo_group:next_play_state_3)

clean_pbp_sub <- clean_pbp_sub |>  
  mutate(fourth_down_bot = pmap_dbl(clean_pbp_sub |>  select(go_wp:max_wp), ~match(..4, c(..1,..2,..3)))) |> 
  mutate(fourth_down_bot = ifelse(is.na(max_wp), NA, fourth_down_bot)) %>%
  mutate(fourth_down_bot = 
           case_when(is.na(fourth_down_bot) ~ NA_character_,
                     fourth_down_bot == 1 ~ "go",
                     fourth_down_bot == 2 ~ "fga",
                     fourth_down_bot == 3 ~ "punt"))

saveRDS(clean_pbp_sub, "./data/pbp_2014_2022.rds")

# Play state data frame  ---------------------------------
# play_state_df = 
#   expand.grid(down = down_label,
#               ydstogo_group = ydstogo_label,
#               yardline_100_group = yardline_label) %>% 
#   arrange(down, 
#           ydstogo_group,
#           yardline_100_group) %>% 
#   unite(play_state, 
#         down,
#         ydstogo_group,
#         yardline_100_group,
#         sep = "_", remove = F) %>% 
#   as_tibble()
# 
# saveRDS(play_state_df, "./data/play_states.rds")
