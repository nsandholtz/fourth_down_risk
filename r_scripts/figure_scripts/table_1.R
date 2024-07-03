library(ppcor)

source("./r_scripts/utils.R")
source("./r_scripts/constant.R")

# Load data model output ---------------------------------------

clean_pbp <- readRDS("./data/pbp_2014_2022.rds")
state_colnames <- readRDS("./data/state_colnames.rds")
tau_hat_coach_wp_seas_med <- readRDS("./output/tau_hat_coach_wp_seas_med.rds")
df_elo <- read.csv("./data/nfl_elo.csv")
#df_elo <- read.csv('https://projects.fivethirtyeight.com/nfl-api/nfl_elo.csv', 
#                   stringsAsFactors = F) 
#write.csv(df_elo, file = "./data/nfl_elo.csv")

# Compute value function -------------------------
value_func <- get_value_function(clean_pbp)
values_added_current_state = value_func %>% 
  filter(team == "off") %>% 
  mutate(play_state = str_remove(play_state, "off_")) %>% 
  rename(current_state_value = value)

# Get average value gained by season, coach, team, wp_group, and field region ----
avg_value_gained_by_coach <- 
  clean_pbp %>% 
  left_join(values_added_current_state %>% 
              dplyr::select(play_state, current_state_value), 
            by = c("play_state")) %>% 
  left_join(value_func %>% 
              dplyr::select(play_state, next_state_value = value), 
            by = c( "next_play_state_3" = "play_state")) %>% 
  mutate(value_gained = next_state_value - current_state_value) %>% 
  mutate(field_region = ifelse(yardline_100 <= 50, "OPP", "OWN")) %>%
  # limit on 4th down plays
  filter(down == 4) %>% 
  group_by(season, coach_name, posteam, wp_group, field_region) %>% 
  summarise(n = n(),
            avg_value_gained = mean(value_gained, na.rm = T)
  ) %>% 
  ungroup()

# Get average elo values by season and team ------------
team_season_comb <- clean_pbp %>%
  dplyr::select(season, coach_name, posteam) %>% 
  distinct()

df_elo_long <- rbind(df_elo %>% 
                       dplyr::select(date, season, team = team1, elo = elo1_pre),
                     df_elo %>% 
                       dplyr::select(date, season, team = team2, elo = elo2_pre) 
) %>% 
  group_by(season, team) %>% 
  summarise(avg_elo = mean(elo)) %>% 
  ungroup() %>% 
  mutate(team = case_when(team == "OAK" ~ "LV",
                          team == "WSH" ~ "WAS",
                          team == "LAR" ~ "LA",
                          TRUE ~ team
  )) %>% 
  mutate(avg_elo_scale = scale(avg_elo))

coach_elo <- team_season_comb %>% 
  left_join(df_elo_long, by = c("posteam" = "team", "season" = "season")) %>% 
  group_by(coach_name, season, posteam) %>% 
  summarise(avg_elo = mean(avg_elo),
            avg_elo_scale = mean(avg_elo_scale)) %>% 
  ungroup()

# Final data set ----------------------

final_data_set <- avg_value_gained_by_coach %>% 
  left_join(coach_elo, by = c("coach_name", "posteam","season")) %>%
  left_join(tau_hat_coach_wp_seas_med,
            by = c("coach_name", "posteam","season", "field_region", "wp_group"))

# Run regression -----------------------

lm(avg_value_gained ~ avg_elo_scale + tau_med + field_region, 
   data = final_data_set |> filter(n >= 25)) %>% 
  summary

# Partial correlations ---------------------

pcor_mat <- pcor(final_data_set |> 
                   filter(n >= 25) |> 
                   dplyr::select(avg_value_gained, avg_elo_scale, tau_med, field_region) |> 
                   mutate(field_region = as.numeric(as.factor(field_region)))) %>%
  with(.$estimate) %>% as.matrix()

pcor_mat[1,]^2
