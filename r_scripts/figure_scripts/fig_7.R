source("./r_scripts/utils.R")
source("./r_scripts/constant.R")


# Load data ---------------------------------------

pbp_dat_app <- nflfastR::load_pbp(2014:2022)
pbp_dat_app <- nfl4th::add_4th_probs(pbp_dat_app)

# Clean data ---------------------------------
clean_pbp_dat_app = clean_data(pbp_dat_app, 
                           ydstogo_bin = ydstogo_bin ,
                           yardline_100_bin = yardline_100_bin,
                           max_ydstogo = MAX_ydstogo,
                           max_yardline_100 = MAX_yardline_100) |> 
  assign_fourth_down_decision()

clean_pbp_sub_app <- clean_pbp_dat_app |> 
  mutate(season = str_sub(game_id, 1, 4) |> as.numeric(),
         coach_name = ifelse(posteam == home_team, home_coach, away_coach),
         max_wp = pmax(go_wp, fg_wp, punt_wp, na.rm = T)) |> 
  dplyr::select(game_id, 
         season,
         home_team,
         away_team,
         posteam,
         coach_name,
         season_type:play_type, 
         score_differential,
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

clean_pbp_sub_app <- clean_pbp_sub_app |>  
  mutate(fourth_down_bot = pmap_dbl(clean_pbp_sub_app |>  
                                      dplyr::select(go_wp:max_wp), ~match(..4, c(..1,..2,..3)))) |> 
  mutate(fourth_down_bot = ifelse(is.na(max_wp), NA, fourth_down_bot)) %>%
  mutate(fourth_down_bot = 
           case_when(is.na(fourth_down_bot) ~ NA_character_,
                     fourth_down_bot == 1 ~ "go",
                     fourth_down_bot == 2 ~ "fga",
                     fourth_down_bot == 3 ~ "punt"))

clean_pbp_sub_app$half_minutes_remaining = clean_pbp_sub_app$half_seconds_remaining/60

score_diff_groups <- clean_pbp_sub_app$score_differential |> 
  cut(c(-Inf, -9, -4, -1, 0, 3, 8, Inf), include.lowest = F, right = T) |> 
  sort() |> 
  as.character() |> 
  unique()

time_buckets <- clean_pbp_sub_app$half_minutes_remaining |> 
  cut(seq(0,15, by = 1), include.lowest = F, right = T) |> 
  sort() |> 
  as.character() |> 
  unique()


clean_pbp_sub_app <- clean_pbp_sub_app |> 
  mutate(score_diff_group = as.character(cut(score_differential, c(-Inf, -9, -4, -1, 0, 3, 8, Inf),
                                             include.lowest = F, right = T)),
         time_bucket = as.character(cut(half_minutes_remaining, seq(0,15, by = 1), 
                                        include.lowest = F, right = T)))


full_quant_h2 <- get_full_quant_opt(clean_pbp_sub_app |> 
                                   filter(game_half == "Half2"))

tau_hat_score_time <- list()
tau_hat_score_time_bot <- list()


for(k in 1:length(score_diff_groups)){
  cat(k, "\r")
  tau_hat_score_time[[k]] <- list()
  tau_hat_score_time_bot[[k]] <- list()
  for(h in 1:length(time_buckets)){
    tau_hat_score_time[[k]][[h]] <- get_tau_hat(pbp_dat = clean_pbp_sub_app |>
                                             filter(game_half == "Half2",
                                                    time != "15:00",
                                                    score_diff_group == score_diff_groups[k],
                                                    time_bucket == time_buckets[h]),
                                           quant_policy = full_quant_h2,
                                           fourth_states_ = fourth_states) |> 
      mutate(score_diff_group = score_diff_groups[k],
             time_bucket = time_buckets[h])
    
    tau_hat_score_time_bot[[k]][[h]] <- get_tau_hat(pbp_dat = clean_pbp_sub_app |>
                                                      filter(game_half == "Half2",
                                                             time != "15:00",
                                                             score_diff_group == score_diff_groups[k],
                                                             time_bucket == time_buckets[h]),
                                                    quant_policy = full_quant_h2,
                                                    fourth_states_ = fourth_states,
                                                    fourth_bot = TRUE) |> 
      mutate(score_diff_group = score_diff_groups[k],
             time_bucket = time_buckets[h])
  }
  tau_hat_score_time[[k]] <- bind_rows(tau_hat_score_time[[k]])
  tau_hat_score_time_bot[[k]] <- bind_rows(tau_hat_score_time_bot[[k]])
}

tau_hat_score_time <- bind_rows(tau_hat_score_time) |> 
  mutate(bot = "League Agg.")
tau_hat_score_time_bot <- bind_rows(tau_hat_score_time_bot) |> 
  mutate(bot = "4th Down Bot")

tau_hat_st <- bind_rows(tau_hat_score_time, tau_hat_score_time_bot)


tau_hat_score_time_mean <- tau_hat_st |> 
  group_by(field_region, score_diff_group, time_bucket, bot) |> 
  summarize(tau = median(tau)) |> 
  mutate(time_bucket = factor(time_bucket, levels = time_buckets),
         score_diff_group = factor(score_diff_group, levels = score_diff_groups))


lhs <- tau_hat_score_time_mean |> 
  ggplot(aes(x=time_bucket, y=score_diff_group, fill = tau)) +
  geom_tile(
    colour="white", 
    linewidth=0.1,
  ) +
  scale_fill_viridis_c(
    name = bquote(hat(tau)~"    "),
    limits = c(.2,.8)) +
  theme_classic() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, size = 10, vjust = .5),
        strip.text = element_text(size = 12),
        axis.text.y = element_text(size = 10)
  ) + 
  xlab("Minutes Remaining") +
  ylab("Score Differential") + 
  facet_grid(field_region ~ bot) +
  guides(fill = guide_colorbar(barheight = 1, 
                               barwidth = 15)) +
  theme(
    legend.title = element_text(size=14), #change legend title font size
    legend.text = element_text(size=10)) +
 scale_y_discrete(labels = rev(c("> 8", "(3, 8]", "(0, 3]", "0", "[-3, 0)", "(-3, -8]", "< -8")))


# Difference --------------------------------------------------------------


tau_hat_diff <- tau_hat_score_time_mean |> 
  pivot_wider(id_cols = c(field_region, score_diff_group, time_bucket),
              names_from = bot,
              values_from = tau) |> 
  mutate(diff_tau = `4th Down Bot` - `League Agg.`)

ann_text <- data.frame(x = c(9.5, 9.5, 4.5, 4.5, 1.5, 1.5,
                             6.5, 6.5, 3.5, 3.5, 2.5, 2.5, 1.5, 1.5),
                       xend = c(9.5, 4.5, 4.5, 1.5, 1.5, .5,
                                6.5, 3.5, 3.5, 2.5, 2.5, 1.5, 1.5, .5),
                       y = c(0.5, 1.5, 1.5, 2.5, 2.5, 4.5,
                             0.5, 1.5, 1.5, 2.5, 2.5, 3.5, 3.5, 5.5), 
                       yend = c(1.5, 1.5, 2.5, 2.5, 4.5, 4.5,
                                1.5, 1.5, 2.5, 2.5, 3.5, 3.5, 5.5, 5.5),
                       field_region = c(rep("OPP", 6),
                                        rep("OWN", 8)))

rhs <- tau_hat_diff |> 
  mutate(bot = "Difference") |> 
  ggplot(aes(x=time_bucket, y=score_diff_group)) +
  geom_tile(aes(fill = diff_tau),
    colour="white", 
    linewidth=0.1,
  ) +
  scale_fill_gradient2(
    name = bquote(hat(tau)^{"Bot"}-hat(tau)^{"League"}),
    low = 'blue',
    mid = 'white',
    midpoint = 0,
    high = 'red',
    limits = tau_hat_diff$diff_tau |> range(na.rm = T)) +
  theme_classic() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, size = 10, vjust = .5),
        strip.text = element_text(size = 12),
        axis.text.y = element_text(size = 10)
  ) + 
  xlab("Minutes Remaining") +
  ylab("Score Differential") + 
  facet_grid(field_region ~ bot) +
  guides(fill = guide_colorbar(barheight = 1, 
                               barwidth = 10)) +
  theme(
    legend.title = element_text(size=14), #change legend title font size
    legend.text = element_text(size=10)) +
  scale_y_discrete(labels = rev(c("> 8", "(3, 8]", "(0, 3]", "0", "[-3, 0)", "(-3, -8]", "< -8"))) +
  geom_segment(mapping = aes(x = x, xend = xend, y = y, yend = yend),
               data = ann_text,
               linewidth = 1.5)

pdf("./figures/fig_7a.pdf", width = 8, height = 6)
lhs
dev.off()

pdf("./figures/fig_7b.pdf", width = 4, height = 6)
rhs
dev.off()

