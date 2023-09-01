#setwd("~/fourth_down_risk")
source("./r_scripts/utils.R")
source("./r_scripts/constant.R")

dec_limit <- 25

# Load data ---------------------------------------

clean_pbp <- readRDS("./data/pbp_2014_2022.rds")
list_of_game_id = sort(unique(clean_pbp$game_id))
state_colnames <- readRDS("./data/state_colnames.rds")
fourth_states <- play_states |>
  filter(down == 4)

full_quant_policy <- list()
for (k in 1:length(wp_groups)) {
  full_quant_policy[[k]] <- get_full_quant_opt(clean_pbp |> 
                                        filter(wp_group == wp_groups[k]))
  full_quant_policy[[k]]$wp_group = wp_groups[k]
}

full_quant_policy <- bind_rows(full_quant_policy)

# Eligible coach list ------------------------------------------------

# Eligible coaches - more than dec_limit decisions in a wp group
coach_list <- list()
for(i in 1:length(wp_groups)){
  if(i == 1){
    coach_list[[i]] <- clean_pbp |>
      mutate(field_region = ifelse(yardline_100 <= 50, "Own Half", "Opp Half")) |> 
      filter(down == 4, 
             !is.na(fourth_down_decision),
             wp_group == wp_groups[i],
             qtr < 4) |>
      group_by(coach_name, posteam, field_region) |>
      summarize(n_dec = n()) |>
      group_by(coach_name, posteam) %>%
      mutate(min_dec = min(n_dec)) %>%
      ungroup() |> 
      filter(min_dec >= dec_limit) |> 
      arrange(coach_name) |> 
      mutate(wp_group = wp_groups[i],
             label_n = glue::glue("({n_dec})")) |> 
      select(coach_name, posteam) |> 
      distinct()
  } else {
    coach_list[[i]] <- clean_pbp |>
      mutate(field_region = ifelse(yardline_100 <= 50, "Own Half", "Opp Half")) |> 
      filter(down == 4, 
             !is.na(fourth_down_decision),
             wp_group == wp_groups[i]) |>
      group_by(coach_name, posteam, field_region) |>
      summarize(n_dec = n()) |>
      group_by(coach_name, posteam) %>%
      mutate(min_dec = min(n_dec)) %>%
      ungroup() |> 
      filter(min_dec >= dec_limit) |> 
      arrange(coach_name) |> 
      mutate(wp_group = wp_groups[i],
             label_n = glue::glue("({n_dec})")) |> 
      select(coach_name, posteam) |> 
      distinct()
  }
}


# Compute tau hat for coach/wp/seas combos ---------------------------

tau_hat_coach_wp_seas <- list()

for (k in 1:length(wp_groups)) {
  print(k)
  tau_hat_coach_wp_seas[[k]] <- list()
  for (i in 1:nrow(coach_list[[k]])) {
    print(i)
    tau_hat_coach_wp_seas[[k]][[i]] <- list()
    for (j in 1:length(seasons)) {
      #print(j)
      if (k == 1) {
        if(clean_pbp |>
                 filter(
                   wp_group == wp_groups[k],
                   qtr %in% c(1, 2, 3),
                   coach_name == coach_list[[k]]$coach_name[i],
                   posteam == coach_list[[k]]$posteam[i],
                   season == seasons[j]
                 ) |>  nrow() == 0) next
        tau_hat_coach_wp_seas[[k]][[i]][[j]] <-
          get_tau_hat(
            pbp_dat = clean_pbp |>
              filter(
                wp_group == wp_groups[k],
                qtr %in% c(1, 2, 3),
                coach_name == coach_list[[k]]$coach_name[i],
                posteam == coach_list[[k]]$posteam[i],
                season == seasons[j]
              ),
            quant_policy = full_quant_policy |>
              filter(wp_group == wp_groups[k]),
            fourth_states_ = fourth_states
          ) |>
          mutate(
            wp_group = wp_groups[k],
            coach_name = coach_list[[k]]$coach_name[i],
            posteam = coach_list[[k]]$posteam[i],
            season = seasons[j]
          )
      } else {
        if(clean_pbp |>
           filter(
             wp_group == wp_groups[k],
             qtr %in% c(1, 2, 3),
             coach_name == coach_list[[k]]$coach_name[i],
             posteam == coach_list[[k]]$posteam[i],
             season == seasons[j]
           ) |>  nrow() == 0) next
        tau_hat_coach_wp_seas[[k]][[i]][[j]] <-
          get_tau_hat(
            pbp_dat = clean_pbp |>
              filter(
                wp_group == wp_groups[k],
                coach_name == coach_list[[k]]$coach_name[i],
                posteam == coach_list[[k]]$posteam[i],
                season == seasons[j]
              ),
            quant_policy = full_quant_policy |>
              filter(wp_group == wp_groups[k]),
            fourth_states_ = fourth_states
          ) |>
          mutate(
            wp_group = wp_groups[k],
            coach_name = coach_list[[k]]$coach_name[i],
            posteam = coach_list[[k]]$posteam[i],
            season = seasons[j]
          )
      }
    }
  }
}

for (k in 1:length(wp_groups)) {
  for (i in 1:nrow(coach_list[[k]])) {
    tau_hat_coach_wp_seas[[k]][[i]] <- bind_rows(tau_hat_coach_wp_seas[[k]][[i]])
  }
}

for (k in 1:length(wp_groups)) {
  tau_hat_coach_wp_seas[[k]] <- bind_rows(tau_hat_coach_wp_seas[[k]])
}
    
tau_hat_coach_wp_seas <- bind_rows(tau_hat_coach_wp_seas)

tau_hat_coach_wp_seas_med <- tau_hat_coach_wp_seas |> 
  summarize(tau_med = reldist::wtd.quantile(tau, q = .5, weight = weight), 
            .by = c(coach_name, posteam, season, field_region, wp_group))

saveRDS(tau_hat_coach_wp_seas, file = "./output/tau_hat_coach_wp_seas.rds")
saveRDS(tau_hat_coach_wp_seas_med, file = "./output/tau_hat_coach_wp_seas_med.rds")

