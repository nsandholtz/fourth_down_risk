library(doParallel)

setwd("~/fourth_down_risk")
source("./r_scripts/utils.R")
source("./r_scripts/constant.R")

# Load data ---------------------------------------

clean_pbp <- readRDS("./data/pbp_2014_2022.rds")
list_of_game_id = sort(unique(clean_pbp$game_id))
state_colnames <- readRDS("./data/state_colnames.rds")
fourth_states <- play_states |>
  filter(down == 4)
boot_indices <- readRDS("./data/boot_indices.rds")

full_quant_policy_wp <- readRDS("./output/boot_policies_wp.rds") 

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
             label_n = glue::glue("({n_dec})"))
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
             label_n = glue::glue("({n_dec})"))
  }
}


# Run in parallel ---------------------------------------------------------

n.cores <- 50
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)

boot_holder <- list()

for(h in 1:length(wp_groups)){
  print(h)
  boot_holder[[h]] <- list()
  for(j in 1:4){
    print(j)
    boot_holder[[h]][[j]] <- foreach(
      i = (n.cores*(j-1)+1):(n.cores*(j-1)+n.cores),
      .packages = c("tidyverse")
    ) %dopar% {
      tau_hat <- list()
      boot_data <- map_dfr(list_of_game_id[boot_indices[, i]], get_one_game_data) 
      for(k in 1:nrow(coach_list[[h]])){
        tau_hat[[k]] <- get_tau_hat(pbp_dat = boot_data |>
                                      filter(wp_group == wp_groups[h], 
                                             coach_name == coach_list[[h]]$coach_name[k],
                                             posteam == coach_list[[h]]$posteam[k]),
                                    quant_policy = full_quant_policy_wp |>
                                      filter(boot_ind == i,
                                             wp_group == wp_groups[h]),
                                    fourth_states_ = fourth_states,
                                    boot_index = i)
        tau_hat[[k]]$coach_name = coach_list[[h]]$coach_name[k]
        tau_hat[[k]]$posteam = coach_list[[h]]$posteam[k]
      } 
      tau_hat <- bind_rows(tau_hat) 
      tau_hat
    }
  }
  saveRDS(bind_rows(boot_holder[[h]]), file = paste0("./output/all_dec/tau_hat_coach_all_dec_",h,".rds"))
}

parallel::stopCluster(cl = my.cluster)





