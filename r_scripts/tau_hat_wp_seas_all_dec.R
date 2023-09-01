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

# Run in parallel ---------------------------------------------------------

n.cores <- 20
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

invisible(parallel::clusterEvalQ(my.cluster, {
  RcppParallel::setThreadOptions(1L)
}))

doParallel::registerDoParallel(cl = my.cluster)

boot_holder <- list()
for (j in 1:10) {
  cat(j, "\r")
  boot_holder[[j]] <- foreach(
    i = (n.cores * (j - 1) + 1):(n.cores * (j - 1) + n.cores),
    .packages = c("tidyverse")
  ) %dopar% {
    tau_hat <- list()
    boot_data <- map_dfr(list_of_game_id[boot_indices[, i]], get_one_game_data) 
    for (k in 1:length(wp_groups)) {
      for (h in 1:length(seasons)) {
        if(k == 1){
          tau_hat_boot <- get_tau_hat(pbp_dat = boot_data |>
                                        filter(wp_group == wp_groups[k],
                                               qtr %in% c(1,2,3),
                                               season == seasons[h]),
                                      quant_policy = full_quant_policy_wp |>
                                        filter(boot_ind == i,
                                               wp_group == wp_groups[k]),
                                      fourth_states_ = fourth_states,
                                      boot_index = i) |> 
            mutate(wp_group = paste0(wp_groups[k], " - Q123")) |> 
            bind_rows(get_tau_hat(pbp_dat = boot_data |>
                                    filter(wp_group == wp_groups[k],
                                           qtr %in% c(4),
                                           season == seasons[h]),
                                  quant_policy = full_quant_policy_wp |>
                                    filter(boot_ind == i,
                                           wp_group == wp_groups[k]),
                                  fourth_states_ = fourth_states,
                                  boot_index = i) |> 
                        mutate(wp_group = paste0(wp_groups[k], " - Q4")))
        } else {
          tau_hat_boot <- get_tau_hat(pbp_dat = boot_data |>
                                        filter(wp_group == wp_groups[k],
                                               season == seasons[h]),
                                      quant_policy = full_quant_policy_wp |>
                                        filter(boot_ind == i,
                                               wp_group == wp_groups[k]),
                                      fourth_states_ = fourth_states,
                                      boot_index = i) |> 
            mutate(wp_group = wp_groups[k])
        }
        
        tau_hat[[(k-1)*length(seasons)+h]] <- tau_hat_boot |> 
          mutate(season = seasons[h])
      }
    }
    tau_hat <- bind_rows(tau_hat)
    tau_hat
  } |>
    bind_rows()
}

parallel::stopCluster(cl = my.cluster)

tau_hat_df <- bind_rows(boot_holder)

saveRDS(tau_hat_df, file = "./output/tau_hat_wp_seas.rds")

