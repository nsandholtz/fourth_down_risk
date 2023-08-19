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

for(j in 1:10){
  cat(j, "\r")
  boot_holder[[j]] <- foreach(
    i = (n.cores*(j-1)+1):(n.cores*(j-1)+n.cores),
    .packages = c("tidyverse")
  ) %dopar% {
    boot_data <- map_dfr(list_of_game_id[boot_indices[, i]], get_one_game_data) 
    tau_hat_Q123 <- list()
    tau_hat_Q4 <- list()
    tau_hat_bot_Q123 <- list()
    tau_hat_bot_Q4 <- list()
    for(k in 1:length(wp_groups)){
      tau_hat_Q123[[k]] <- get_tau_hat(pbp_dat = boot_data |>
                                         filter(wp_group == wp_groups[k],
                                                qtr %in% c(1,2,3)),
                                       quant_policy = full_quant_policy_wp |>
                                         filter(boot_ind == i,
                                                wp_group == wp_groups[k]),
                                       fourth_states_ = fourth_states,
                                       boot_index = i)
      tau_hat_Q4[[k]] <- get_tau_hat(pbp_dat = boot_data |>
                                       filter(wp_group == wp_groups[k],
                                              qtr %in% c(4)),
                                     quant_policy = full_quant_policy_wp |>
                                       filter(boot_ind == i,
                                              wp_group == wp_groups[k]),
                                     fourth_states_ = fourth_states,
                                     boot_index = i)
      tau_hat_bot_Q123[[k]] <- get_tau_hat(pbp_dat = boot_data |>
                                             filter(wp_group == wp_groups[k],
                                                    qtr %in% c(1,2,3)),
                                           quant_policy = full_quant_policy_wp |>
                                             filter(boot_ind == i,
                                                    wp_group == wp_groups[k]),
                                           fourth_states_ = fourth_states,
                                           boot_index = i, 
                                           fourth_bot = TRUE)
      tau_hat_bot_Q4[[k]] <- get_tau_hat(pbp_dat = boot_data |>
                                           filter(wp_group == wp_groups[k],
                                                  qtr %in% c(4)),
                                         quant_policy = full_quant_policy_wp |>
                                           filter(boot_ind == i,
                                                  wp_group == wp_groups[k]),
                                         fourth_states_ = fourth_states,
                                         boot_index = i, 
                                         fourth_bot = TRUE)
      tau_hat_Q123[[k]]$wp_group = wp_groups[k]
      tau_hat_Q4[[k]]$wp_group = wp_groups[k]
      tau_hat_bot_Q123[[k]]$wp_group = wp_groups[k]
      tau_hat_bot_Q4[[k]]$wp_group = wp_groups[k]
    } 
    tau_hat_Q123 <- bind_rows(tau_hat_Q123) |> mutate(bot = 0)
    tau_hat_Q4 <- bind_rows(tau_hat_Q4) |> mutate(bot = .5)
    tau_hat_bot_Q123 <- bind_rows(tau_hat_bot_Q123) |> mutate(bot = 1)
    tau_hat_bot_Q4 <- bind_rows(tau_hat_bot_Q4) |> mutate(bot = 1.5)
    bind_rows(tau_hat_Q123, tau_hat_Q4, tau_hat_bot_Q123, tau_hat_bot_Q4)
  }
}

parallel::stopCluster(cl = my.cluster)

tau_hat_wp <- bind_rows(bind_rows(boot_holder[[1]]),
                        bind_rows(boot_holder[[2]]),
                        bind_rows(boot_holder[[3]]),
                        bind_rows(boot_holder[[4]]),
                        bind_rows(boot_holder[[5]]),
                        bind_rows(boot_holder[[6]]),
                        bind_rows(boot_holder[[7]]),
                        bind_rows(boot_holder[[8]]),
                        bind_rows(boot_holder[[9]]),
                        bind_rows(boot_holder[[10]]))

saveRDS(tau_hat_wp, file = "./output/tau_hat_wp_quarter.rds")

