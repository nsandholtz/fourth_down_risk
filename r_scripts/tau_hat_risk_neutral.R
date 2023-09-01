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

full_quant_policy_all_dec <- readRDS("./output/boot_policies_all_dec.rds") 

# Run in parallel ---------------------------------------------------------

n.cores <- 50
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

invisible(parallel::clusterEvalQ(my.cluster, {
  RcppParallel::setThreadOptions(1L)
}))

doParallel::registerDoParallel(cl = my.cluster)

boot_holder <- list()
for (j in 1:4) {
  cat(j, "\r")
  boot_holder[[j]] <- foreach(
    i = (n.cores * (j - 1) + 1):(n.cores * (j - 1) + n.cores),
    .packages = c("tidyverse")
  ) %dopar% {
    boot_data <- map_dfr(list_of_game_id[boot_indices[, i]], get_one_game_data) 
    tau_hat_boot_risk <- get_tau_hat(pbp_dat = boot_data,
                                     quant_policy = full_quant_policy_all_dec |>
                                           filter(boot_ind == i),
                                     fourth_states_ = fourth_states,
                                     risk_neut = TRUE,
                                     boot_index = i) |> 
      mutate(wp_group = "ALL") |> 
      mutate(season = "Risk Neutral") 
    tau_hat_boot_risk
  } |>
    bind_rows()
}

parallel::stopCluster(cl = my.cluster)

tau_hat_df <- bind_rows(boot_holder)

saveRDS(tau_hat_df, file = "./output/tau_hat_risk_neutral.rds")

