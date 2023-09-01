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

# Run in parallel ---------------------------------------------------------

n.cores <- 50
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)

boot_holder <- list()
for (j in 1:4) {
  cat(j, "\r")
  boot_holder[[j]] <- foreach(
    i = (n.cores * (j - 1) + 1):(n.cores * (j - 1) + n.cores),
    .packages = c("tidyverse", "scam")
  ) %dopar% {
    boot_data <- map_dfr(list_of_game_id[boot_indices[, i]], get_one_game_data)
      boot_full_quant <- get_full_quant_opt(boot_data)
      full_quant_policy <-
        data.frame(boot_full_quant,
                   boot_ind = i)
    full_quant_policy
  }
}
parallel::stopCluster(cl = my.cluster)

full_quant_policy_all_dec <- bind_rows(bind_rows(boot_holder[[1]]),
                                  bind_rows(boot_holder[[2]]),
                                  bind_rows(boot_holder[[3]]),
                                  bind_rows(boot_holder[[4]]))
saveRDS(full_quant_policy_all_dec, file = "./output/boot_policies_all_dec.rds")

