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

n.cores <- 100
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

invisible(parallel::clusterEvalQ(my.cluster, {
  RcppParallel::setThreadOptions(1L)
}))

doParallel::registerDoParallel(cl = my.cluster)

league_avg_loss <- list()

for(j in 1:2){
  cat(j, "\r")
  league_avg_loss[[j]] <- foreach(
    i = (n.cores*(j-1)+1):(n.cores*(j-1)+n.cores),
    .packages = c("tidyverse", "scam")
  ) %dopar% {
    RcppParallel::setThreadOptions(1L)
    boot_data <- map_dfr(list_of_game_id[boot_indices[, i]], get_one_game_data)
    boot_loss <- get_loss_curve(pbp_dat = boot_data,
                                fourth_states_ = fourth_states,
                                tau_vec = round(seq(.01,.99,by = .01), digits = 2))
    data.frame(boot_loss, boot_ind = i)
  } |>
    bind_rows()
}

parallel::stopCluster(cl = my.cluster)

league_avg_loss <- bind_rows(league_avg_loss)
saveRDS(league_avg_loss, file = "./output/boot_league_avg_loss_curves_all_dec.rds")

