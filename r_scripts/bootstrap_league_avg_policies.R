#!/usr/bin/env Rscript
args = as.numeric(commandArgs(trailingOnly=TRUE))
print(args)

source("./r_scripts/utils.R")
source("./r_scripts/constant.R")

# Load data and model output ---------------------------------------

clean_pbp <- readRDS("./data/pbp_2014_2022.rds")
list_of_game_id = sort(unique(clean_pbp$game_id))
state_colnames <- readRDS("./data/state_colnames.rds")
fourth_states <- play_states %>%
  filter(down == 4)
boot_indices <- readRDS("./data/boot_indices.rds")
tau_vec <- round(seq(.2,.8,by = .01), digits = 2)


# Get bootstrap decision maps ----------------------------------------


boot_full_policies <- list()

for(i in args[1]:args[2]){
  cat(i, "\r")
  boot_data <- map_dfr(list_of_game_id[boot_indices[, i]], get_one_game_data)
  value_func <- get_value_function(boot_data)
  
  # Smoothed policies -------------------------------------------------------
  
  augmented_action_TPM <- get_action_TPMs(boot_data,
                                          augment_third = T)
  
  # Full smooth tau policy ---------------------------------------------------------
  
  full_policy_smooth <- list()
  for(j in 1:length(tau_vec)) {
    #cat(tau_vec[i],"\r")
    full_policy_smooth[[j]] <- get_tau_policy_smooth(value_func = value_func,
                                                     a_TPM = augmented_action_TPM,
                                                     tau = tau_vec[j],
                                                     num_knots = 4)
  }
  
  boot_full_policies[[i]] <- bind_rows(full_policy_smooth) %>%
    mutate(max_decision = str_to_upper(max_decision),
           boot_ind = i)
}


# Save to disk -------------------------------------------------------


boot_full_policies_long <- bind_rows(boot_full_policies)
saveRDS(boot_full_policies_long, file = paste0("./output/boot_league_avg_policies_",
                                               args[1], "_",
                                               args[2], ".rds"))

