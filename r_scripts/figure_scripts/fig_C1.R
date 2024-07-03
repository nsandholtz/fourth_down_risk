# Paritioning the fourth down state space

source("./r_scripts/utils.R")
source("./r_scripts/constant.R")

# Load data and model output ---------------------------------------

clean_pbp <- readRDS("./data/pbp_2014_2022.rds")
state_colnames <- readRDS("./data/state_colnames.rds")
fourth_states <- play_states |>
  filter(down == 4)

# Calculating Loss ---------------------------------------------------

value_func <- get_value_function(clean_pbp)

## Trival partition ----------------------------------------------

action_TPM <- get_action_TPMs(clean_pbp, augment_third = T)

obs_policy <- clean_pbp |> 
  get_observed_policy(fourth_state_df = fourth_states)
  
tau_vec <- seq(.01, .99, by = .01)

full_q_policy <- matrix(NA, 100, length(tau_vec))
for(i in 1:length(tau_vec)) {
  cat(tau_vec[i],"\r")
  full_q_policy[,i] <- get_tau_policy_smooth(value_func = value_func,
                                             tau = tau_vec[i],
                                             a_TPM = action_TPM,
                                             num_knots = 4) |>
    pull(max_decision)
}

num_full_q_policy <- full_q_policy
num_full_q_policy[num_full_q_policy == "go" & !is.na(num_full_q_policy)] <- 1
num_full_q_policy[num_full_q_policy == "fga" & !is.na(num_full_q_policy)] <- 2
num_full_q_policy[num_full_q_policy == "punt" & !is.na(num_full_q_policy)] <- 3
num_full_q_policy <- t(apply(num_full_q_policy, 1, as.numeric))

loss <- matrix(NA, length(tau_vec), 2)
loss[,1] <- tau_vec

for(i in 1:length(tau_vec)){
  my_inds <- which(!is.na(num_full_q_policy[,i]))
  help_mat <- cbind(my_inds, num_full_q_policy[my_inds,i])

  # Decision Loss
  loss[i,2] <- sum(obs_policy$obs_total_n[my_inds]-as.matrix(obs_policy[,c("obs_go_n", "obs_fg_n", "obs_punt_n")])[help_mat])/sum(obs_policy$obs_total_n[my_inds])
}

l1 <- min(loss[,2])

## Optimal Size-2 partition -----------------------------------------------

# Using our chosen size 2 parition (opponent half vs own half)

loss2 <- matrix(NA, length(tau_vec), 3)
loss2[,1] <- tau_vec

for(i in 1:length(tau_vec)){
  FG_inds <- which(!is.na(num_full_q_policy[,i]) & (as.numeric(obs_policy$yardline_100_group) <= 5))
  PUNT_inds <- which(!is.na(num_full_q_policy[,i]) & (as.numeric(obs_policy$yardline_100_group) > 5))
  
  FG_help_mat <- cbind(FG_inds, num_full_q_policy[FG_inds,i])
  PUNT_help_mat <- cbind(PUNT_inds, num_full_q_policy[PUNT_inds,i])
  
  # Decision Loss
  loss2[i,2] <- sum(obs_policy$obs_total_n[FG_inds]-as.matrix(obs_policy[,c("obs_go_n", "obs_fg_n", "obs_punt_n")])[FG_help_mat])/sum(obs_policy$obs_total_n[FG_inds])
  loss2[i,3] <- sum(obs_policy$obs_total_n[PUNT_inds]-as.matrix(obs_policy[,c("obs_go_n", "obs_fg_n", "obs_punt_n")])[PUNT_help_mat])/sum(obs_policy$obs_total_n[PUNT_inds])
}

l2 <- (min(loss2[,2]) * sum(obs_policy$obs_total_n[FG_inds]) + 
  min(loss2[,3]) * sum(obs_policy$obs_total_n[PUNT_inds]))/sum(obs_policy$obs_total_n, na.rm = T)


## Optimal size-3 partition -----------------------------------------------


loss3 <- matrix(NA, length(tau_vec), 4)
loss3[,1] <- tau_vec

for(i in 1:length(tau_vec)){
  FG_inds <- which(!is.na(num_full_q_policy[,i]) & (obs_policy$observed_decision == "fga"))
  PUNT_inds <- which(!is.na(num_full_q_policy[,i]) & (obs_policy$observed_decision == "punt"))
  GO_inds <- which(!is.na(num_full_q_policy[,i]) & (obs_policy$observed_decision == "go"))
  
  FG_help_mat <- cbind(FG_inds, num_full_q_policy[FG_inds,i])
  PUNT_help_mat <- cbind(PUNT_inds, num_full_q_policy[PUNT_inds,i])
  GO_help_mat <- cbind(GO_inds, num_full_q_policy[GO_inds,i])
  
  # Decision Loss
  loss3[i,2] <- sum(obs_policy$obs_total_n[FG_inds]-as.matrix(obs_policy[,c("obs_go_n", "obs_fg_n", "obs_punt_n")])[FG_help_mat])/sum(obs_policy$obs_total_n[FG_inds])
  loss3[i,3] <- sum(obs_policy$obs_total_n[PUNT_inds]-as.matrix(obs_policy[,c("obs_go_n", "obs_fg_n", "obs_punt_n")])[PUNT_help_mat])/sum(obs_policy$obs_total_n[PUNT_inds])
  loss3[i,4] <- sum(obs_policy$obs_total_n[GO_inds]-as.matrix(obs_policy[,c("obs_go_n", "obs_fg_n", "obs_punt_n")])[GO_help_mat])/sum(obs_policy$obs_total_n[GO_inds])
}

l3 <- (min(loss3[,2]) * sum(obs_policy$obs_total_n[FG_inds]) + 
         min(loss3[,3]) * sum(obs_policy$obs_total_n[PUNT_inds]) + 
         min(loss3[,4]) * sum(obs_policy$obs_total_n[GO_inds]))/sum(obs_policy$obs_total_n, na.rm = T)


## Singleton parition ------------------------------------------------------------

numerator <- sum(obs_policy$obs_total_n - apply(obs_policy[,c("obs_go_n", "obs_fg_n", "obs_punt_n")], 1, max), na.rm = T)
l99 <- numerator/sum(obs_policy$obs_total_n, na.rm = T)

# PLOTS ---------------------------------------------------------------

# a) Note - we created the partition boundaries for this plot by manually
# editing the plots in powerpoint

l1_df <- clean_pbp |> 
  get_observed_policy(fourth_state_df = fourth_states) |> 
  mutate(ydstogo_group = factor(ydstogo_group, levels = ydstogo_label),
         L = "1")
l2_df <- clean_pbp |> 
  get_observed_policy(fourth_state_df = fourth_states) |> 
  mutate(ydstogo_group = factor(ydstogo_group, levels = ydstogo_label),
         L = "2")
l3_df <- clean_pbp |> 
  get_observed_policy(fourth_state_df = fourth_states) |> 
  mutate(ydstogo_group = factor(ydstogo_group, levels = ydstogo_label),
         L = "3")
l99_df <- clean_pbp |> 
  get_observed_policy(fourth_state_df = fourth_states) |> 
  mutate(ydstogo_group = factor(ydstogo_group, levels = ydstogo_label),
         L = "99")

partition_df <- bind_rows(l1_df, l2_df, l3_df, l99_df) 

observed_decision_map_no_alpha = partition_df |>
  plot_decision_map(fill_variable = "discrete", 
                    fill = observed_decision,
                    legend_name = "Most frequent\ndecision") + 
  facet_wrap(~ L, nrow = 2, labeller = label_bquote(rows = L ~ "=" ~ .(L)))  

pdf(file = "./figures/fig_C1a.pdf", width = 5, height = 4.25)
observed_decision_map_no_alpha
dev.off()

# b)
loss_df <- data.frame(L = factor(c("1","2","3","99")),
                      loss = c(l1, l2, l3, l99))


pdf(file = "./figures/fig_C1b.pdf", width = 5, height = 4.25)
loss_df |> 
  ggplot(aes(x = L, y = loss, group = 1)) +
  geom_line() +
  geom_point() + 
  ylab("Loss")
dev.off()

