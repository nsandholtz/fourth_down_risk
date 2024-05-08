source("./r_scripts/utils.R")
source("./r_scripts/constant.R")

# Load data and model output ---------------------------------------

clean_pbp <- readRDS("./data/pbp_2014_2022.rds")
state_colnames <- readRDS("./data/state_colnames.rds")
fourth_states <- play_states |>
  filter(down == 4)

boot_loss_full <- readRDS("./output/all_dec/boot_league_avg_loss_curves_all_dec.rds") 


# Minimizing loss ----------------------------------------------------

boot_avg <- boot_loss_full |> group_by(tau) |>
  summarise(OWN = mean(OWN, na.rm = T),
            OPP = mean(OPP, na.rm = T))

boot_min_OWN <- boot_avg[boot_avg$OWN == min(boot_avg$OWN), c("tau", "OWN")]
boot_min_OPP <- boot_avg[boot_avg$OPP == min(boot_avg$OPP), c("tau", "OPP")]

OPP_med <- boot_loss_full |>
  select(-OWN) |>
  group_by(boot_ind) |>
  filter(OPP == min(OPP)) |>
  mutate(weight = 1/n()) |> 
  ungroup() |> 
  summarize(tau_med = reldist::wtd.quantile(tau, q = .5, weight = weight))

OWN_med <- boot_loss_full |>
  select(-OPP) |>
  group_by(boot_ind) |>
  filter(OWN == min(OWN)) |>
  mutate(weight = 1/n()) |> 
  ungroup() |> 
  summarize(tau_med = reldist::wtd.quantile(tau, q = .5, weight = weight))

# Loss curve plots ----------------------------------------------------------------
## OPP Half ----------------------------------------------------------------

OPP_loss_a <- boot_loss_full |>
  ggplot(aes(tau, OPP)) +
  annotate("rect", xmin = 0, xmax = .2, ymin = 0, ymax = max(boot_loss_full$OPP, na.rm = T),
           alpha = .1,fill = gray(.5,.35)) +
  annotate("rect", xmin = .8, xmax = 1, ymin = 0, ymax = max(boot_loss_full$OPP, na.rm = T),
           alpha = .1,fill = gray(.5,.35)) +
  geom_line(aes(group = boot_ind), color = gray(.25,.05)) +
  geom_rug(data = boot_loss_full |>
             select(-OWN) |>
             group_by(boot_ind) |>
             filter(OPP == min(OPP)), sides = "b", position = "jitter", color = rgb(1,.45,0,.1)) +
  ylab("Loss") +
  theme_minimal() + 
  xlab(expression(tau[1])) +
  ggtitle(expression(paste(P[1]," (Opponent Half)"))) +
  scale_x_continuous(breaks = seq(0, 1, by = .1)) + 
  theme(plot.title = element_text(hjust = 0.5))

OPP_loss_b <- boot_loss_full |>
  select(-OWN) |>
  group_by(boot_ind) |>
  filter(OPP == min(OPP)) |>
  mutate(weight = 1/n()) |>
  ggplot(aes(x = tau, weight = weight)) +
  annotate("rect", xmin = 0, xmax = .2, ymin = 0, ymax = 16,
           alpha = .1,fill = gray(.5,.35)) +
  annotate("rect", xmin = .8, xmax = 1, ymin = 0, ymax = 16,
           alpha = .1,fill = gray(.5,.35)) +
  geom_density(fill = rgb(1,.65,0,.5),
               bw = "nrd",
               color = NA) + 
  geom_vline(xintercept = OPP_med$tau_med,
               color = rgb(1,.45,0,1), size = .75, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 1, by = .1),
                     limits = c(0,1)) + 
  ylab("Density") +
  xlab(expression(tau[1])) +
  theme_minimal() 

png(file = "./figures/fig_5_left.png", units="in", width=5, height=5.5, res=500)
egg::ggarrange(OPP_loss_a,
               OPP_loss_b, nrow = 2, heights = c(5,1))
dev.off()


## OWN Half ----------------------------------------------------------------

OWN_loss_a <- boot_loss_full |>
  ggplot(aes(tau, OWN)) +
  annotate("rect", xmin = 0, xmax = .2, ymin = 0, ymax = max(boot_loss_full$OWN, na.rm = T),
           alpha = .1,fill = gray(.5,.35)) +
  annotate("rect", xmin = .8, xmax = 1, ymin = 0, ymax = max(boot_loss_full$OWN, na.rm = T),
           alpha = .1,fill = gray(.5,.35)) +
  geom_line(aes(group = boot_ind), color = gray(.25,.05)) +
  geom_rug(data = boot_loss_full |>
             select(-OPP) |>
             group_by(boot_ind) |>
             filter(OWN == min(OWN)), sides = "b", position = "jitter", color = rgb(0,0,.5,.1)) +
  ylab("Loss") +
  theme_minimal() + 
  xlab(expression(tau[2])) +
  ggtitle(expression(paste(P[2]," (Own Half)"))) +
  scale_x_continuous(breaks = seq(0, 1, by = .1)) + 
  theme(plot.title = element_text(hjust = 0.5))

OWN_loss_b <- boot_loss_full |>
  select(-OPP) |>
  group_by(boot_ind) |>
  filter(OWN == min(OWN)) |>
  mutate(weight = 1/n()) |>
  ggplot(aes(x = tau, weight = weight)) +
  annotate("rect", xmin = 0, xmax = .2, ymin = 0, ymax = 3,
           alpha = .1,fill = gray(.5,.35)) +
  annotate("rect", xmin = .8, xmax = 1, ymin = 0, ymax = 3,
           alpha = .1,fill = gray(.5,.35)) +
  geom_density(bw = "nrd",
               fill = rgb(0,0,.5,.5),
               color = NA) + 
  geom_vline(xintercept = OWN_med$tau_med,
             color = rgb(0,0,.5,1), linewidth = .75, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 1, by = .1),
                     limits = c(0,1)) + 
  ylab("Density") +
  xlab(expression(tau[2])) +
  theme_minimal() 

png(file = "./figures/fig_5_right.png", units="in", width=5, height=5.5, res=500)
egg::ggarrange(OWN_loss_a,
               OWN_loss_b, nrow = 2, heights = c(5,1))
dev.off()

