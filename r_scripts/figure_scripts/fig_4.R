source("./r_scripts/utils.R")
source("./r_scripts/constant.R")

# Load data ---------------------------------------

clean_pbp <- readRDS("./data/pbp_2014_2022.rds")
state_colnames <- readRDS("./data/state_colnames.rds")
fourth_states <- play_states |>
  filter(down == 4)


# Get quantile action value functions --------------------------------

value_func <- get_value_function(clean_pbp)

action_TPM <- get_action_TPMs(clean_pbp, 
                              augment_third = F)

tau_vec <- c(.3, .4, .5, .6, .7)

q_policy <- list()
for(i in 1:length(tau_vec)) {
  q_policy[[i]] <- get_tau_policy(value_func = value_func,
                                  a_TPM = action_TPM,
                                  tau = tau_vec[i])
}

# PLOTS -------------------------------------------------------------------


## Raw Quantile action values ----

qval_long <- bind_rows(q_policy) |>
  pivot_longer(cols = starts_with("V_"),
               names_to = "action",
               names_prefix = "V_",
               values_to = "value") |>
  mutate(action = str_to_upper(action)) |>
  select(1:4, action, value, tau)

p = qval_long |> 
  ggplot(aes(x=yardline_100_group, y=ydstogo_group, fill = value)) +
  geom_tile(
    colour="white", 
    linewidth=0.1,
  ) +
  scale_fill_gradient2(
    name = bquote(hat(q)[tau]^{bar(pi)}~(sigma~","~a)),
    low = 'blue',
    mid = 'white',
    midpoint = 0,
    high = 'red'
  ) +
  theme_classic() +
  theme(legend.key.height = unit(.2, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'),
        legend.position = "top", 
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 7)
  ) + 
  xlab("") +
  ylab("Yards to go") + 
  facet_grid(fct_relevel(action, "GO")~tau, labeller = label_bquote(cols = tau ~ "=" ~ .(tau))) +
  coord_fixed() + 
  guides(fill = guide_colorbar(barheight = .5)) +
  theme(
    legend.title = element_text(size=10), 
    legend.text = element_text(size=8))

pdf(file = "./figures/fig_5a_top.pdf", width = 8)
p
dev.off()


## Raw Quantile optimal policies ----

qopt_long<- bind_rows(q_policy) |>
  mutate(max_decision = str_to_upper(max_decision))

qopt_long$arg_max = "Arg max" 

p_argmax = qopt_long |>
  ggplot(aes(x = yardline_100_group, y = ydstogo_group, fill = fct_relevel(max_decision, "GO"))) +
  geom_tile(colour = "white", linewidth = 0.1) +
  scale_fill_manual(
    name = bquote(a^{"*"}~(sigma~","~hat(q)[tau]^{bar(pi)})),
    values = c("#d53e4f", "gold", "#2C7FB8"),
    limits = c("GO", "FGA", "PUNT")
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1,
      size = 7
    ),
    axis.text.y = element_text(
      size = 7
    ),
    strip.text.x = element_blank()
  ) + 
  theme(legend.position = "bottom") +
  xlab("Yards to opponent endzone") +
  ylab(" ") + 
  facet_grid(arg_max~tau, labeller = label_bquote(cols = tau ~ "=" ~ .(tau))) +
  coord_fixed() + 
  guides(fill = guide_legend(keyheight = .5, keywidth = .5)) +
  theme(
    legend.title = element_text(size=10), #change legend title font size
    legend.text = element_text(size=8)) #change legend text font size

pdf(file = "./figures/fig_5a_bottom.pdf", width = 8)
p_argmax
dev.off()


## Smooth Quantile action values ----

augmented_action_TPM <- get_action_TPMs(clean_pbp,
                                        augment_third = T)

q_policy_smooth <- list()
for(i in 1:length(tau_vec)) {
  q_policy_smooth[[i]] <- get_tau_policy_smooth(value_func = value_func,
                                                a_TPM = augmented_action_TPM,
                                                tau = tau_vec[i],
                                                num_knots = 4)
}

qval_long_smooth <- bind_rows(q_policy_smooth) |>
  pivot_longer(cols = starts_with("V_"),
               names_to = "action",
               names_prefix = "V_",
               values_to = "value") |>
  mutate(action = str_to_upper(action)) |>
  select(1:4, action, value, tau)

p_smooth = qval_long_smooth |> 
  ggplot(aes(x=yardline_100_group, y=ydstogo_group, fill = value)) +
  geom_tile(
    colour="white", 
    linewidth=0.1,
  ) +
  scale_fill_gradient2(
    name = bquote(hat(q)[tau]^{bar(pi)}~(sigma~","~a)),
    low = 'blue',
    mid = 'white',
    midpoint = 0,
    high = 'red',
    limits = qval_long$value |> range(na.rm = T)) +
  theme_classic() +
  theme(legend.key.height = unit(.2, 'cm'),
        legend.key.width = unit(2, 'cm'),
        legend.position = "top", 
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 7)
  ) + 
  xlab("") +
  ylab("Yards to go") + 
  facet_grid(fct_relevel(action, "GO")~tau, labeller = label_bquote(cols = tau ~ "=" ~ .(tau))) +
  coord_fixed() + 
  guides(fill = guide_colorbar(barheight = .5)) +
  theme(
    legend.title = element_text(size=10), #change legend title font size
    legend.text = element_text(size=8))

pdf(file = "./figures/fig_5b_top.pdf", width = 8)
p_smooth
dev.off()


## Smooth Quantile optimal policies ----


qopt_long_smooth <- bind_rows(q_policy_smooth) |>
  mutate(max_decision = str_to_upper(max_decision))

qopt_long_smooth$arg_max = "Arg max" 

p_argmax_smooth = qopt_long_smooth |>
  ggplot(aes(x = yardline_100_group, y = ydstogo_group, fill = fct_relevel(max_decision, "GO"))) +
  geom_tile(colour = "white", size = 0.1) +
  scale_fill_manual(
    name = bquote(a^{"*"}~(sigma~","~hat(q)[tau]^{bar(pi)})),
    values = c("#d53e4f", "gold", "#2C7FB8"),
    limits = c("GO", "FGA", "PUNT")
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1,
      size = 7
    ),
    axis.text.y = element_text(
      size = 7
    ),
    strip.text.x = element_blank()
  ) + 
  theme(legend.position = "bottom") +
  xlab("Yards to opponent endzone") +
  ylab(" ") + 
  facet_grid(arg_max~tau, labeller = label_bquote(cols = tau ~ "=" ~ .(tau))) +
  coord_fixed() + 
  guides(fill = guide_legend(keyheight = .5, keywidth = .5)) +
  theme(
    legend.title = element_text(size=10), #change legend title font size
    legend.text = element_text(size=8)) #change legend text font size

pdf(file = "./figures/fig_5b_bottom.pdf", width = 8)
p_argmax_smooth
dev.off()
