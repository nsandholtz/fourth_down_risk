source("./r_scripts/utils.R")
source("./r_scripts/constant.R")

library(lemon)

# Load data and model output ---------------------------------------

clean_pbp <- readRDS("./data/pbp_2014_2022.rds")
state_colnames <- readRDS("./data/state_colnames.rds")
fourth_states <- play_states |>
  filter(down == 4)

tau_hat_df <- readRDS("./output/all_dec/tau_hat_wp_quarter.rds") 


# Plot a -------------------------------------------------------------

## League Avg (Qtrs 1,2, 3) ----

observed_policy_wp_Q123 <- list()
for(i in 1:length(wp_groups)){
  observed_policy_wp_Q123[[i]] <- clean_pbp |> 
    filter(wp_group == wp_groups[i]) |>
    filter(qtr %in% c(1,2,3)) |> 
    get_observed_policy(fourth_state_df = fourth_states) |> 
    mutate(ydstogo_group = factor(ydstogo_group, levels = ydstogo_label),
           wp_group = wp_groups[i], 
           pct = obs_max_n/obs_total_n,
           max_decision = observed_decision,
           pct_cat = cut(pct, breaks = seq(0.4, 1, 0.1), include.lowest = T, right = F))
}
observed_policy_wp_Q123 <- bind_rows(observed_policy_wp_Q123)

## League Avg (Qtr 4) ----

observed_policy_wp_Q4 <- list()
for(i in 1:length(wp_groups)){
  observed_policy_wp_Q4[[i]] <- clean_pbp |> 
    filter(wp_group == wp_groups[i]) |>
    filter(qtr %in% c(4)) |> 
    get_observed_policy(fourth_state_df = fourth_states) |> 
    mutate(ydstogo_group = factor(ydstogo_group, levels = ydstogo_label),
           wp_group = wp_groups[i], 
           pct = obs_max_n/obs_total_n,
           max_decision = observed_decision,
           pct_cat = cut(pct, breaks = seq(0.4, 1, 0.1), include.lowest = T, right = F))
}
observed_policy_wp_Q4 <- bind_rows(observed_policy_wp_Q4)


plot_a <- observed_policy_wp_Q123 |> 
  dplyr::select(ydstogo_group, yardline_100_group, max_decision, pct_cat, wp_group) |> 
  mutate(Quarter = "Q1, Q2, Q3") |> 
  bind_rows(observed_policy_wp_Q4 |> 
              dplyr::select(ydstogo_group, yardline_100_group, max_decision, pct_cat, wp_group) |> 
              mutate(Quarter = "Q4")) |> 
  plot_decision_map(fill_variable = "discrete", 
                    legend_name = "",
                    fill = max_decision, alpha = pct_cat, na.value = "grey") + 
  scale_y_discrete(position = "right") + 
  facet_grid(Quarter ~ wp_group, 
             labeller = label_bquote(cols = "WP Group =" ~ .(wp_group)),
             switch = "y") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text( size = 8),
        axis.title.x = element_text( size = 9),
        axis.title.y = element_text( size = 9),
        strip.text = element_text( size = 8)) + 
  theme(aspect.ratio = 1) +
  theme(legend.position = "none") +
  geom_vline(xintercept = 5.5, linetype = "dashed")


plot_a


####### bivariate legend
d = expand.grid(qnt =  c("40-49%", "50-59%", "60-69%", "70-79%","80-89%", "90-100%"),
                decision = c("field goal", "go for it", "punt")) |> 
  arrange(decision, qnt )
d$x = rep(1:3, each = 6)
d$y = rep(seq(1,6,1), 3)
d$pct = rep(seq(0.4, 0.9, 0.1), 3)

g.legend_alt1 <- ggplot()+
  geom_tile(data = d, aes(x = x,y = y,fill=decision,
                          alpha=pct))+
  scale_fill_manual(name = "Decision",
                    values=c("gold","#d53e4f","#2C7FB8"),
                    na.value = "grey90",
                    limits = c("field goal", "go for it", "punt")
  ) +
  scale_y_continuous(breaks = seq(1,6,1),
                     labels = c("40-49%", "50-59%", "60-69%", "70-79%","80-89%", "90-100%"))+
  scale_x_continuous(breaks = seq(1,3,1),
                     labels = c("FG", "GO", "PUNT"))+
  theme(legend.position="none",
        panel.background=element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(color = "black", 
                                   angle = 45, size = 6),
        axis.text.y = element_text(color = "black", size = 8),
        axis.text=element_text(size=10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.margin=margin(t=10,b=10,l=0))+
  labs(x="Decision",y="Frequency") 

g.legend_2 = ggpubr::ggarrange(NULL, g.legend_alt1 , NULL,
                               widths = c(.3,.3,.3) ,
                               heights = c(0.9, 1.6, 0.9),
                               ncol = 1, nrow = 3)
ggpubr::ggarrange(g.legend_2,
                  plot_a, 
                  widths = c(.5, 2.25), nrow = 1)

ggsave("./figures/fig_8a.pdf", width = 6, height = 3.5)

# New plot b ------------


q_ribbon <- tau_hat_df |> 
  mutate(bot = factor(bot, levels = c(0,.5, 1, 1.5),
                      labels = c("Q1, Q2, Q3", "Q4","4th Down Bot\nQ1, Q2, Q3", "4th Down Bot\nQ4")),
         field_region = factor(field_region,
                               labels = c("Opponent Half", "Own Half")),
         Bot2 = if_else(bot %in% c(1,1.5), "4th Down Bot", "League Agg.") ,
         Quarter = if_else(bot %in% c(.5,1.5), "Q4", "Q1,Q2,Q3")) |> 
  filter(bot == "Q1, Q2, Q3" | bot == "Q4") |> 
  group_by(field_region, wp_group, bot) |> 
  summarize(lower = wtd.quantile(tau, q = .025, weight = weight),
            upper = wtd.quantile(tau, q = .975, weight = weight),
            fit = weighted.mean(tau, w = weight)) |> 
  mutate(wp_group = as.factor(wp_group))



q_ribbon |> 
  ggplot(aes(x = as.numeric(wp_group), fill = bot)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              alpha = .75,
              color = NA,
              data = q_ribbon) +
  geom_pointline(aes(y = fit, color = bot), 
                 data = q_ribbon,
                 size = .5,
                 show.legend = F) +
  facet_wrap(~ field_region) +
  scale_fill_manual(name = "Quarter", values = c("purple", "green2")) +
  scale_color_manual(values = c("purple3", "green4")) +
  scale_x_continuous(limits=c(1,3), breaks = seq(1, 3, by = 1), 
                     labels = c("[0, 0.2)        ", "[0.2, 0.8)         ", "[0.8, 1]      ")) +
  scale_y_continuous(limits=c(.2,.8), breaks = seq(.2, .8, by = .1)) +
  xlab("Win Probability") +
  ylab(expression(tau)) + 
  theme_bw() +
  theme(strip.background = element_rect(colour = 'black', fill = NA),
        axis.text.x = element_text(angle=45)) +
  theme(legend.position = c(.83,.75),
        legend.background = element_rect(fill='transparent'),
        legend.title = element_text(size = 8), 
        legend.text = element_text(size = 8))

ggsave("./figures/fig_8b.pdf", width = 4, height = 3)







