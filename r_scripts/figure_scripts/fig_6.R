source("./r_scripts/utils.R")
source("./r_scripts/constant.R")

library(lemon)

# Load data and model output ---------------------------------------

clean_pbp <- readRDS("./data/pbp_2014_2022.rds")
state_colnames <- readRDS("./data/state_colnames.rds")
fourth_states <- play_states |>
  filter(down == 4)

tau_hat_df <- readRDS("./output/all_dec/tau_hat_wp_quarter.rds") 


# 4th down action counts ---------------------

## Qtrs 1, 2, and 3 ----
observed_decision_breakdown_OPP <- list()
for(i in 1:length(wp_groups)){
  observed_decision_breakdown_OPP[[i]] <- clean_pbp |> 
    filter(wp_group == wp_groups[i]) |>
    filter(qtr %in% c(1,2,3)) |> 
    get_observed_policy(fourth_state_df = fourth_states) |> 
    drop_na(observed_decision) |> 
    mutate(field_region = if_else(as.numeric(yardline_100_group) > 5, "OWN", "OPP")) |> 
    filter(field_region == "OPP") |>
    summarize(go_decisions = sum(obs_go_n, na.rm = T),
              fga_decisions = sum(obs_fg_n, na.rm = T),
              punt_decisions = sum(obs_punt_n, na.rm = T)) |> 
    mutate(wp_group = wp_groups[i])
}
observed_decision_breakdown_OPP <- bind_rows(observed_decision_breakdown_OPP)

observed_decision_breakdown_OWN <- list()
for(i in 1:length(wp_groups)){
  observed_decision_breakdown_OWN[[i]] <- clean_pbp |> 
    filter(wp_group == wp_groups[i]) |>
    filter(qtr %in% c(1,2,3)) |> 
    get_observed_policy(fourth_state_df = fourth_states) |> 
    drop_na(observed_decision) |> 
    mutate(field_region = if_else(as.numeric(yardline_100_group) > 5, "OWN", "OPP")) |> 
    filter(field_region == "OWN") |> 
    summarize(go_decisions = sum(obs_go_n, na.rm = T),
              fga_decisions = sum(obs_fg_n, na.rm = T),
              punt_decisions = sum(obs_punt_n, na.rm = T)) |> 
    mutate(wp_group = wp_groups[i])
}
observed_decision_breakdown_OWN <- bind_rows(observed_decision_breakdown_OWN)

observed_decision_breakdown_OPP
observed_decision_breakdown_OWN

# Proportions
t((observed_decision_breakdown_OPP[,1:3])/rowSums(observed_decision_breakdown_OPP[,1:3]))
t((observed_decision_breakdown_OWN[,1:3])/rowSums(observed_decision_breakdown_OWN[,1:3]))

## Qtr 4 ----
observed_decision_breakdown_OPP <- list()
for(i in 1:length(wp_groups)){
  observed_decision_breakdown_OPP[[i]] <- clean_pbp |> 
    filter(wp_group == wp_groups[i]) |>
    filter(qtr %in% c(4)) |> 
    get_observed_policy(fourth_state_df = fourth_states) |> 
    drop_na(observed_decision) |> 
    mutate(field_region = if_else(as.numeric(yardline_100_group) > 5, "OWN", "OPP")) |> 
    filter(field_region == "OPP") |> 
    summarize(go_decisions = sum(obs_go_n, na.rm = T),
              fga_decisions = sum(obs_fg_n, na.rm = T),
              punt_decisions = sum(obs_punt_n, na.rm = T)) |> 
    mutate(wp_group = wp_groups[i])
}
observed_decision_breakdown_OPP <- bind_rows(observed_decision_breakdown_OPP)

observed_decision_breakdown_OWN <- list()
for(i in 1:length(wp_groups)){
  observed_decision_breakdown_OWN[[i]] <- clean_pbp |> 
    filter(wp_group == wp_groups[i]) |>
    filter(qtr %in% c(4)) |> 
    get_observed_policy(fourth_state_df = fourth_states) |> 
    drop_na(observed_decision) |> 
    mutate(field_region = if_else(as.numeric(yardline_100_group) > 5, "OWN", "OPP")) |> 
    filter(field_region == "OWN") |> 
    summarize(go_decisions = sum(obs_go_n, na.rm = T),
              fga_decisions = sum(obs_fg_n, na.rm = T),
              punt_decisions = sum(obs_punt_n, na.rm = T)) |> 
    mutate(wp_group = wp_groups[i])
}
observed_decision_breakdown_OWN <- bind_rows(observed_decision_breakdown_OWN)

observed_decision_breakdown_OPP
observed_decision_breakdown_OWN

# Proportions
t((observed_decision_breakdown_OPP[,1:3])/rowSums(observed_decision_breakdown_OPP[,1:3]))
t((observed_decision_breakdown_OWN[,1:3])/rowSums(observed_decision_breakdown_OWN[,1:3]))

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
  select(ydstogo_group, yardline_100_group, max_decision, pct_cat, wp_group) |> 
  mutate(Quarter = "Q1, Q2, Q3") |> 
  bind_rows(observed_policy_wp_Q4 |> 
              select(ydstogo_group, yardline_100_group, max_decision, pct_cat, wp_group) |> 
              mutate(Quarter = "Q4")) |> 
  plot_decision_map(fill_variable = "discrete", 
                    legend_name = "",
                    fill = max_decision, alpha = pct_cat, na.value = "grey") + 
  scale_y_discrete(position = "right") + 
  facet_grid(Quarter ~ wp_group, 
             labeller = label_bquote(cols = "WP Group =" ~ .(wp_group)),
             switch = "y") +
  # theme(plot.title = element_text(hjust = 0.5, size = 10),
  #       plot.subtitle = element_text(hjust = 0.5),
  #       axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
  #       axis.text.y = element_text( size = 8),
  #       axis.title.x = element_text( size = 9),
  #       axis.title.y = element_text( size = 9),
  #       strip.text = element_text( size = 8)) + 
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
        axis.text.x = element_text(color = "black", size = 6),
        axis.text.y = element_text(color = "black", size = 8),
        axis.text=element_text(size=10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.margin=margin(t=10,b=10,l=0))+
  labs(x="Decision",y="Frequency") 

g.legend_2 = ggpubr::ggarrange(NULL, g.legend_alt1 , NULL,
                               widths = c(.3,.3,.3) ,
                               heights = c(0.9, 1.1, 0.9),
                               ncol = 1, nrow = 3)

ggpubr::ggarrange(g.legend_2,
                  plot_a, 
                  widths = c(.3, 1.25), nrow = 1)

ggsave("./figures/fig_7a_new.pdf", width = 7, height = 4.15)

# New plot b

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
  # geom_point(aes(y = fit),
  #            data = q_ribbon |> 
  #              filter(wp < .03 | near(wp, .475) | wp > .95),
  #            color = "red",
  #            show.legend = F) +
  facet_wrap(~ field_region) +
  # scale_fill_viridis_d(name = "",
  #                      option = "D") +
  scale_fill_manual(name = "Quarter", values = c("purple", "green2")) +
  scale_color_manual(values = c("purple3", "green4")) +
  # scale_color_viridis_d(name = "",
  #                        option = "D") +
  scale_x_continuous(limits=c(1,3), breaks = seq(1, 3, by = 1), 
                     labels = c("[0, 0.2)        ", "[0.2, 0.8)         ", "[0.8, 1]      ")) +
  scale_y_continuous(limits=c(.2,.8), breaks = seq(.2, .8, by = .1)) +
  xlab("Win Probability") +
  ylab(expression(tau)) + 
  theme_bw() +
  theme(strip.background = element_rect(colour = 'black', fill = NA),
        axis.text.x = element_text(angle=45)) 

ggsave("./figures/fig_7b_new.pdf", width = 7, height = 3.5)









tau_hat_intervals_plot <- tau_hat_df |> 
  mutate(bot = factor(bot, levels = c(0,.5, 1, 1.5),
                      labels = c("Q1, Q2, Q3", "Q4","4th Down Bot\nQ1, Q2, Q3", "4th Down Bot\nQ4")),
         field_region = factor(field_region,
                               labels = c("Opponent Half", "Own Half")),
         Bot2 = if_else(bot %in% c(1,1.5), "4th Down Bot", "League Agg.") ,
         Quarter = if_else(bot %in% c(.5,1.5), "Q4", "Q1,Q2,Q3")) |> 
  filter(bot == "Q1, Q2, Q3" | bot == "Q4") |> 
  ggplot(aes(x = bot, y = tau , weight = weight, fill = bot)) +
  geom_boxplot(outlier.size = .5,
  ) +
  facet_grid(field_region ~ wp_group, 
             labeller = label_bquote(cols = "WP Group =" ~ .(wp_group)),
             switch = "y") +  
  scale_fill_viridis_d(name = "",
                       option = "D") +
  # scale_fill_manual(name = "Field Region",
  #                   values = c(rgb(1,.65,0,.5),
  #                              rgb(0,0,.5,.5))) +
  xlab("") +
  ylab(expression(tau)) +
  scale_y_continuous(position = "right") + 
  theme_bw() +
  theme(strip.background.x = element_rect(colour = 'black', fill = NA),
        strip.background.y = element_rect(colour = 'black', fill = NA)) +
  guides(fill = "none") 


pdf(file = "./figures/fig_6b.pdf", width = 8.5, height = 6.5)
tau_hat_intervals_plot
dev.off()













## 4th Down Bot Q123---- 
estimate_wp_policy <- list()
for(i in 1:length(wp_groups)){
  estimate_wp_policy[[i]] = clean_pbp |> 
    filter(down == 4) |>
    filter(wp_group == wp_groups[i],
           qtr %in% c(1,2,3)) |>
    mutate(ydstogo_group = yard_cut(ydstogo, upper = MAX_ydstogo, by = ydstogo_bin), 
           yardline_100_group = yard_cut(yardline_100, upper = MAX_yardline_100, by = yardline_100_bin)) |> 
    mutate(max_value =  pmax(go_wp, fg_wp, punt_wp, na.rm = T),
           fourth_down_decision = case_when( max_value == go_wp ~ "go",
                                             max_value == fg_wp ~ "fga",
                                             max_value == punt_wp ~ "punt",
                                             TRUE ~ NA_character_
           )
    ) |> 
    select(down,ydstogo_group, yardline_100_group, max_value, fourth_down_decision) |> 
    filter(!is.na(fourth_down_decision)) |>
    mutate(fourth_down_decision = as.factor(fourth_down_decision)) |>
    group_by(ydstogo_group, yardline_100_group, fourth_down_decision) |>
    summarise(count_d= n()) |>
    group_by(ydstogo_group, yardline_100_group) |>
    summarise(
      max_d_count = max(count_d), 
      pct = max(count_d)/sum((count_d)),
      max_decision = fourth_down_decision[which.max(count_d)]) |> 
    ungroup() |> 
    bind_rows(data.frame(ydstogo_group = "1", yardline_100_group = "91+",
                         max_d_count = NA,
                         pct = NA,
                         max_decision = NA)) |>
    mutate(ydstogo_group = factor(ydstogo_group, levels = ydstogo_label)) |>
    mutate(pct_cat = cut(pct, breaks = seq(0.3, 1, 0.1), include.lowest = T, right = F),
           wp_group = wp_groups[i]) 
}
estimate_wp_policy <- bind_rows(estimate_wp_policy)

## 4th Down Bot Q4---- 
estimate_wp_policy_Q4 <- list()
for(i in 1:length(wp_groups)){
  estimate_wp_policy_Q4[[i]] = clean_pbp |> 
    filter(down == 4) |>
    filter(wp_group == wp_groups[i],
           qtr == 4) |>
    mutate(ydstogo_group = yard_cut(ydstogo, upper = MAX_ydstogo, by = ydstogo_bin), 
           yardline_100_group = yard_cut(yardline_100, upper = MAX_yardline_100, by = yardline_100_bin)) |> 
    mutate(max_value =  pmax(go_wp, fg_wp, punt_wp, na.rm = T),
           fourth_down_decision = case_when( max_value == go_wp ~ "go",
                                             max_value == fg_wp ~ "fga",
                                             max_value == punt_wp ~ "punt",
                                             TRUE ~ NA_character_
           )
    ) |> 
    select(down,ydstogo_group, yardline_100_group, max_value, fourth_down_decision) |> 
    filter(!is.na(fourth_down_decision)) |>
    mutate(fourth_down_decision = as.factor(fourth_down_decision)) |>
    group_by(ydstogo_group, yardline_100_group, fourth_down_decision) |>
    summarise(count_d= n()) |>
    group_by(ydstogo_group, yardline_100_group) |>
    summarise(
      max_d_count = max(count_d), 
      pct = max(count_d)/sum((count_d)),
      max_decision = fourth_down_decision[which.max(count_d)]) |> 
    ungroup() |> 
    bind_rows(data.frame(ydstogo_group = "1", yardline_100_group = "91+",
                         max_d_count = NA,
                         pct = NA,
                         max_decision = NA)) |>
    mutate(ydstogo_group = factor(ydstogo_group, levels = ydstogo_label)) |>
    mutate(pct_cat = cut(pct, breaks = seq(0.3, 1, 0.1), include.lowest = T, right = F),
           wp_group = wp_groups[i]) 
}

estimate_wp_policy_Q4 <- bind_rows(estimate_wp_policy_Q4)


plot_a <- observed_policy_wp_Q123 |> 
  select(ydstogo_group, yardline_100_group, max_decision, pct_cat, wp_group) |> 
  mutate(bot = 0) |> 
  bind_rows(observed_policy_wp_Q4 |> 
              select(ydstogo_group, yardline_100_group, max_decision, pct_cat, wp_group) |> 
              mutate(bot = .5)) |> 
  bind_rows(estimate_wp_policy |> 
              select(ydstogo_group, yardline_100_group, max_decision, pct_cat, wp_group) |> 
              mutate(bot = 1)) |> 
  bind_rows(estimate_wp_policy_Q4 |> 
              select(ydstogo_group, yardline_100_group, max_decision, pct_cat, wp_group) |> 
              mutate(bot = 1.5)) |> 
  mutate(bot = factor(bot, levels = c(0,.5, 1, 1.5), 
                      labels = c("Leage Agg.\nQ1, Q2, Q3", "Leage Agg.\nQ4","4th Down Bot\nQ1, Q2, Q3", "4th Down Bot\nQ4"))) |> 
  plot_decision_map(fill_variable = "discrete", 
                    legend_name = "",
                    fill = max_decision, alpha = pct_cat, na.value = "grey") + 
  scale_y_discrete(position = "right") + 
  facet_grid(wp_group~bot, 
             labeller = label_bquote(rows = "WP Group =" ~ .(wp_group)),
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
        axis.text.x = element_text(color = "black", size = 6),
        axis.text.y = element_text(color = "black", size = 8),
        axis.text=element_text(size=10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.margin=margin(t=10,b=10,l=0))+
  labs(x="Decision",y="Frequency") 

g.legend_2 = ggpubr::ggarrange(NULL, NULL, NULL,g.legend_alt1, NULL , NULL,
                               widths = c(.3,.15,.3,.15,.3,.15) ,
                               heights = c(0.8, 0.5, 0.8, 0.8, 0.5, 0.8),
                               ncol = 2, nrow = 3)

ggpubr::ggarrange(g.legend_2,
                  plot_a, 
                  widths = c(.7,1.25), nrow = 1)
ggsave("./figures/fig_6a.pdf", width = 10.5, height = 6.5)


# Plot b --------------------------------------------------------------------


tau_hat_intervals_plot <- tau_hat_df |> 
  mutate(bot = factor(bot, levels = c(0,.5, 1, 1.5),
                       labels = c("Leage Agg.\nQ1, Q2, Q3", "Leage Agg.\nQ4","4th Down Bot\nQ1, Q2, Q3", "4th Down Bot\nQ4")),
         field_region = factor(field_region,
                               labels = c("Opp Half", "Own Half")),
         Bot2 = if_else(bot %in% c(1,1.5), "4th Down Bot", "League Agg.") ,
         Quarter = if_else(bot %in% c(.5,1.5), "Q4", "Q1,Q2,Q3")) |> 
  ggplot(aes(x = field_region, y = tau , weight = weight, fill = field_region)) +
  geom_boxplot(outlier.size = .5,
               ) +
  facet_grid(wp_group~bot, 
             labeller = label_bquote(rows = "WP Group =" ~ .(wp_group)),
             switch = "y") +  
  scale_fill_manual(name = "Field Region",
                     values = c(rgb(1,.65,0,.5),
                                rgb(0,0,.5,.5))) +
  xlab("") +
  ylab(expression(tau)) +
  scale_y_continuous(position = "right") + 
  theme_bw() +
  theme(strip.background.x = element_rect(colour = 'black', fill = NA),
       strip.background.y = element_rect(colour = 'black', fill = NA)) +
  guides(fill = "none") 


pdf(file = "./figures/fig_6b.pdf", width = 8.5, height = 6.5)
tau_hat_intervals_plot
dev.off()


# tau_hat_df |> 
# group_by(bot, field_region, wp_group) %>%
#   summarize(tau_med = matrixStats::weightedMedian(tau, weight))


