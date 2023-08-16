# Source utils script and constants
source("./r_scripts/utils.R")
source("./r_scripts/constant.R")

clean_pbp <- readRDS("./data/pbp_2014_2022.rds")
fourth_states <- play_states |>
  filter(down == 4)

# League aggregate most frequent observed policy ----

observed_decision_map = clean_pbp |>
  get_observed_policy(fourth_state_df = fourth_states) |>
  mutate(ydstogo_group = factor(ydstogo_group, levels = ydstogo_label)) |>
  mutate(pct = obs_max_n/obs_total_n) |> 
  mutate(max_decision = observed_decision) |> 
  mutate(pct_cat = cut(pct, breaks = seq(0.4, 1, 0.1), include.lowest = T, right = F)) |>
  plot_decision_map(fill_variable = "discrete",
                    fill = max_decision,
                    legend_name = "",
                    alpha = pct_cat) +
  ggtitle("Observed decisions from NFL games") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  theme(legend.position = "none") +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text( size = 15)) + 
  theme(text = element_text(size=15)) +
  theme(aspect.ratio = 1) 


# Most frequent 4th down bot policy ----------------------------------

estimate_wp_policy = clean_pbp |> 
  filter(down == 4) |>
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
  mutate(pct_cat = cut(pct, breaks = seq(0.3, 1, 0.1), include.lowest = T, right = F)) |> 
  plot_decision_map(fill_variable = "discrete", 
                    legend_name = "",
                    fill = max_decision, alpha = pct_cat, na.value = "grey") + 
  ggtitle("Optimal decisions based on estimated win probability") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text( size = 15)) + 
  theme(text = element_text(size=15)) +
  theme(aspect.ratio = 1) +
  theme(legend.position = "none") 


# Bivariate legend ---------------------------------------------------

d = expand.grid(qnt =  c("40-49%", "50-59%", "60-69%", "70-79%","80-89%", "90-100%"),
                decision = c("field goal", "go for it", "punt")
) |> 
  arrange(decision, qnt)
d$x = rep(1:3, each = 6)
d$y = rep(seq(1,6,1), 3)
d$pct = rep(seq(0.4, 0.9, 0.1), 3)

g.legend <- ggplot()+
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
                     labels = c("FGA", "GO", "PUNT"))+
  theme(legend.position="none",
        panel.background=element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        axis.text=element_text(size=20),
        axis.title.x = element_text(size = 16),
        plot.margin=margin(t=10,b=10,l=10))+
  labs(x="Decision",y="Frequency") 


### combine 3 plots together

g.legend_2 = ggpubr::ggarrange(NULL, g.legend, NULL,
                               widths = c(0.5),
                               heights = c(0.2, 0.5, 0.2),
                               ncol = 1, nrow = 3)

ggpubr::ggarrange(observed_decision_map, estimate_wp_policy,
                  g.legend_2, 
                  widths = c(0.7,0.7,0.3), nrow = 1)
ggsave("./figures/fig_1.pdf", width = 18, height = 7)


