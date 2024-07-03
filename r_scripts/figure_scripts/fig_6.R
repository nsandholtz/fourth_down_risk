source("./r_scripts/utils.R")
source("./r_scripts/constant.R")

library(lemon)

# Load data and model output ---------------------------------------

clean_pbp <- readRDS("./data/pbp_2014_2022.rds")
state_colnames <- readRDS("./data/state_colnames.rds")
fourth_states <- play_states |>
  filter(down == 4)

wp_groups2 <- clean_pbp$wp |> 
  cut(seq(0,1, by = .05), include.lowest = F, right = T) |> 
  sort() |> 
  as.character() |> 
  unique()

clean_pbp <- clean_pbp |> 
  mutate(wp_group2 = as.character(cut(wp, seq(0,1, by = .05),
                                      include.lowest = F, right = T)))

# Plot a -------------------------------------------------------------

## League Avg ----

observed_policy_wp <- list()
for(i in 1:length(wp_groups2)){
  observed_policy_wp[[i]] <- clean_pbp |> 
    filter(wp_group2 == wp_groups2[i]) |>
    get_observed_policy(fourth_state_df = fourth_states) |> 
    mutate(ydstogo_group = factor(ydstogo_group, levels = ydstogo_label),
           wp_group2 = wp_groups2[i], 
           pct = obs_max_n/obs_total_n,
           max_decision = observed_decision,
           pct_cat = cut(pct, breaks = seq(0.4, 1, 0.1), include.lowest = T, right = F))
}
observed_policy_wp <- bind_rows(observed_policy_wp)

## 4th Down Bot ---- 
estimate_wp_policy <- list()
for(i in 1:length(wp_groups2)){
  estimate_wp_policy[[i]] = clean_pbp |> 
    filter(down == 4) |>
    filter(wp_group2 == wp_groups2[i]) |>
    mutate(ydstogo_group = yard_cut(ydstogo, upper = MAX_ydstogo, by = ydstogo_bin), 
           yardline_100_group = yard_cut(yardline_100, upper = MAX_yardline_100, by = yardline_100_bin)) |> 
    mutate(max_value =  pmax(go_wp, fg_wp, punt_wp, na.rm = T),
           fourth_down_decision = case_when( max_value == go_wp ~ "go",
                                             max_value == fg_wp ~ "fga",
                                             max_value == punt_wp ~ "punt",
                                             TRUE ~ NA_character_
           )
    ) |> 
    dplyr::select(down,ydstogo_group, yardline_100_group, max_value, fourth_down_decision) |> 
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
    mutate(pct_cat = cut(pct, breaks = seq(0.4, 1, 0.1), include.lowest = T, right = F),
           wp_group2 = wp_groups2[i]) 
}
estimate_wp_policy <- bind_rows(estimate_wp_policy)


## Combined -----------------------------------------------------------

plot_a <- observed_policy_wp |> 
  dplyr::select(ydstogo_group, yardline_100_group, max_decision, pct_cat, wp_group2) |> 
  mutate(bot = 0) |> 
  bind_rows(estimate_wp_policy |> 
              dplyr::select(ydstogo_group, yardline_100_group, max_decision, pct_cat, wp_group2) |> 
              mutate(bot = 1)) |> 
  mutate(bot = factor(bot, levels = c(0, 1), 
                      labels = c("Leage Agg.", "4th Down Bot")) |> 
           fct_rev()) |> 
  filter(wp_group2 %in% c("(0,0.05]", "(0.45,0.5]", "(0.95,1]")) |> 
  plot_decision_map(fill_variable = "discrete", 
                    legend_name = "",
                    fill = max_decision, alpha = pct_cat, na.value = "grey") + 
  scale_y_discrete(position = "right") + 
  facet_grid(bot ~ wp_group2, 
             labeller = label_bquote(cols = "WP Group =" ~ .(wp_group2)),
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

ggsave("./figures/fig_6a.pdf", width = 6, height = 3.5)



# Plot b ---------------------------------------------------------

## Computing confidence intervals -------------------------------------

tau_hat_wp2_LA <- readRDS(file = "./output/tau_hat_wp_twenty.rds") |> 
  mutate(bot = 0)
tau_hat_wp2_bot <- readRDS(file = "./output/tau_hat_wp_twenty_bot.rds") |> 
  mutate(bot = 1)

tau_hat_wp2 <- bind_rows(tau_hat_wp2_LA, tau_hat_wp2_bot) |> 
  mutate(bot = factor(bot, levels = c(0, 1), 
                      labels = c("League Agg.", "4th Down Bot")) |> 
           fct_rev())
  
q_ribbon <- tau_hat_wp2 |> 
  group_by(field_region, wp, bot) |> 
  summarize(lower = wtd.quantile(tau, q = .025, weight = weight),
            upper = wtd.quantile(tau, q = .975, weight = weight),
            fit = weighted.mean(tau, w = weight)) 

plot_b <- tau_hat_wp2 |> 
  ggplot(aes(x = wp, fill = field_region)) +
  lemon::geom_pointline(aes(y = fit, color = field_region), 
            data = q_ribbon,
            size = .5,
            show.legend = F) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              color = NA,
              data = q_ribbon) +
  geom_point(aes(y = fit),
             data = q_ribbon |>
               filter(wp < .03 | near(wp, .475) | wp > .95),
             color = "red",
             show.legend = F) +
  facet_wrap(~ bot, nrow = 2) +
  scale_fill_manual(name = "Field Region", 
                    labels = c(expression("Opponent Half (" * hat(tau)[1] * ")"),
                               expression("Own Half (" * hat(tau)[2] * ")")),
                    values = c(rgb(1,.65,0,.25), rgb(0,0,.5,.25))) +
  scale_color_manual(values = c(rgb(1,.65,0,.75), rgb(0,0,.5,.75))) +
  scale_x_continuous(limits=c(0,1), breaks = seq(0, 1, by = .1)) +
  scale_y_continuous(limits=c(.2,.8), breaks = seq(.2, .8, by = .1)) +
  xlab("Win Probability") +
  ylab(expression(hat(tau))) + 
  theme_bw() +
  theme(strip.background = element_rect(colour = 'black', fill = NA)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(angle = 0, vjust = .5)) +
  theme(legend.position = c(.73,.35),
        legend.background = element_rect(fill='transparent'),
        legend.title = element_text(size = 8), 
        legend.text = element_text(size = 8))

plot_b
ggsave("./figures/fig_6b.pdf", width = 3, height = 5)


# Plot c ------------------------------------------------------------------

diff_OPP <- tau_hat_wp2 |> 
  dplyr::select(-min_loss) |> 
  filter(field_region == "OPP")|> 
  dplyr::select(-field_region)

diff_OWN <- tau_hat_wp2 |> 
  dplyr::select(-min_loss) |> 
  filter(field_region == "OWN") |> 
  dplyr::select(-field_region)

diff_bot <- tau_hat_wp2 |> 
  dplyr::select(-min_loss) |> 
  filter(bot == "4th Down Bot")|> 
  dplyr::select(-bot)

diff_la <- tau_hat_wp2 |> 
  dplyr::select(-min_loss) |> 
  filter(bot == "League Agg.") |> 
  dplyr::select(-bot)


field_region_diff <- full_join(diff_OPP, diff_OWN, by = join_by(wp_group, wp, boot_ind, bot),
                      relationship = "many-to-many") |> 
  mutate(tau_diff = tau.x - tau.y) |> 
  group_by(wp, bot, boot_ind) |> 
  mutate(weight = 1/n()) |> 
  dplyr::select(boot_ind, wp_group, wp, bot, tau_diff, weight) |> 
  group_by(wp, wp_group, bot) |> 
  summarize(lower = wtd.quantile(tau_diff, q = .025, weight = weight),
            upper = wtd.quantile(tau_diff, q = .975, weight = weight),
            fit = weighted.mean(tau_diff, w = weight)) |> 
  mutate(bot_field_region = bot) |> 
  dplyr::select(-bot)

bot_diff <- full_join(diff_bot, diff_la, by = join_by(wp_group, wp, boot_ind, field_region),
                               relationship = "many-to-many") |> 
  filter(boot_ind <= 20) |> 
  mutate(tau_diff = tau.x - tau.y) |> 
  group_by(wp, field_region, boot_ind) |> 
  mutate(weight = 1/n()) |> 
  dplyr::select(boot_ind, wp_group, wp, field_region, tau_diff, weight) |> 
  group_by(wp, wp_group, field_region) |> 
  summarize(lower = wtd.quantile(tau_diff, q = .025, weight = weight),
            upper = wtd.quantile(tau_diff, q = .975, weight = weight),
            fit = weighted.mean(tau_diff, w = weight)) |> 
  mutate(bot_field_region = field_region) |> 
  dplyr::select(-field_region)

bot_field_region_diff <- bind_rows(field_region_diff, bot_diff) |> 
  mutate(bot_field_region = fct_relevel(bot_field_region,
                                        "League Agg.",
                                        "4th Down Bot",
                                        "OPP"))

facet_names <- c(
  `League Agg.` = ~hat(tau)[OPP]^"League" - hat(tau)[OWN]^"League",
  `4th Down Bot` = ~hat(tau)[OPP]^"Bot" - hat(tau)[OWN]^"Bot",
  `OPP` = ~hat(tau)[OPP]^"Bot" - hat(tau)[OPP]^"League",
  `OWN` = ~hat(tau)[OWN]^"Bot" - hat(tau)[OWN]^"League"
)

plot_c <- bot_field_region_diff |> 
  mutate(Difference = "Difference") |> 
  ggplot(aes(x = wp)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = gray(.5, .5)) +
  lemon::geom_pointline(aes(y = fit), 
                        size = .5,
                        show.legend = F) +
  facet_wrap(~ bot_field_region, nrow = 1, 
             labeller = label_bquote(.(as.expression(
               eval(parse(text = paste0('facet_names', '$`', bot_field_region, '`')))
             )))) + 
  scale_x_continuous(limits=c(0,1), breaks = seq(0, 1, by = .1)) +
  scale_y_continuous(limits=c(-.15,.5), breaks = seq(-.1, .5, by = .1)) +
  xlab("Win Probability") +
  ylab("Difference") + 
  theme_bw() +
  #ggtitle(label = expression(hat(tau)[1]^"Bot" - hat(tau)[2]^"Bot")) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.background = element_rect(colour = 'black', fill = NA),
        strip.text = element_text(size = 12))


pdf(file = "./figures/fig_6c.pdf", 
    width = 10, height = 3)
plot_c
dev.off()

