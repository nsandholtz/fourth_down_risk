source("./r_scripts/utils.R")
source("./r_scripts/constant.R")
n_boot <- 200

# Load and format quantile policy output --------------------------------------------

output_files <- system("ls ./output", intern = T) 
output_inds <- which(str_detect(output_files, "boot_league_avg_policies_") == T)

boot_full_policies <- list()
for(i in 1:length(output_files[output_inds])){
  boot_full_policies[[i]] <- bind_rows(readRDS(paste0("./output/",
                                                      output_files[output_inds][i])))
}

# Appendix plot -----------------------------------------------------------

boot_full_policies_long <- bind_rows(boot_full_policies) |>
  arrange(play_state, tau, boot_ind) 

plot_df <- boot_full_policies_long |>
  group_by(ydstogo_group, yardline_100_group, tau) |>
  summarise(n_PUNT = sum(max_decision == "PUNT", na.rm = T),
            n_FGA = sum(max_decision == "FGA", na.rm = T),
            n_GO = sum(max_decision == "GO", na.rm = T)) |>
  arrange(tau) |>
  mutate(max_value =  pmax(n_PUNT, n_FGA, n_GO, na.rm = T),
         fourth_down_decision = case_when( max_value == n_GO ~ "go",
                                           max_value == n_FGA ~ "fga",
                                           max_value == n_PUNT ~ "punt",
                                           TRUE ~ NA_character_
         ),
         pct = max_value/n_boot) |>
  mutate(fourth_down_decision = as.factor(fourth_down_decision)) |>
  ungroup()

plot_df$pct[plot_df$max_value == 0] <- NA
plot_df$fourth_down_decision[plot_df$max_value == 0] <- NA

boot_arg_max_plot_appendix <- plot_df |> 
  mutate(pct_cat = cut(pct, breaks = seq(0.3, 1, 0.1), include.lowest = T, right = F)) |>
  plot_decision_map(., fill_variable = "discrete", 
                    legend_name = "",
                    fill = fourth_down_decision, alpha = pct_cat, na.value = "grey") +
  facet_wrap(~tau, labeller = label_bquote(cols = tau ~ "=" ~ .(tau)),
             nrow = 8) + 
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) + 
  theme_classic() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1,
      size = 4
    ),
    axis.text.y = element_text(
      size = 4
    ),
    strip.text.x = element_text(size = 5, margin = margin(.05,0,.05,0, "cm"))
  ) + 
  theme(aspect.ratio = 1) +
  theme(legend.position = "none") 

pdf(file = "./figures/fig_C3.pdf", height = 10, width = 7)
boot_arg_max_plot_appendix
dev.off()


