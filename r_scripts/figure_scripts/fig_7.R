library(glue)
library(reldist)
library(ggtext)

source("./r_scripts/utils.R")
source("./r_scripts/constant.R")

# Load data and model output ---------------------------------------

clean_pbp <- readRDS("./data/pbp_2014_2022.rds")
state_colnames <- readRDS("./data/state_colnames.rds")
fourth_states <- play_states |>
  filter(down == 4)

tau_hat_wp_bot <- readRDS("./output/all_dec/tau_hat_wp_bot.rds") # For 4th Down Bot
tau_hat_wp_seas <- readRDS("./output/all_dec/tau_hat_wp_seas.rds") # For League Behavior

# Plot ----------------------------------------------------------

tau_hat_seas_plot_df <- bind_rows(tau_hat_wp_bot,
                                  tau_hat_wp_seas |>
                                    filter(wp_group != "(0,0.2] - Q4") |> 
                                    mutate(wp_group = if_else(wp_group == "(0,0.2] - Q123",
                                                              "(0,0.2]",
                                                              wp_group),
                                           season = as.character(season))) |>
  mutate(field_region = factor(field_region,
                               labels = c("Opp Half", "Own Half")),
         season = as.factor(season))

tau_hat_seas_plot <- tau_hat_seas_plot_df |> 
  ggplot(aes(x = tau, y = season, weight = weight, fill = field_region)) +
  geom_boxplot(
    outlier.size = .5
    #outlier.shape = NA
    ) +
  facet_grid(field_region~wp_group, 
             labeller = label_bquote(cols = "WP Group =" ~ .(wp_group))) +  
  scale_fill_manual(name = "Field Region",
                    values = c(rgb(1,.65,0,.5),
                               rgb(0,0,.5,.5))) +
  #scale_y_discrete(labels= function(x) highlight(x, "4th Down Bot", "black")) +
  scale_x_continuous(limits=c(.2,.8), breaks = seq(.2, .8, by = .1)) +
  ylab("") +
  xlab(expression(tau)) + 
  theme_bw() +
  theme(strip.background.x = element_rect(colour = 'black', fill = NA),
        strip.background.y = element_rect(colour = 'black', fill = NA),
        #axis.text.y = element_markdown(size = 9)
        ) +
  guides(fill = "none")

pdf(file = "./figures/fig_7.pdf", width = 6.5*1.5, height = 4*1.5)
tau_hat_seas_plot
dev.off()


