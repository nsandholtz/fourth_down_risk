source("./r_scripts/utils.R")
source("./r_scripts/constant.R")

# Load data ---------------------------------------

clean_pbp <- readRDS("./data/pbp_2014_2022.rds")
state_colnames <- readRDS("./data/state_colnames.rds")
fourth_states <- play_states |> 
  filter(down == 4)


# Observed decision policy plot -------------------------------------------

observed_action_plot <- clean_pbp  |>  
  filter(qtr %in% c(1,3)) |> 
  get_observed_policy(fourth_state_df = fourth_states) |> 
  mutate(decision = str_to_upper(observed_decision)) |>
  ggplot() +
  geom_tile(aes(y=ydstogo_group, x=yardline_100_group),fill = "white",
            colour="gray",linewidth=0.2, alpha = 0.5
  ) +
  geom_point(data = clean_pbp |>   
               filter(down == 4, qtr %in% c(1,3)) |> 
               mutate(decision = str_to_upper(fourth_down_decision)), aes(y=ydstogo_group, 
                                                                          x=yardline_100/10 + 0.5,
                                                                          color = decision),
             size = 0.9,
             alpha = 0.5,
             pch = 16,
             show.legend = T,
             position=position_jitter(w=.1, seed = 123)
  ) + 
  scale_color_manual(values=c("#d53e4f","gold","#2C7FB8"),
                     limits = c("GO", "FGA", "PUNT"),
                     na.value = "grey",
                     name = "Decision") +
  theme_classic() +
  xlab("Yards to opponent endzone") +
  ylab("Yards to go") +
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
    strip.text.y.right = element_text(angle = 90)
  ) +
  theme(legend.position="right") + 
  coord_fixed(clip = "off") +
  geom_vline(xintercept=5.5, linetype="dashed", 
             color = gray(.1), linewidth=1) + 
  annotate(geom="text", x=3, y=10.75, label=list('paste(P[1]," (Opponent Half)")'),
           parse = T,
           color="black") +
  annotate(geom="text", x=8, y=10.75, label=list('paste(P[2]," (Own Half)")'),
           parse = T,
           color="black") +
  guides(colour = guide_legend(override.aes = list(size=5)))

png(file = "./figures/appendix_fig_13a.png", units="in", width=7, height=5.5, res=300)
observed_action_plot
dev.off()

# Loss curve plots ----------------------------------------------------------------

league_agg_loss <- get_loss_curve(clean_pbp, 
                                  tau_vec = round(seq(.01,.99,by = .01), digits = 2)) |> 
  as.data.frame()

loss_long <- league_agg_loss |>
  rename("Own half" = PUNT, 
         "Opp half" = FG) |>
  pivot_longer(cols = c("Own half", "Opp half"),
               names_to = "Field Region",
               values_to = "Loss")


min_OWN <- league_agg_loss[league_agg_loss$PUNT == min(league_agg_loss$PUNT), c("tau", "PUNT")]
min_OPP <- league_agg_loss[league_agg_loss$FG == min(league_agg_loss$FG), c("tau", "FG")]


p1 <- loss_long |>
  ggplot(aes(tau, Loss, color = `Field Region`)) +
  geom_line(linewidth = 2) + 
  scale_color_manual(values = c(rgb(1,.65,0,.75),
                                 rgb(0,0,.5,.75)), 
                     labels = c(expression(P[1]), expression(P[2]))) + 
  geom_segment(data = min_OWN[1,],
               aes(x = 0, 
                   y = PUNT, 
                   xend = 1,
                   yend = PUNT),
               color = rgb(0,0,.5,.5), linewidth = 1, lty="11") +
  geom_segment(data = min_OPP[1,],
               aes(x = 0, 
                   y = FG, 
                   xend = 1,
                   yend = FG),
               color = rgb(1,.65,0,.75), linewidth = 1, lty="11") +
  annotate(geom = "text", x = .2, y = .02,
           label = list('paste(hat(tau)[2] %in% "[0.02, 0.36] ", union("[0.39, 0.40]"))'),
           parse = T, size = 4) + 
  annotate(geom = "text", x = .64, y = .09,
           label = list('hat(tau)[1] %in% "[0.41, 0.43]"'),  
           parse = T, size = 4) + 
  theme_minimal() + 
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 1, by = .1)) + 
  xlab(expression(tau)) 

pdf("./figures/appendix_fig_13b.pdf",
    width = 7, height = 5.5)
p1
dev.off()


