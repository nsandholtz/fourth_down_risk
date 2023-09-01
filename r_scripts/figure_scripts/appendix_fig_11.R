source("./r_scripts/constant.R")
fourth_states <- play_states %>%
  filter(down == 4)

# Plot ---------------------------------------------------------------

vertex <- c(6,6)

part.labs <- c(bquote(rho[1]), bquote(rho[2]), bquote(rho[3]), bquote(rho[4]))
names(part.labs) <- 1:4

partition_visual <- fourth_states |>
  mutate(part_1 = as.numeric(ydstogo_group) < vertex[1] & as.numeric(yardline_100_group) < vertex[2],
         part_2 = as.numeric(ydstogo_group) < vertex[1] & as.numeric(yardline_100_group) >= vertex[2],
         part_3 = as.numeric(ydstogo_group) >= vertex[1] & as.numeric(yardline_100_group) < vertex[2],
         part_4 = as.numeric(ydstogo_group) >= vertex[1] & as.numeric(yardline_100_group) >= vertex[2]) |>
  pivot_longer(cols = starts_with("part_"),
               names_to = "partition",
               names_prefix = "part_",
               values_to = "part") |>
  arrange(partition) |>
  mutate(part = as.factor(ifelse(part == T,1,2))) |>
  ggplot(aes(x = yardline_100_group, y = ydstogo_group, fill = part)) +
  geom_tile(colour = "white", size = 0.1) +
  scale_fill_discrete(
    name = " ",
    labels = c(as.expression(bquote(P[1])), as.expression(bquote(P[2])))
  ) +
  annotate("point",5.5,5.5) +
  theme_classic() +
  xlab("Yards to opponent endzone") +
  ylab("Yards to go") + 
  coord_fixed() + 
  theme(axis.text.x = element_text(angle = 45, 
                                   size = 8, 
                                   hjust = 1)) +
  facet_wrap(~partition, nrow = 1, labeller = label_bquote(cols = rho[.(partition)](x,y)))

pdf(file = "./figures/appendix_fig_11.pdf", height = 3, width = 10)
partition_visual
dev.off()
