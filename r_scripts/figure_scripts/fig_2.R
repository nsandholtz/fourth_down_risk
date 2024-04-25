source("./r_scripts/utils.R")
source("./r_scripts/constant.R")

clean_pbp <- readRDS("./data/pbp_2014_2022.rds")
state_colnames <- readRDS("./data/state_colnames.rds")

# Prep data ---------------------------------------------------------------

my_dat_a1 = clean_pbp |>
  mutate(ns_ep = lead(ep),
         ch_poss = lead(posteam) != posteam,
         ns_ep2 = if_else(ch_poss, 
                       if_else(sp == 1, ns_ep*-1 + 6.95, ns_ep*-1),
                       ns_ep)) |> 
  filter(play_state == "4_8_31-40",
         !is.na(fourth_down_decision),
         fourth_down_decision == "go") |>
  dplyr::select(fourth_down_decision, ns_ep2) 

my_dat_a2 = clean_pbp |>
  mutate(ns_ep = lead(ep),
         ch_poss = lead(posteam) != posteam,
         ns_ep2 = if_else(ch_poss, 
                       if_else(sp == 1, ns_ep*-1 + 3, ns_ep*-1),
                       ns_ep)) |> 
  filter(play_state == "4_8_31-40",
         !is.na(fourth_down_decision),
         fourth_down_decision == "fga") |>
  dplyr::select(fourth_down_decision, ns_ep2) 

my_dat_a3 = clean_pbp |>
  mutate(ns_ep = lead(ep),
         ch_poss = lead(posteam) != posteam,
         ns_ep2 = if_else(ch_poss, 
                       if_else(sp == 1, ns_ep - 6.95, ns_ep*-1),
                       ns_ep)) |> 
  filter(play_state == "4_8_31-40",
         !is.na(fourth_down_decision),
         fourth_down_decision == "punt") |>
  dplyr::select(fourth_down_decision, ns_ep2) 


# "Go for it" density plot --------------------------------------------------

a1 = ggplot(data = my_dat_a1, 
            aes(x = ns_ep2)) +
  stat_density(trim = F, aes(fill = fourth_down_decision, color = fourth_down_decision), 
               adjust = 1/5
               ) +
  geom_rug() + 
  guides(fill = "none") +
  guides(color = "none") +
  scale_fill_manual(values = c("red")) +
  scale_color_manual(values = c("red")) +
  theme_gray() +
  xlab(" ") + 
  scale_x_continuous(breaks = c(-6,-4,-2,0,2,4,6), limits = c(-7,7.5)) + 
  ylab("Density") + 
  ggtitle("Next State Expected Points Distribution ") +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)) +
  facet_wrap(~fourth_down_decision, 
             ncol = 1, 
             labeller = labeller(fourth_down_decision = c('punt' = "PUNT", 'fga' = "FGA", 'go' = "Go for it"))) + 
  geom_vline(xintercept = c(mean(my_dat_a1$ns_ep2)),
             color = "black",
             linewidth = 0.8, linetype = c(2)) + 
  annotate("text", 
           x = c(mean(my_dat_a1$ns_ep2)) + .1,
           y = .35, label = bquote("Empirical Mean = " ~ .(round(mean(my_dat_a1$ns_ep2),2))), hjust = 0, size = 3) + 
  theme(strip.text = element_text(size=10)) +
  theme(strip.background = element_rect(colour = NA, fill = gray(.85))) 


# FGA density plot --------------------------------------------------------

a2 = ggplot(data = my_dat_a2, 
            aes(x = ns_ep2)) +
  stat_density(trim = F, aes(fill = fourth_down_decision, color = fourth_down_decision), adjust = 1/4) +
  geom_rug() +
  guides(fill = "none") +
  guides(color = "none") +
  scale_fill_manual(values = c("gold")) +
  scale_color_manual(values = c("gold")) +
  theme_bw() + 
  theme(
    plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-6,-4,-2,0,2,4,6), limits = c(-7,7.5)) + 
  xlab(" ") + 
  ylab("Density") + 
  facet_wrap(~fourth_down_decision, 
             ncol = 1, 
             labeller = labeller(fourth_down_decision = c('punt' = "Punt", 'fga' = "Field goal attempt", 'go' = "Go for it"))) + 
  geom_vline(xintercept = c(mean(my_dat_a2$ns_ep2)),
             color = "black",
             linewidth = 0.8, linetype = c(2)) + 
  annotate("text", 
           x = c(mean(my_dat_a2$ns_ep2)) - .1,
           y = .6, label = bquote("Empirical Mean = " ~ .(round(mean(my_dat_a2$ns_ep2),2))), hjust = 1, size = 3) + 
  theme(strip.text = element_text(size=10)) +
  theme(strip.background = element_rect(colour = NA, fill = gray(.85))) 


# Punt density plot -------------------------------------------------------

a3 = ggplot(data = my_dat_a3, 
            aes(x = ns_ep2)) +
  stat_density(trim = F, aes(fill = fourth_down_decision, color = fourth_down_decision), adjust = 1) +
  geom_rug() + 
  guides(fill = "none") +
  guides(color = "none") +
  scale_fill_manual(values = c("blue")) +
  scale_color_manual(values = c("blue")) +
  theme_bw() +
  scale_x_continuous(breaks = c(-6,-4,-2,0,2,4,6), limits = c(-7,7.5)) + 
  xlab("Expected points") + 
  ylab("Density") + 
  facet_wrap(~fourth_down_decision, 
             ncol = 1, 
             labeller = labeller(fourth_down_decision = c('punt' = "Punt", 'fga' = "Field goal attempt", 'go' = "Go for it"))) + 
  geom_vline(xintercept = c(mean(my_dat_a3$ns_ep2)),
             color = "black",
             linewidth = 0.8, linetype = c(2)) + 
  annotate("text", 
           x = c(mean(my_dat_a3$ns_ep2)) + .74,
           y = .52, label = bquote("Empirical Mean = " ~ .(round(mean(my_dat_a3$ns_ep2),2))), hjust = 0, size = 3) + 
  theme(strip.text = element_text(size=10)) +
  theme(strip.background = element_rect(colour = NA, fill = gray(.85))) 


lhs = cowplot::plot_grid(a1,a2,a3, nrow = 3)
pdf("./figures/fig_2a.pdf",
    width = 5.5, height = 5.5)
lhs
dev.off()


# Quantile function plot --------------------------------------------------

a1_dens = density(my_dat_a1$ns_ep2, adjust = 1/5)
a1_quant_f = data.frame(value = a1_dens$x,
                        quant = cumsum(a1_dens$y*c(lead(a1_dens$x) - a1_dens$x)[1])) |> 
  filter(quant <= 1)

a2_dens = density(my_dat_a2$ns_ep2, adjust = 1/4)
a2_quant_f = data.frame(value = a2_dens$x,
                        quant = cumsum(a2_dens$y*c(lead(a2_dens$x) - a2_dens$x)[1])) |> 
  filter(quant <= 1)

a3_dens = density(my_dat_a3$ns_ep2, adjust = 1)
a3_quant_f = data.frame(value = a3_dens$x,
                        quant = cumsum(a3_dens$y*c(lead(a3_dens$x) - a3_dens$x)[1])) |> 
  filter(quant <= 1)

my_q_holder = seq(0,1,by = .001)
mat_q = matrix(NA, length(my_q_holder), 3)

for(i in 1:length(my_q_holder)){
  mat_q[i,1] = a1_quant_f$value[which.min(abs(a1_quant_f$quant - my_q_holder[i]))]
  mat_q[i,2] = a2_quant_f$value[which.min(abs(a2_quant_f$quant - my_q_holder[i]))]
  mat_q[i,3] = a3_quant_f$value[which.min(abs(a3_quant_f$quant - my_q_holder[i]))]
}

my_inds = apply(mat_q, 1, which.max)

quant_plot = ggplot(data=a1_quant_f, aes(x=quant, y=value)) + 
  geom_line(linewidth = 1, color = "red") + 
  geom_line(data = a2_quant_f, 
            aes(x=quant, y=value), 
            linewidth = 1, color = "gold") + 
  geom_line(data = a3_quant_f, 
            aes(x=quant, y=value), 
            linewidth = 1, color = "blue") +
  geom_line(data = data.frame(quant = my_q_holder[which(my_inds == 3)],
                              value = mat_q[which(my_inds == 3), 3]),
            aes(x=quant, y=value), 
            linewidth = 3, color = "blue") + 
  geom_line(data = data.frame(quant = my_q_holder[which(my_inds == 2)],
                              value = mat_q[which(my_inds == 2), 2]),
            aes(x=quant, y=value), 
            linewidth = 3, color = "gold") + 
  geom_line(data = data.frame(quant = c(my_q_holder[which(my_inds == 1)]),
                              value = c(mat_q[which(my_inds == 1), 1])),
            aes(x=quant, y=value), 
            linewidth = 3, color = "red") + 
  xlab("Quantile") + 
  ylab("Expected points") + 
  ggtitle("Nex State Expected Points by Quantile"
        ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) + 
  annotate(geom = "text", x = .17, y = -.1,
           label = "PUNT optimal",  size = 3) + 
  annotate(geom = "text", x = .5, y = 2.3,
           label = "FGA optimal",  size = 3) + 
  annotate(geom = "text", x = .83, y = 4.8,
           label = "GO optimal",  size = 3) 

pdf("./figures/fig_2b.pdf",
    width = 5.5, height = 5.5)
quant_plot
dev.off()

