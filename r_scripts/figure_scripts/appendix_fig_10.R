source("./r_scripts/utils.R")
source("./r_scripts/constant.R")

# Load data and get value function ---------------------------------------

clean_pbp <- readRDS("./data/pbp_2014_2022.rds")
state_colnames <- readRDS("./data/state_colnames.rds")

value_func <- get_value_function(clean_pbp)

# Plot global value function -------------------------------------------

down.labs <- str_c("Down ", 1:4)
names(down.labs) <- 1:4

value_plot = value_func |> 
  filter(team == "off", 
         !is.na(down)) |>
  mutate(ydstogo_group = factor(ydstogo_group, levels = (ydstogo_label))) |>
  mutate(yardline_100_group = factor(yardline_100_group, levels = (yardline_label))) |>
  ggplot(aes(x=yardline_100_group, y=ydstogo_group, fill=value))+
  geom_tile(colour="white",
            linewidth = 0.1)+
  scale_fill_gradient2(low=('blue'), mid='white',midpoint = 0, high=('red'),
                       name = bquote(hat(nu)[A]^{bar(pi)}),
                       na.value = "grey50") + 
  scale_x_discrete(labels = as.character(fix_yardline_label)) +
  theme_classic() +
  xlab("Yardline to endzone") +
  ylab("Yards to go") +
  facet_wrap(~down, nrow = 1, labeller = labeller(down = down.labs)) +
  coord_fixed() +
  theme(axis.text.x = element_text(angle = 45, 
                                   size = 8, 
                                   hjust = 1)) 
value_plot

pdf(file = "./figures/appendix_fig_10.pdf", height = 3, width = 10)
value_plot
dev.off()
