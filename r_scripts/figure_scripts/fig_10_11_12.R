source("./r_scripts/utils.R")
source("./r_scripts/constant.R")

# Load data  ---------------------------------------

clean_pbp <- readRDS("./data/pbp_2014_2022.rds")
state_colnames <- readRDS("./data/state_colnames.rds")
fourth_states <- play_states |>
  filter(down == 4)

# Fourth down bot intervals ------------------------------------------

tau_hat_wp_bot <- readRDS("./output/all_dec/tau_hat_wp_bot.rds") 
tau_hat_risk_neutral <- readRDS("./output/all_dec/tau_hat_risk_neutral.rds") 

tau_hat_wp_bot$field_region <- factor(tau_hat_wp_bot$field_region, 
                                  levels = c("OPP", "OWN"), 
                                  labels = c("Opp Half", "Own Half"))

tau_hat_risk_neutral$field_region <- factor(tau_hat_risk_neutral$field_region, 
                                  levels = c("OPP", "OWN"), 
                                  labels = c("Opp Half", "Own Half"))

coach_list <- list()
for(i in 1:length(wp_groups)){
  if(i == 1){
    coach_list[[i]] <- clean_pbp |>
      mutate(field_region = ifelse(yardline_100 <= 50, "Own Half", "Opp Half")) |> 
      filter(down == 4, 
             !is.na(fourth_down_decision),
             wp_group == wp_groups[i],
             qtr < 4) |>
      group_by(coach_name, posteam, field_region) |>
      summarize(n_dec = n()) |>
      group_by(coach_name, posteam) %>%
      mutate(min_dec = min(n_dec)) %>%
      ungroup() |> 
      filter(min_dec >= dec_limit) |> 
      arrange(coach_name) |> 
      mutate(wp_group = wp_groups[i],
             label_n = glue::glue("({n_dec})"))
  } else {
    coach_list[[i]] <- clean_pbp |>
      mutate(field_region = ifelse(yardline_100 <= 50, "Own Half", "Opp Half")) |> 
      filter(down == 4, 
             !is.na(fourth_down_decision),
             wp_group == wp_groups[i]) |>
      group_by(coach_name, posteam, field_region) |>
      summarize(n_dec = n()) |>
      group_by(coach_name, posteam) %>%
      mutate(min_dec = min(n_dec)) %>%
      ungroup() |> 
      filter(min_dec >= dec_limit) |> 
      arrange(coach_name) |> 
      mutate(wp_group = wp_groups[i],
             label_n = glue::glue("({n_dec})"))
  }
}


# Get tau hats for each coach ----------------------------------------

tau_hat_coach <- list()
for(i in 1:3){
  tau_hat_coach[[i]] <- readRDS(paste0("./output/all_dec/tau_hat_coach_all_dec_",i,".rds")) |>
    mutate(wp_group = wp_groups[i]) |>
    group_by(coach_name, posteam, field_region) 
  tau_hat_coach[[i]]$field_region <- factor(tau_hat_coach[[i]]$field_region, 
                                    levels = c("OPP", "OWN"), 
                                    labels = c("Opp Half", "Own Half"))
  
}
tau_hat_coach <- bind_rows(tau_hat_coach,
                           tau_hat_risk_neutral |> 
                             mutate(coach_name = "Risk Neutral Policy"),
                           tau_hat_wp_bot |> 
                             mutate(coach_name = "4th Down Bot")) |> 
  select(-season) |> 
  ungroup()

# Plot --------------------------------------------------------------------

tau_coach_plot_OPP <- list()
tau_coach_plot_OWN <- list()

for(i in 1:3){
  
  ## Opponent Half ------------------------------------------------------
  
  tau_coach_plot_OPP_df <- tau_hat_coach |> 
    filter(wp_group %in% c(wp_groups[i], "ALL")) |>
    filter(field_region == "Opp Half") |> 
    mutate(posteam = ifelse(is.na(posteam), "", posteam)) |> 
    left_join(coach_list[[i]], 
              by = c("coach_name", "posteam", "field_region", "wp_group")) |> 
    filter(n_dec >= dec_limit | coach_name == "4th Down Bot" | coach_name == "Risk Neutral Policy") |> 
    mutate(coach_team = factor(paste(coach_name |> 
                                       str_remove_all("'"), posteam),
                               levels = (tau_hat_coach |>
                                           filter(wp_group %in% c(wp_groups[i], "ALL")) |>
                                           filter(field_region == "Opp Half") |>
                                           mutate(posteam = ifelse(is.na(posteam), "", posteam)) |> 
                                           left_join(coach_list[[i]], 
                                                     by = c("coach_name", "posteam", "field_region", "wp_group")) |> 
                                           filter(n_dec >= dec_limit | coach_name == "4th Down Bot" | coach_name == "Risk Neutral Policy") |> 
                                           mutate(coach_team = factor(paste(coach_name |> 
                                                                              str_remove_all("'"), posteam))) |> 
                                           ungroup() |> 
                                           summarize(tau_med = reldist::wtd.quantile(tau, q = .5, weight = weight), 
                                                     .by = coach_team) |>
                                           arrange(desc(tau_med)) |> 
                                           pull(coach_team) |>
                                           rev()))) |> 
    mutate(coach_team = fct_relevel(coach_team, "Risk Neutral Policy ", "4th Down Bot ", after = Inf)) |> 
    ungroup()
  
  tau_coach_plot_OPP[[i]] <- tau_coach_plot_OPP_df |>
    ggplot(aes(x = tau, y = coach_team)) +
    geom_boxplot(aes(weight = weight,
                     fill = field_region), 
                 outlier.size = .25) +
    geom_text(data = (tau_coach_plot_OPP_df %>% 
                group_by(coach_team) %>% 
                summarise(q3 = reldist::wtd.quantile(tau, 0.75, weight = weight),
                          q1 = reldist::wtd.quantile(tau, 0.25, weight = weight),
                          iqr = q3 - q1,
                          top = max(tau),
                          n=first(label_n))), 
              aes(x=top, y=coach_team, label= n),
              nudge_x = .025,
              size = 2.5) +
    scale_fill_manual(name = "Field Region",
                      values = gray(.85) # c(rgb(1,.65,0,.5))
                      ) +
    scale_x_continuous(limits=c(.2,.826), breaks = seq(.2, .8, by = .1)) +
    ylab("") +
    xlab(expression(tau[1])) +
    theme_bw() + 
    theme(strip.background.x = element_rect(colour = 'black', fill = NA),
          strip.background.y = element_rect(colour = 'black', fill = NA),
          legend.position = "none") +
    labs(title = "Opponent Half", subtitle = glue::glue("Win probability group: {wp_groups[i]}"))
  

  ## Own Half ------------------------------------------------------

  tau_coach_plot_OWN_df <- tau_hat_coach |> 
    filter(wp_group %in% c(wp_groups[i], "ALL")) |>
    filter(field_region == "Own Half") |> 
    mutate(posteam = ifelse(is.na(posteam), "", posteam)) |> 
    left_join(coach_list[[i]], 
              by = c("coach_name", "posteam", "field_region", "wp_group")) |> 
    filter(n_dec >= dec_limit | coach_name == "4th Down Bot" | coach_name == "Risk Neutral Policy") |> 
    mutate(coach_team = factor(paste(coach_name |> 
                                       str_remove_all("'"), posteam),
                               levels = (tau_hat_coach |>
                                           filter(wp_group %in% c(wp_groups[i], "ALL")) |>
                                           filter(field_region == "Own Half") |>
                                           mutate(posteam = ifelse(is.na(posteam), "", posteam)) |> 
                                           left_join(coach_list[[i]], 
                                                     by = c("coach_name", "posteam", "field_region", "wp_group")) |> 
                                           filter(n_dec >= dec_limit | coach_name == "4th Down Bot" | coach_name == "Risk Neutral Policy") |> 
                                           mutate(coach_team = factor(paste(coach_name |> 
                                                                              str_remove_all("'"), posteam))) |> 
                                           ungroup() |> 
                                           summarize(tau_med = reldist::wtd.quantile(tau, q = .5, weight = weight), 
                                                     .by = coach_team) |>
                                           arrange(desc(tau_med)) |> 
                                           pull(coach_team) |>
                                           rev()))) |>
    mutate(coach_team = fct_relevel(coach_team, "Risk Neutral Policy ", "4th Down Bot ", after = Inf)) |> 
    ungroup()
    
  tau_coach_plot_OWN[[i]] <- tau_coach_plot_OWN_df |>
    ggplot(aes(x = tau, y = coach_team)) +
    geom_boxplot(aes(weight = weight,
                     fill = field_region), 
                 outlier.size = .25) +
    geom_text(data = (tau_coach_plot_OWN_df |>  
                        group_by(coach_team) |>  
                        summarise(q3 = reldist::wtd.quantile(tau, 0.75, weight = weight),
                                  q1 = reldist::wtd.quantile(tau, 0.25, weight = weight),
                                  iqr = q3 - q1,
                                  top = max(tau),
                                  n=first(label_n))), 
              aes(x=top, y=coach_team, label= n),
              nudge_x = .025,
              size = 2.5) +
    scale_fill_manual(name = "Field Region",
                       values = gray(.85) #c(rgb(0,0,.5,.5))
                      ) +
    scale_x_continuous(limits=c(.2,.826), breaks = seq(.2, .8, by = .1)) +
    ylab("") +
    xlab(expression(tau[2])) +
    theme_bw() + 
    theme(strip.background.x = element_rect(colour = 'black', fill = NA),
          strip.background.y = element_rect(colour = 'black', fill = NA),
          legend.position = "none") +
    labs(title = "Own Half", subtitle = glue::glue("Win probability group: {wp_groups[i]}"))
}

pdf(file = "./figures/fig_10.pdf", width = 12, height = 12)
gridExtra::grid.arrange(tau_coach_plot_OPP[[2]], tau_coach_plot_OWN[[2]], nrow = 1)
dev.off()

pdf(file = "./figures/fig_11.pdf", width = 12, height = 6)
gridExtra::grid.arrange(tau_coach_plot_OPP[[1]], tau_coach_plot_OWN[[1]], nrow = 1)
dev.off()

pdf(file = "./figures/fig_12.pdf", width = 12, height = 8)
gridExtra::grid.arrange(tau_coach_plot_OPP[[3]], tau_coach_plot_OWN[[3]], nrow = 1)
dev.off()






# Coaches more risky in own half ----------------------------------------------------

tau_hat_coach |>
  filter(wp_group == wp_groups[2]) |>
  group_by(coach_name, posteam, field_region) |>
  summarize(tau_med = matrixStats::weightedMean(tau, weight)) |> 
  pivot_wider(names_from = field_region, values_from = tau_med) |> 
  mutate(tau_diff = `Opp Half` - `Own Half`) |> 
  View()


# Unused tables ------------------------------------------------------

# table of coach decision counts -----

# coach_table <- clean_pbp |>
#   filter(down == 4, !is.na(fourth_down_decision)) |>
#   group_by(coach_name, posteam, wp_group) |>
#   summarize(n_dec = n()) |>
#   arrange(coach_name) |> 
#   mutate(coach_team = paste(coach_name, posteam)) |> 
#   ungroup() |> 
#   select(coach_team, wp_group, n_dec) |> 
#   pivot_wider(names_from = wp_group,
#               values_from = n_dec)
# 
# coach_table[is.na(coach_table)] <- 0
# print(xtable::xtable(coach_table, caption = "holder text"),
#       include.rownames=FALSE)

# Eligible coaches - more than dec_limit decisions in a wp group
# note that coach could be on multiple different teams
