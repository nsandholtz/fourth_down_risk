# Required libraries

library(tidyverse)
library(scam)
library(glue)
library(reldist)
library(ggtext)

# MISC FUNCTIONS ----------------------------------------------------------

# For assigning yardline groupings.
yard_cut <- function(x, 
                     lower = 1,
                     upper, 
                     by = 10, 
                     sep = "-", 
                     above_char = "+") {
  if(lower == by){
    labs <- c(paste0(seq(lower, upper - by, by = by)),
              paste(upper, above_char, sep = ""))
  }else{
    labs <- c(paste(seq(lower, upper - by, by = by),
                    seq(lower + by - 1, upper - 1, by = by),
                    sep = sep),
              paste(upper, above_char, sep = ""))
  }
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

clean_data = function(data, 
                      ydstogo_bin,
                      yardline_100_bin,
                      max_ydstogo, 
                      max_yardline_100){

  scoring_states = c("touchdown", 
                     "field_goal", 
                     "safety", 
                     "opp_touchdown", 
                     "opp_field_goal",
                     "opp_safety")
  
  kickoff_states = c( "kickoff1",
                      "kickoff2",
                      "kickoff3",
                      "kickoff4",
                      "kickoff5",
                      "kickoff6")
  
  # Remove PATs and any missing play_type:
  removed_pbp_data <- data |>
    filter(!is.na(play_type),
           !is.na(ydstogo),
           !is.na(yardline_100),
           !(play_type %in% c("extra_point")),
           two_point_attempt == 0,
           timeout == 0 | (timeout == 1 & play_type != "no_play"),
           extra_point_attempt == 0,
           ydstogo >= 0) |>
    mutate(ydstogo_group = yard_cut(ydstogo, upper = max_ydstogo, by = ydstogo_bin), 
           yardline_100_group = yard_cut(yardline_100, upper = max_yardline_100, by = yardline_100_bin))
  
  # cleaning & generating features
  clean_pbp_data = 
    removed_pbp_data |>
    group_by(game_id, game_half) |>
    mutate(play_index = row_number(),
           drive_length = n(),
           next_posteam = lead(posteam),
           previous_type = lag(play_type),
           next_down = lead(down),
           next_ydstogo = lead(ydstogo),
           next_yardline_100 = lead(yardline_100),
           next_ydstogo_group = lead(ydstogo_group),
           next_yardline_100_group = lead(yardline_100_group)) |>
    ungroup() |>
    # combine state, down and yardline_100 to one variable
    unite(play_state, 
          down, 
          ydstogo_group,
          yardline_100_group, 
          sep = "_", remove = FALSE) |>
    # set up the scoring states as absorption states
    mutate(absorption_state = case_when(
      # define Team B gets touchdown after Team A kickoff 
      kickoff_attempt == 1 & td_team == posteam & touchdown == 1 ~ "opp_touchdown",
      td_team != posteam & touchdown == 1 ~ "opp_touchdown",
      td_team == posteam & touchdown == 1 ~ "touchdown",
      kickoff_attempt == 1 ~ "kickoff",
      field_goal_result == "made" ~ "field_goal",
      safety == 1 ~ "safety",
      TRUE ~ "end_of_time"
    )) |> 
    # convert kickoff to kickoff at specific yardline
    mutate(play_state =
             case_when(
               kickoff_attempt == 1 & yardline_100 <= 20 ~ "kickoff1",
               kickoff_attempt == 1 & yardline_100 == 25 ~ "kickoff2",
               kickoff_attempt == 1 & yardline_100 == 30 ~ "kickoff3",
               kickoff_attempt == 1 & yardline_100 == 35 ~ "kickoff4",
               kickoff_attempt == 1 & yardline_100 == 40 ~ "kickoff5",
               kickoff_attempt == 1 & yardline_100 >= 50 ~ "kickoff6",
               TRUE ~ play_state)
    ) |>
    # combine next_state, next_down and next_yardline_100 to one variable
    unite(next_play_state, 
          next_down,
          next_ydstogo_group,
          next_yardline_100_group,
          sep = "_", remove = FALSE) |>
    # define next_play_state with off and opp
    mutate(next_play_state_2 = ifelse(posteam == next_posteam, 
                                      glue::glue("off_{next_play_state}"),
                                      glue::glue("opp_{next_play_state}")
    )
    ) |> 
    mutate(next_play_state_2 = case_when(
      # KICKOFF to OTHER TEAM
      kickoff_attempt == 1 & posteam == next_posteam ~ glue::glue("opp_{next_play_state}"),
      # KICKOFF to ONSIDES/FUMBLE RECOVERY
      kickoff_attempt == 1 & posteam != next_posteam ~ glue::glue("off_{next_play_state}"),
      TRUE ~ next_play_state_2)
    ) |> 
    # convert kickoff to kickoff at specific yardline
    mutate(absorption_state =
             case_when(
               absorption_state == "kickoff" & yardline_100 <= 20 ~ "kickoff1",
               absorption_state == "kickoff" & yardline_100 == 25 ~ "kickoff2",
               absorption_state == "kickoff" & yardline_100 == 30 ~ "kickoff3",
               absorption_state == "kickoff" & yardline_100 == 35 ~ "kickoff4",
               absorption_state == "kickoff" & yardline_100 == 40 ~ "kickoff5",
               absorption_state == "kickoff" & yardline_100 >= 50 ~ "kickoff6",
               TRUE ~ absorption_state)
    ) |> 
    mutate(prev_absorption_state = lag(absorption_state),
           next_absorption_state = lead(absorption_state)) |> 
    mutate(next_play_state_2 =
             ifelse(absorption_state == "end_of_time" & (next_absorption_state %in% c("kickoff1",
                                                                                      "kickoff2",
                                                                                      "kickoff3",
                                                                                      "kickoff4",
                                                                                      "kickoff5",
                                                                                      "kickoff6")
             ),
             yes = next_absorption_state,
             no = next_play_state_2)
    ) |>
    mutate(prev_absorption_state = lag(absorption_state),
           next_absorption_state = lead(absorption_state)) |>
    # set next_play_state_3 to be absorption states such as touchdown, field_goal etc
    mutate(next_play_state_3 = ifelse(!(absorption_state) %in% c("end_of_time",
                                                                 "kickoff1",
                                                                 "kickoff2",
                                                                 "kickoff3",
                                                                 "kickoff4",
                                                                 "kickoff5",
                                                                 "kickoff6"),
                                      yes = (absorption_state),
                                      no = next_play_state_2)
    ) |>
    mutate(next_absorption_state = ifelse(absorption_state %in% c("field_goal",
                                                                  "touchdown",
                                                                  "opp_touchdown",
                                                                  "safety"),
                                          yes = "kickoff",
                                          no = next_play_state_2
    )) |> 
    mutate(next_absorption_state =
             case_when(
               next_absorption_state == "kickoff" & lead(yardline_100) <= 20 ~ "kickoff1",
               next_absorption_state == "kickoff" & lead(yardline_100) == 25 ~ "kickoff2",
               next_absorption_state == "kickoff" & lead(yardline_100) == 30 ~ "kickoff3",
               next_absorption_state == "kickoff" & lead(yardline_100) == 35 ~ "kickoff4",
               next_absorption_state == "kickoff" & lead(yardline_100) == 40 ~ "kickoff5",
               next_absorption_state == "kickoff" & lead(yardline_100) >= 50 ~ "kickoff6",
               TRUE ~ next_absorption_state),
           wp_group = as.character(cut(wp, c(0,0.2,0.8,1), include.lowest = F, right = T))
    ) 
  
  # remove the end of period plays
  clean_pbp_data = clean_pbp_data |> 
    filter(!(str_detect(next_play_state_3, "kickoff") & next_play_state == "NA_NA_NA"))
  
  return(clean_pbp_data)
}

assign_fourth_down_decision <- function(data){
  data = data |> 
    mutate(fourth_down_decision = case_when(
      is.na(down) ~ NA_character_,
      down != 4 ~ NA_character_,
      # remove "qb_kneel"
      down == 4 & (play_type %in% c("run", "pass", "qb_spike", "no_play") & 
                     (!str_detect(tolower(desc), "field goal")) &  
                     (!str_detect(tolower(desc), "punt"))
      ) ~ "go",
      down == 4 & (play_type %in% c("field_goal") | 
                     (play_type %in% "no_play" & str_detect(tolower(desc), "field goal"))
      ) ~ "fga",
      down == 4 & (play_type %in% c("punt") | 
                     (play_type %in% "no_play" & str_detect(tolower(desc), "punt"))
      ) ~ "punt", 
      TRUE ~ "idk")
    ) |> 
    # fake punts are assigned to go for it
    mutate(fourth_down_decision = ifelse(fourth_down_decision == "idk" & play_type %in% c("run", "pass"),
                                         yes = "go", 
                                         no = fourth_down_decision)) 
  return(data)
}

plot_decision_map <- function(data, 
                              fill_variable = "discrete",
                              legend_name,
                              ...){
  if(fill_variable == "discrete"){
    p = data |> 
      ggplot(aes(x = yardline_100_group, y = ydstogo_group, ...)) +
      geom_tile(colour = "white", linewidth = 0.1) +
      scale_fill_manual(
        name = legend_name,
        values = c("#d53e4f", "gold", "#2C7FB8"),
        limits = c("go", "fga", "punt")
      ) +
      theme_classic() +
      xlab("Yards to opponent endzone") +
      ylab("Yards to go") + 
      coord_fixed()
    
  }else if(fill_variable == "continuous"){
    p = data |> 
      mutate(ydstogo_group = factor(ydstogo_group, levels = ydstogo_label)) |>
      ggplot(aes(x=ydstogo_group, y=yardline_100_group, ...))+
      geom_tile(
        colour="white", 
        linewidth=0.1,
      ) +
      scale_fill_gradient2(
        name = "value",
        low = 'blue',
        mid = 'white',
        midpoint = 0,
        high = 'red',
      ) +
      scale_y_discrete(labels = as.character(fix_yardline_label)) +
      coord_flip() + 
      ylab("Yards to opponent endzone") +
      xlab("Yards to go") 
  }else{
    print("check your spelling...")
  }
  p = p + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(plot.subtitle = element_text(hjust = 0.5)) + 
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
      strip.text.y.right = element_text(angle = 90))
  return(p)
}

get_one_game_data <- function(x) {
  clean_pbp |> filter(game_id %in% x)
}

# FUNCTIONS FOR VALUE FUNCTION --------------------------------------------


get_half_TCM <- function(clean_data, 
                         unique_down_state = down_label,
                         unique_ydstogo_state = ydstogo_label,
                         unique_yardline_100_state = yardline_label
){
  scoring_states = c("touchdown", 
                     "field_goal", 
                     "safety", 
                     "opp_touchdown", 
                     "opp_field_goal",
                     "opp_safety")
  
  clean_pbp_data = clean_data
  
  # define absorption states:
  absorption_states <- unique(clean_pbp_data$absorption_state)
  
  # states to scoring states count
  a = clean_pbp_data |>
    group_by(play_state, next_play_state_3) |>
    count() |> 
    filter(!is.na(next_play_state_3)) |> 
    filter(!(str_detect(play_state, "kickoff") & (next_play_state_3 %in% scoring_states)))
  
  # scoring to scoring states count
  a1 = clean_pbp_data |>
    filter(absorption_state !=  "end_of_time") |>
    group_by(absorption_state, next_absorption_state) |>
    count() |> 
    dplyr::select(play_state = absorption_state, 
                  next_play_state_3 = next_absorption_state,
                  n) |> 
    filter(next_play_state_3 != "end_of_time" ) |> 
    filter(!(play_state == "touchdown" &  next_play_state_3 == "opp_touchdown")) |> 
    filter(next_play_state_3 != play_state )
  
  a2 = rbind(a, a1)
  
  possible_grids = 
    expand.grid(down = unique_down_state,
                ydstogo_group = unique_ydstogo_state,
                yardline_100_group = unique_yardline_100_state
    ) |> 
    arrange(down, 
            ydstogo_group,
            yardline_100_group) |> 
    unite(play_state, 
          down,
          ydstogo_group,
          yardline_100_group,
          sep = "_", remove = T)
  
  
  events = c("touchdown", 
             "field_goal", 
             "safety", 
             "opp_touchdown", 
             "opp_field_goal",
             "opp_safety",
             "kickoff1",
             "kickoff2",
             "kickoff3",
             "kickoff4",
             "kickoff5",
             "kickoff6")
  
  off_play_state = data.frame(play_state = append(possible_grids$play_state, events), stringsAsFactors = F)
  off_play_state = off_play_state |> 
    mutate(off = ifelse(play_state %in% events, 
                        play_state,
                        glue::glue("off_{play_state}")))
  
  
  b = expand.grid(off_play_state, 
                  stringsAsFactors = F) |> 
    filter(play_state!=off)
  
  opp_play_state = data.frame(play_state = append(possible_grids$play_state, events), stringsAsFactors = F)
  opp_play_state = opp_play_state |> 
    mutate(opp = ifelse(play_state %in% events, 
                        play_state,
                        glue::glue("opp_{play_state}")))
  
  b2 = expand.grid(opp_play_state, stringsAsFactors = F) |> 
    filter(play_state!=opp)
  
  mat1 = b |> 
    left_join(a2, by = c("play_state" = "play_state", 
                         "off"="next_play_state_3")) |>
    unique() |>
    pivot_wider(id_cols = play_state, names_from = off,values_from = n) 
  mat1[is.na(mat1)] = 0
  
  mat2 = b2 |> 
    left_join(a2, by = c("play_state" = "play_state", 
                         "opp"="next_play_state_3")) |>
    unique() |> 
    pivot_wider(id_cols = play_state, names_from = opp, values_from = n) #|> 
  mat2[is.na(mat2)] = 0
  
  
  mat3 = cbind(mat1 |> 
                 dplyr::select(-touchdown:-kickoff6),
               mat2 |> dplyr::select(-play_state)) |> 
    dplyr::select(-play_state) |> 
    as.matrix()
  
  return(mat3)
}

mirror_TPM <- function(half_TPM,
                       unique_down_state = down_label,
                       unique_ydstogo_state = ydstogo_label,
                       unique_yardline_100_state = yardline_label,
                       unique_kickoff_lines = kickoff_lines){
  
    n_combination = length(unique_down_state)*length(unique_ydstogo_state)*length(unique_yardline_100_state)
    n_scoring = length(score_states)
    n_scoring_both = n_scoring*2
    n_kickoff = length(unique_kickoff_lines)
    n_state_space = (n_combination * 2) + (n_scoring * 2) + (n_kickoff * 2)
    
    # initialize the full TPM matrix
    final_mat = matrix(0, nrow = n_state_space, ncol = n_state_space)
    off_end_index = n_combination 
    
  def_start_index = off_end_index + 1
  def_end_index = off_end_index * 2

  # initialize
  final_mat[1:off_end_index, 1:off_end_index] <- half_TPM[1:off_end_index, 1:off_end_index] # A
  final_mat[1:off_end_index, def_start_index:def_end_index] <- half_TPM[1:off_end_index, def_start_index:def_end_index] #B
  final_mat[1:off_end_index, def_end_index+1:n_scoring_both] <- half_TPM[1:off_end_index, def_end_index+1:n_scoring_both] #C & D
  
  # No line to bring over nonscoring transitions from half_TPM to kickoffs because I'm assuming those will all be 0 (need to make sure this is true)
  
  final_mat[def_end_index+n_scoring_both+1:n_kickoff, def_end_index+1:n_scoring_both] = (half_TPM[off_end_index+n_scoring_both+1:n_kickoff, def_end_index+1:n_scoring_both]) #H & I
  final_mat[def_end_index+1:n_scoring, def_end_index + n_scoring_both + 1:n_kickoff] = (half_TPM[off_end_index+1:n_scoring, def_end_index+ n_scoring_both + 1:n_kickoff]) #E
  final_mat[def_end_index + n_scoring_both + 1:n_kickoff, 1:off_end_index] = half_TPM[off_end_index+n_scoring_both+1:n_kickoff, 1:off_end_index] #F
  final_mat[def_end_index + n_scoring_both + 1:n_kickoff, def_start_index:def_end_index] = half_TPM[off_end_index+n_scoring_both+1:n_kickoff, def_start_index:def_end_index] #G
  
  # copy or flip matrix 
  final_mat[def_start_index:def_end_index, def_start_index:def_end_index] <- half_TPM[1:off_end_index, 1:off_end_index] # (A)
  final_mat[def_start_index:def_end_index, 1:off_end_index] <- half_TPM[1:off_end_index, def_start_index:def_end_index] # (B)
  final_mat[def_start_index:def_end_index, def_end_index + n_scoring + 1:n_scoring] <- half_TPM[1:off_end_index, def_end_index + 1:n_scoring] # (C)
  final_mat[def_start_index:def_end_index, def_end_index + 1:n_scoring] <- half_TPM[1:off_end_index, def_end_index + n_scoring + 1:n_scoring] # (D) 
  final_mat[def_end_index + n_scoring + 1:n_scoring, def_end_index + n_scoring_both + n_kickoff + 1:n_kickoff] <- half_TPM[off_end_index + 1:n_scoring, def_end_index + n_scoring_both + 1:n_kickoff] # (E) 
  final_mat[def_end_index + n_scoring_both + n_kickoff + 1:n_kickoff, def_start_index:def_end_index] <- half_TPM[off_end_index + n_scoring_both + 1:n_kickoff, 1:off_end_index] # (F)
  final_mat[def_end_index + n_scoring_both + n_kickoff + 1:n_kickoff, 1:off_end_index] <- half_TPM[off_end_index + n_scoring_both + 1:n_kickoff, def_start_index:def_end_index] # (G)
  final_mat[def_end_index + n_scoring_both + n_kickoff + 1:n_kickoff, def_end_index + n_scoring + 1:n_scoring] <- half_TPM[off_end_index + n_scoring_both + 1:n_kickoff, def_end_index + 1:n_scoring] # (H)
  final_mat[def_end_index + n_scoring_both + n_kickoff + 1:n_kickoff, def_end_index + 1:n_scoring] <- half_TPM[off_end_index + n_scoring_both + 1:n_kickoff, def_end_index + n_scoring + 1:n_scoring] # (I)
  
  return(final_mat)
}

solve_value_function <- function(TPM, 
                                 touchdown_value_ = touchdown_value,
                                 unique_down_state = down_label,
                                 unique_ydstogo_state = ydstogo_label,
                                 unique_yardline_100_state = yardline_label,
                                 unique_kickoff_lines = kickoff_lines){
  
  n_combination = length(unique_down_state)*length(unique_ydstogo_state)*length(unique_yardline_100_state)
  n_scoring = length(score_states)
  n_scoring_both = n_scoring*2
  n_kickoff = length(unique_kickoff_lines)
  n_state_space = (n_combination * 2) + (n_scoring * 2) + (n_kickoff * 2)
  
  off_end_index = n_combination 
  def_start_index = off_end_index + 1
  def_end_index = off_end_index * 2
  
  # CONSTRUCT Eq. 19 from Chan et. al. 2021
  
  # P_AA + p_AB ->
  
  #  A C  J1    B  D J2
  # J3 J5 E  + J4 J6 J7
  #  F H  J8    G  I J9
  
  A_r = A_c = 1:off_end_index
  B_r = 1:off_end_index; B_c = (def_start_index):(def_end_index)
  C_r = 1:off_end_index; C_c = (def_end_index) + 1:n_scoring
  D_r = 1:off_end_index; D_c = (def_end_index) + n_scoring + 1:n_scoring
  E_r = (def_end_index) + 1:n_scoring; E_c = (def_end_index) + n_scoring_both + 1:n_kickoff
  F_r = (def_end_index) + n_scoring_both + 1:n_kickoff; F_c = 1:off_end_index
  G_r = (def_end_index) + n_scoring_both + 1:n_kickoff; G_c = (def_start_index):(def_end_index)
  H_r = (def_end_index) + n_scoring_both + 1:n_kickoff; H_c = (def_end_index) + 1:n_scoring
  I_r = (def_end_index) + n_scoring_both + 1:n_kickoff; I_c = (def_end_index) + n_scoring + 1:n_scoring
  
  J1_r = 1:off_end_index; J1_c = (def_end_index) + n_scoring_both + 1:n_kickoff
  J2_r = 1:off_end_index; J2_c = (def_end_index) + n_scoring_both + n_kickoff + 1:n_kickoff
  J3_r = (def_end_index) + 1:n_scoring; J3_c = 1:off_end_index
  J4_r = (def_end_index) + 1:n_scoring; J4_c = (def_start_index):(def_end_index)
  J5_r = (def_end_index) + 1:n_scoring; J5_c = (def_end_index) + 1:n_scoring 
  J6_r = (def_end_index) + 1:n_scoring; J6_c = (def_end_index) + n_scoring + 1:n_scoring
  J7_r = (def_end_index) + 1:n_scoring; J7_c = (def_end_index) + n_scoring_both + n_kickoff + 1:n_kickoff
  J8_r = (def_end_index) + n_scoring_both + 1:n_kickoff; J8_c = (def_end_index) + n_scoring_both + 1:n_kickoff
  J9_r = (def_end_index) + n_scoring_both + 1:n_kickoff; J9_c = (def_end_index) + n_scoring_both + n_kickoff + 1:n_kickoff
  
  p_AA = TPM[c(A_r, J3_r, F_r), c(A_c, C_c, J1_c)]
  p_AB = TPM[c(B_r, J4_r, G_r), c(B_c, D_c, J2_c)]
  
  I_nfl = diag(nrow(p_AA)) # Identity matrix
  reward_A = c(rep(0, off_end_index), touchdown_value_, 3, -2, rep(0, n_kickoff)) # reward vector
  value_func = solve(I_nfl - p_AA + p_AB) %*% reward_A # Solve equation 19
  value_func = ifelse(value_func == 0, NA, value_func) # Set exact zeros to NAs
  return(value_func)
}

get_value_df_full <- function(value_states,
                              my_col_names = state_colnames){
  value_df_full = data.frame( play_state = my_col_names,
                              value = c(
                                value_states[1:400],
                                -value_states[1:400],
                                value_states[401:403],
                                -value_states[401:403],
                                value_states[404:409])
  ) |> 
    mutate(index = row_number()) |> 
    separate(play_state, c("team", "down", "ydstogo_group","yardline_100_group"), sep = "_", remove = F) |> 
    mutate(down = as.numeric(down))
  value_df_full$value[which(value_df_full$down == 1 & 
            value_df_full$yardline_100_group != "1-10" &
            !(value_df_full$ydstogo_group %in% c("5","10+")))] <- NA # These states shouldn't be possible...
  
  value_df_full[c(801:803, 807:812), 2] <- "off"
  value_df_full[801:812, 3:5] <- NA 
  value_df_full
}

get_value_function <- function(data){
  half_TCM <- data |>
    get_half_TCM()
  half_TPM <- t(apply(half_TCM, 1, function(x){x/sum(x)})) # Normalize
  half_TPM[is.nan(half_TPM)] <- 0 # Make rows with no obs zeros
  TPM <- half_TPM |> mirror_TPM()
  value_states = solve_value_function(TPM) 
  value_df <- suppressWarnings(get_value_df_full(value_states, state_colnames))
  value_df
}


# FUNCTIONS FOR QUANTILE OPTIMAL POLICIES ---------------------------------


get_augmented_third_down_data <- function(data){
  all_states_grid =
    expand.grid(down = 1:4,
                ydstogo = c(1:99),
                yardline_100 = 1:99) |>
    arrange(down,
            ydstogo,
            yardline_100) |>
    unite(play_state,
          down,
          ydstogo,
          yardline_100,
          sep = "_", remove = F) |>
    mutate(state_index = row_number()) |> 
    dplyr::select(-play_state)
  
  
  third_down_penalty = data |> 
    mutate(off_team = ifelse(posteam == next_posteam, "off", "opp")) |> 
    filter(down == 3 & penalty ==1) |>
    unite(next_play_state_exact, 
          off_team,
          next_down,
          next_ydstogo,
          next_yardline_100,
          sep = "_", remove = FALSE) |>
    unite(play_state_exact,
          down,
          ydstogo,
          yardline_100,
          sep = "_", remove = FALSE) |> 
    left_join(all_states_grid, by = c("down" = "down", "ydstogo" = "ydstogo", "yardline_100" = "yardline_100")) |> 
    left_join(all_states_grid |> rename(next_state_index = state_index), 
              by = c("next_down" = "down", "next_ydstogo" = "ydstogo", "next_yardline_100" = "yardline_100")
    ) |> 
    mutate(state_index = ifelse(off_team == "opp", nrow(all_states_grid) + state_index , state_index),
           next_state_index = ifelse(off_team == "opp", nrow(all_states_grid) + next_state_index , next_state_index)) |> 
    mutate(next_down_2 = case_when(next_down == 4 ~ 1,
                                   next_down == 3 ~ 4,
                                   next_down == 1 ~ 1,
                                   TRUE ~ NA_real_)
    ) |>  
    mutate(off_team_2 = ifelse(next_down == 4 , "opp", off_team)) |> 
    dplyr::mutate(next_yardline_100_2 = ifelse(next_down == 4 , 100 - yardline_100, next_yardline_100)) |>
    dplyr::mutate(next_ydstogo_2 = ifelse(next_down == 4 , 10, next_ydstogo)) |>
    dplyr::mutate(down = 4) |> 
    unite(play_state,
          # off_team,
          down,
          ydstogo_group,
          yardline_100_group,
          sep = "_", remove = FALSE) |> 
    dplyr::mutate(next_ydstogo_group_hyp = yard_cut(next_ydstogo_2, upper = MAX_ydstogo, by = ydstogo_bin),
                  next_yardline_100_group_hyp = yard_cut(next_yardline_100_2, upper = MAX_yardline_100, by = yardline_100_bin)) |>
    unite(next_play_state_hyp,
          off_team_2,
          next_down_2,
          next_ydstogo_group_hyp,
          next_yardline_100_group_hyp,
          sep = "_", remove = T) |>
    mutate(next_play_state_hyp = ifelse(is.na(next_state_index), next_play_state_3, next_play_state_hyp)) |>  
    mutate(play_type = ifelse(play_type == "no_play", "pass", play_type)) |> 
    dplyr::select(-next_play_state_exact, -play_state_exact, -next_play_state_3, 
           -state_index, -next_state_index, -off_team,
           -next_yardline_100_2, -next_ydstogo_2) |> 
    rename(next_play_state_3 = next_play_state_hyp ) 
  
  third_down_df = data |> 
    filter(down == 3 & penalty ==0) |> 
    dplyr::mutate(next_yardline_100 = ifelse(third_down_converted == 0, 100 - yardline_100, next_yardline_100)) |> 
    dplyr::mutate(next_down = ifelse(third_down_converted == 0, 1, next_down)) |> 
    dplyr::mutate(next_ydstogo = ifelse(third_down_converted == 0, 10, next_ydstogo)) |> 
    dplyr::mutate(next_ydstogo_group_hyp = yard_cut(next_ydstogo, upper = MAX_ydstogo, by = ydstogo_bin), 
                  next_yardline_100_group_hyp = yard_cut(next_yardline_100, upper = MAX_yardline_100, by = yardline_100_bin)) |> 
    unite(next_play_state_hyp, 
          next_down,
          next_ydstogo_group_hyp,
          next_yardline_100_group_hyp,
          sep = "_", remove = FALSE) |>
    mutate(next_play_state_hyp = ifelse(third_down_converted == 0, 
                                        glue::glue("opp_{next_play_state_hyp}"),
                                        glue::glue("off_{next_play_state_hyp}")
    )
    ) |> 
    mutate(next_play_state_hyp = ifelse(third_down_converted == 0 & next_play_state_3 == "opp_touchdown", next_play_state_3, next_play_state_hyp )) |> 
    mutate(next_play_state_hyp = ifelse(third_down_converted == 0 & next_play_state_3 == "safety", next_play_state_3, next_play_state_hyp )) |> 
    mutate(next_play_state_3 = ifelse(third_down_converted == 0, next_play_state_hyp, next_play_state_3 )) |> 
    dplyr::select(-next_ydstogo_group_hyp, -next_yardline_100_group_hyp, -next_play_state_hyp) |> 
    dplyr::mutate(down = 4) |> 
    unite(play_state, 
          down, 
          ydstogo_group,
          yardline_100_group, 
          sep = "_", remove = FALSE) |> 
    filter(play_type != "field_goal") 
  
  if(nrow(third_down_penalty) == 0){
    augmented_df = bind_rows(data, third_down_df)
  } else {
    augmented_df = bind_rows(data, third_down_df, third_down_penalty)
  }
  return(augmented_df)
}

get_action_TCMs <- function(clean_data, augment_third = FALSE){
  actions <- c("go", "fga", "punt")
  a_TCM <- list()
  if(augment_third){
    # Augment with 3rd down data for action = GO
    a_TCM[[1]] = clean_data |> 
      get_augmented_third_down_data() |>
      assign_fourth_down_decision() |>
      filter(down == 4) |>
      filter(fourth_down_decision == actions[1]) |> 
      get_half_TCM() |> 
      (\(.) .[301:400,])() 
    a_TCM[[1]][is.nan(a_TCM[[1]])] <- 0 # Make rows with no obs zeros
    
    for(i in 2:3){
      a_TCM[[i]] = clean_data |> 
        filter(down == 4) |>
        filter(fourth_down_decision == actions[i]) |> 
        get_half_TCM() |> 
        (\(.) .[301:400,])() 
      a_TCM[[i]][is.nan(a_TCM[[i]])] <- 0 # Make rows with no obs zeros
    }
  } else {
    for(i in 1:3){
      a_TCM[[i]] = clean_data |> 
        filter(down == 4) |>
        filter(fourth_down_decision == actions[i]) |> 
        get_half_TCM() |> 
        (\(.) .[301:400,])()
      a_TCM[[i]][is.nan(a_TCM[[i]])] <- 0 # Make rows with no obs zeros
    }
  }
  a_TCM
}

get_action_TPMs <- function(clean_data, augment_third = FALSE){
  actions <- c("go", "fga", "punt")
  a_TPM <- list()
  
  if(augment_third){
    # Augment with 3rd down data for action = GO
    a_TPM[[1]] = clean_data |> 
      get_augmented_third_down_data() |>
      assign_fourth_down_decision() |>
      filter(down == 4) |>
      filter(fourth_down_decision == actions[1]) |> 
      get_half_TCM() |> 
      (\(.) .[301:400,])() |> 
      apply(1, function(x){x/sum(x)}) |> # Get TPM
      t()
    a_TPM[[1]][is.nan(a_TPM[[1]])] <- 0 # Make rows with no obs zeros
    
    for(i in 2:3){
      a_TPM[[i]] = clean_data |> 
        filter(down == 4) |>
        filter(fourth_down_decision == actions[i]) |> 
        get_half_TCM() |> 
        (\(.) .[301:400,])() |> 
        apply(1, function(x){x/sum(x)}) |> # Get TPM
        t()
      a_TPM[[i]][is.nan(a_TPM[[i]])] <- 0 # Make rows with no obs zeros
    }
  } else {
    for(i in 1:3){
      a_TPM[[i]] = clean_data |> 
        filter(down == 4) |>
        filter(fourth_down_decision == actions[i]) |> 
        get_half_TCM() |>
        (\(.) .[301:400,])() |> 
        apply(1, function(x){x/sum(x)}) |> # Get TPM
        t()
      a_TPM[[i]][is.nan(a_TPM[[i]])] <- 0 # Make rows with no obs zeros
    }
  }
  a_TPM
}

my_quantile <- function(V_dist, tau){
  V_dist |> 
    filter(cum_prob >= tau) |>
    slice(1) |>
    pull(value)
}

get_tau_policy <- function(value_func,
                           a_TPM,
                           tau, 
                           fourth_state_df = fourth_states){
  actions <- c("go", "fga", "punt")
  fourth_state_df$V_go <- NA
  fourth_state_df$V_fga <- NA
  fourth_state_df$V_punt <- NA
  
  q_vals <- vector("list", 3)

  for(i in 1:3){  
    for(j in 1:nrow(fourth_state_df)){
      if(sum(a_TPM[[i]][j,]) == 0){
        q_vals[[i]][j] <- NA
        next
      }
      V <- cbind.data.frame(trans_prob = a_TPM[[i]][j,],value = value_func$value) |>
        filter(trans_prob > 0) |>
        arrange(value) |>
        mutate(cum_prob = cumsum(trans_prob))
      q_vals[[i]][j] <- my_quantile(V, tau)
    }
  }
  fourth_state_df$V_go <- q_vals[[1]]
  fourth_state_df$V_fga <- q_vals[[2]]
  fourth_state_df$V_punt <- q_vals[[3]]
  fourth_state_df <- fourth_state_df |> 
    mutate(max_value = pmax(V_go, V_fga, V_punt, na.rm = T)) |> 
    mutate(max_decision = 
             case_when(
               max_value == V_go ~ "go",
               max_value == V_fga ~  "fga",
               max_value == V_punt ~ "punt",
               TRUE ~ NA_character_))
  fourth_state_df$tau <- tau
  fourth_state_df
}

scam_smooth2 <- function(fourth_dat, value, knot_vec,...){
  value <- eval(substitute(value), fourth_dat)
  value.name <- substitute(value)
  
  my_smooth <- try(scam(
    eval(substitute(value))  ~ s(
      as.numeric(yardline_100_group),
      as.numeric(ydstogo_group),
      k = knot_vec,
      bs = "tedmd",
      m = 2
    ),
    family = gaussian(link = "identity"),
    data = fourth_dat,
    optimizer="efs",
    ...
  ))
  if("try-error" %in% class(my_smooth)){
    my_smooth <- try(scam(
      eval(substitute(value))  ~ s(
        as.numeric(yardline_100_group),
        as.numeric(ydstogo_group),
        k = c(knot_vec[1]+1, knot_vec[2]+1),
        bs = "tedmd",
        m = 2
      ),
      family = gaussian(link = "identity"),
      data = fourth_dat,
      optimizer="efs",
      ...
    ))
  } else {
    return(my_smooth)
  }
  if("try-error" %in% class(my_smooth)){
    my_smooth <- try(scam(
      eval(substitute(value))  ~ s(
        as.numeric(yardline_100_group),
        as.numeric(ydstogo_group),
        k = c(knot_vec[1]+2, knot_vec[2]+2),
        bs = "tedmd",
        m = 2
      ),
      family = gaussian(link = "identity"),
      data = fourth_dat,
      optimizer="efs",
      control = list(maxit = 150),
      ...
    ))
  } else {
    return(my_smooth)
  }
  if("try-error" %in% class(my_smooth)){
    print("Still broken")
    return(NULL)
  } else {
    return(my_smooth)
  }
}


get_tau_policy_smooth <- function(value_func, 
                           a_TPM,
                           tau, 
                           num_knots,
                           smooth_weights = NULL,
                           fourth_state_df = fourth_states){
  actions <- c("go", "fga", "punt")
  
  q_vals <- vector("list", 3)
  for(i in 1:3){
    for(j in 1:nrow(fourth_state_df)){
      if(sum(a_TPM[[i]][j,]) == 0){
        q_vals[[i]][j] <- NA
        next
      }
      V <- cbind.data.frame(trans_prob = a_TPM[[i]][j,],value = value_func$value) |>
        filter(trans_prob > 0) |>
        arrange(value) |>
        mutate(cum_prob = cumsum(trans_prob))
      q_vals[[i]][j] <- my_quantile(V, tau)
    }
  }
  fourth_state_df$V_go <- q_vals[[1]]
  fourth_state_df$V_fga <- q_vals[[2]]
  fourth_state_df$V_punt <- q_vals[[3]]
  
  # Smooth GO values
  if(is.null(smooth_weights)){
    weights_ <- NULL
  } else {
    weights_ <- smooth_weights[[1]]
  }
  GO_smooth <- scam_smooth2(fourth_dat = fourth_state_df,
                           value = V_go,
                           knot_vec = c(num_knots, num_knots),
                           weights = weights_)
  if(is.null(GO_smooth)){
    fourth_state_df$V_go <- NA
    fourth_state_df$V_fga <- NA
    fourth_state_df$V_punt <- NA
    fourth_state_df$max_value <- NA
    fourth_state_df$max_decision <- NA
    fourth_state_df$tau <- tau
    return(fourth_state_df)
  }
  
  fourth_state_df$V_go = fourth_state_df |>
    mutate(ydstogo_group = as.numeric(ydstogo_group),
           yardline_100_group = as.numeric(yardline_100_group)) |> 
    (\(.) predict(GO_smooth, newdata = .))()
  fourth_state_df$V_go[10] <- NA # impossible state
  
  # Smooth FG values
  if(is.null(smooth_weights)){
    weights_ <- NULL
  } else {
    weights_ <- smooth_weights[[2]]
  }
  FGA_smooth <- scam_smooth2(fourth_dat = fourth_state_df,
                           value = V_fga, 
                           knot_vec = c(num_knots, num_knots),
                           weights = weights_)
  if(is.null(FGA_smooth)){
    fourth_state_df$V_go <- NA
    fourth_state_df$V_fga <- NA
    fourth_state_df$V_punt <- NA
    fourth_state_df$max_value <- NA
    fourth_state_df$max_decision <- NA
    fourth_state_df$tau <- tau
    return(fourth_state_df)
  }
  
  fourth_state_df$V_fga = fourth_state_df |>
    mutate(ydstogo_group = as.numeric(ydstogo_group),
           yardline_100_group = as.numeric(yardline_100_group)) |>
    (\(.) predict(FGA_smooth, newdata = .))()
  fourth_state_df$V_fga[as.numeric(fourth_state_df$yardline_100_group) >= 5] <- NA
  
  # Smooth Punt values
  if(is.null(smooth_weights)){
    weights_ <- NULL
  } else {
    weights_ <- smooth_weights[[3]]
  }
  PUNT_smooth <- scam_smooth2(fourth_dat = fourth_state_df,
                             value = V_punt, 
                             knot_vec = c(num_knots, num_knots),
                             weights = weights_)
  if(is.null(PUNT_smooth)){
    fourth_state_df$V_go <- NA
    fourth_state_df$V_fga <- NA
    fourth_state_df$V_punt <- NA
    fourth_state_df$max_value <- NA
    fourth_state_df$max_decision <- NA
    fourth_state_df$tau <- tau
    return(fourth_state_df)
  }
  
  fourth_state_df$V_punt = fourth_state_df |>
    mutate(ydstogo_group = as.numeric(ydstogo_group),
           yardline_100_group = as.numeric(yardline_100_group)) |>
    (\(.) predict(PUNT_smooth, newdata = .))()
  fourth_state_df$V_punt[10] <- NA # impossible state
  fourth_state_df$V_punt[as.numeric(fourth_state_df$yardline_100_group) < 4] <- NA
  
  # Get max decision
  fourth_state_df <- fourth_state_df |> 
    mutate(max_value = pmax(V_go, V_fga, V_punt, na.rm = T)) |> 
    mutate(max_decision = 
             case_when(
               max_value == V_go ~ "go",
               max_value == V_fga ~  "fga",
               max_value == V_punt ~ "punt",
               TRUE ~ NA_character_))
  fourth_state_df$tau <- tau
  fourth_state_df
}


# FUNCTIONS FOR LOSS CURVE ------------------------------------------------

get_observed_policy <- function(dat,
                                fourth_state_df){
  data1 = dat |> 
    filter(down == 4) |> 
    filter(fourth_down_decision != "idk") 
  #if(!is.null(boot)) {data1 = data1[sample(1:nrow(data1), replace = T),]}
  data1 = data1 |> 
    filter(down == 4) |> 
    filter(fourth_down_decision != "idk") |> 
    mutate(fourth_down_decision = factor(fourth_down_decision, levels = c("go", "fga", "punt"))) |>
    group_by(ydstogo_group, yardline_100_group, fourth_down_decision) |>
    summarise(count_d= n(), .groups = "drop_last") |> 
    complete(fourth_down_decision = fourth_down_decision) |>
    pivot_wider(id_cols = c(ydstogo_group, yardline_100_group),
                names_from = fourth_down_decision,
                values_from = count_d) |>
    mutate(max_d_count =  pmax(go, fga, punt, na.rm = T),
           max_decision = case_when( max_d_count == go ~ "go",
                                     max_d_count == fga ~ "fga",
                                     max_d_count == punt ~ "punt",
                                     TRUE ~ NA_character_
           )
    ) |>
    ungroup() |>
    right_join(fourth_state_df, by = c("ydstogo_group", "yardline_100_group")) |> 
    arrange(ydstogo_group, yardline_100_group)
  
  observed_decision_df = data1 |>
    rename(observed_decision = max_decision,
           fg_n = `fga`,
           go_n = `go`,
           punt_n = `punt`) |> 
    mutate(go_n = replace_na(go_n, 0),
           fg_n = replace_na(fg_n, 0),
           punt_n = replace_na(punt_n, 0)) |>
    mutate(total_n = fg_n + go_n + punt_n) |> 
    mutate(max_n = pmax(go_n, fg_n, punt_n, na.rm = T)) |> 
    mutate(observed_decision = ifelse(max_n == 0, NA, observed_decision)) |> 
    mutate(total_n = ifelse(max_n == 0, NA, total_n)) |> 
    rename(obs_max_n = max_n, 
           obs_total_n = total_n,
           obs_go_n = go_n,
           obs_fg_n = fg_n,
           obs_punt_n = punt_n
    ) |> 
    dplyr::select( -max_d_count)
  observed_decision_df = observed_decision_df |> 
    arrange(down, ydstogo_group, yardline_100_group) |> 
    mutate(index2 = row_number()) |>
    dplyr::select(play_state, down, ydstogo_group, yardline_100_group, obs_go_n, 
           obs_fg_n, obs_punt_n, obs_max_n, obs_total_n, observed_decision)
  
  return(observed_decision_df)
}

get_bot_policy <- function(dat,
                           fourth_state_df){
  data1 = dat |>
    filter(down == 4) |>
    filter(!is.na(fourth_down_bot))
  #if(!is.null(boot)) {data1 = data1[sample(1:nrow(data1), replace = T),]}
  data1 = data1 |> 
    mutate(fourth_down_bot = factor(fourth_down_bot, levels = c("go", "fga", "punt"))) |>
    group_by(ydstogo_group, yardline_100_group, fourth_down_bot) |>
    summarise(count_d= n(), .groups = "drop_last") |> 
    complete(fourth_down_bot = fourth_down_bot) |>
    pivot_wider(id_cols = c(ydstogo_group, yardline_100_group),
                names_from = fourth_down_bot,
                values_from = count_d) |>
    mutate(max_d_count =  pmax(go, fga, punt, na.rm = T),
           max_decision = case_when( max_d_count == go ~ "go",
                                     max_d_count == fga ~ "fga",
                                     max_d_count == punt ~ "punt",
                                     TRUE ~ NA_character_
           )
    ) |>
    ungroup() |>
    right_join(fourth_state_df, by = c("ydstogo_group", "yardline_100_group")) |> 
    arrange(ydstogo_group, yardline_100_group)
  
  observed_decision_df = data1 |>
    rename(observed_decision = max_decision,
           fg_n = `fga`,
           go_n = `go`,
           punt_n = `punt`) |> 
    mutate(go_n = replace_na(go_n, 0),
           fg_n = replace_na(fg_n, 0),
           punt_n = replace_na(punt_n, 0)) |>
    mutate(total_n = fg_n + go_n + punt_n) |> 
    mutate(max_n = pmax(go_n, fg_n, punt_n, na.rm = T)) |> 
    mutate(observed_decision = ifelse(max_n == 0, NA, observed_decision)) |> 
    mutate(total_n = ifelse(max_n == 0, NA, total_n)) |> 
    rename(obs_max_n = max_n, 
           obs_total_n = total_n,
           obs_go_n = go_n,
           obs_fg_n = fg_n,
           obs_punt_n = punt_n
    ) |> 
    dplyr::select( -max_d_count)
  observed_decision_df = observed_decision_df |> 
    arrange(down, ydstogo_group, yardline_100_group) |> 
    mutate(index2 = row_number()) |>
    dplyr::select(play_state, down, ydstogo_group, yardline_100_group, obs_go_n, 
           obs_fg_n, obs_punt_n, obs_total_n, observed_decision)
  
  return(observed_decision_df)
}

get_risk_neut_policy <- function(dat,
                                 fourth_state_df = fourth_states,
                                 augment = T){
  value_func <- get_value_function(dat)
  a_TPM <- get_action_TPMs(dat, augment_third = augment)
  
  actions <- c("go", "fga", "punt")
  fourth_state_df$V_go <- NA
  fourth_state_df$V_fga <- NA
  fourth_state_df$V_punt <- NA
  
  q_vals <- vector("list", 3)
  
  for(i in 1:3){  
    for(j in 1:nrow(fourth_state_df)){
      if(sum(a_TPM[[i]][j,]) == 0){
        q_vals[[i]][j] <- NA
        next
      }
      q_vals[[i]][j] <- cbind.data.frame(trans_prob = a_TPM[[i]][j,],value = value_func$value) |>
        with(weighted.mean(value, trans_prob, na.rm = T))
    }
  }
  fourth_state_df$V_go <- q_vals[[1]]
  fourth_state_df$V_fga <- q_vals[[2]]
  fourth_state_df$V_punt <- q_vals[[3]]
  fourth_state_df <- fourth_state_df |> 
    mutate(max_value = pmax(V_go, V_fga, V_punt, na.rm = T)) |> 
    mutate(max_decision = 
             case_when(
               max_value == V_go ~ "go",
               max_value == V_fga ~  "fga",
               max_value == V_punt ~ "punt",
               TRUE ~ NA_character_))
  
  observed_decision_df = right_join(dat, fourth_state_df) |> 
    filter(fourth_down_decision != "idk") |> 
    mutate(fourth_down_decision = factor(max_decision, levels = c("go", "fga", "punt"))) |>
    group_by(play_state, down, ydstogo_group, yardline_100_group, fourth_down_decision) |>
    summarise(count_d= n(), .groups = "drop_last") |> 
    complete(fourth_down_decision = fourth_down_decision) |>
    pivot_wider(id_cols = c(ydstogo_group, yardline_100_group),
                names_from = fourth_down_decision,
                values_from = count_d) |>
    mutate(max_d_count =  pmax(go, fga, punt, na.rm = T),
           max_decision = case_when( max_d_count == go ~ "go",
                                     max_d_count == fga ~ "fga",
                                     max_d_count == punt ~ "punt",
                                     TRUE ~ NA_character_
           )
    ) |>
    ungroup() |>
    right_join(fourth_state_df |> dplyr::select(-max_decision), by = c("ydstogo_group", "yardline_100_group")) |> 
    arrange(ydstogo_group, yardline_100_group) |> 
    rename(observed_decision = max_decision,
           fg_n = `fga`,
           go_n = `go`,
           punt_n = `punt`) |> 
    mutate(go_n = replace_na(go_n, 0),
           fg_n = replace_na(fg_n, 0),
           punt_n = replace_na(punt_n, 0)) |>
    mutate(total_n = fg_n + go_n + punt_n) |> 
    mutate(max_n = pmax(go_n, fg_n, punt_n, na.rm = T)) |> 
    mutate(observed_decision = ifelse(max_n == 0, NA, observed_decision)) |> 
    mutate(total_n = ifelse(max_n == 0, NA, total_n)) |> 
    rename(obs_max_n = max_n, 
           obs_total_n = total_n,
           obs_go_n = go_n,
           obs_fg_n = fg_n,
           obs_punt_n = punt_n
    ) |> 
    dplyr::select( -max_d_count)
  observed_decision_df = observed_decision_df |> 
    arrange(down, ydstogo_group, yardline_100_group) |> 
    mutate(index2 = row_number()) |>
    dplyr::select(play_state, down, ydstogo_group, yardline_100_group, obs_go_n, 
           obs_fg_n, obs_punt_n, obs_total_n, observed_decision)
}

get_full_quant_opt <- function(pbp_dat, 
                               augment = T,
                               tau_vec = round(seq(.2,.8,by = .01), digits = 2)){
  value_func <- get_value_function(pbp_dat)
  augmented_action_TPM <- get_action_TPMs(pbp_dat, augment_third = augment)
  q_policy_smooth <- list()
  for(i in 1:length(tau_vec)) {
    #cat(i,"\r")
    q_policy_smooth[[i]] <- get_tau_policy_smooth(value_func = value_func,
                                                  a_TPM = augmented_action_TPM,
                                                  tau = tau_vec[i],
                                                  num_knots = 4)
  }
  
  q_policy_smooth_df <- bind_rows(q_policy_smooth) |>
    mutate(max_index = as.numeric(factor(max_decision, levels = c("go", "fga", "punt"))))
  
  q_policy_smooth_df
}

get_tau_hat <- function(pbp_dat, 
                        quant_policy,
                        fourth_states_,
                        tau_vec = round(seq(.2,.8,by = .01), digits = 2),
                        boot_index = NULL,
                        fourth_bot = FALSE,
                        risk_neut = FALSE
){
  
  if(fourth_bot){
    obs_policy <- get_bot_policy(dat = pbp_dat,
                                 fourth_state_df = fourth_states_)
  } else if (risk_neut){
    obs_policy <- get_risk_neut_policy(dat = pbp_dat,
                                       fourth_state_df = fourth_states_)
  } else {
    obs_policy <- get_observed_policy(dat = pbp_dat,
                                      fourth_state_df = fourth_states_)
  }
  
  action_df <- obs_policy |>
    dplyr::select(obs_go_n:obs_total_n)
  
  loss_matrix <- matrix(NA, length(tau_vec), 3)
  loss_matrix[,1] <- tau_vec
  
  # Own half vs. Opp half
  OPP_inds <- which(as.numeric(obs_policy$yardline_100_group) <= 5)
  OWN_inds <- which(as.numeric(obs_policy$yardline_100_group) > 5)
  
  for(i in 1:length(tau_vec)){
    helper_matrix <- quant_policy |>
      filter(tau == tau_vec[i]) |>
      pull(max_index) |>
      (\(.) cbind(1:100, .))() # 1:100 indexes the fourth down states
    
    if(sum(is.na(helper_matrix[,2])) == 100) next
    suboptimal_decisions <- action_df$obs_total_n - as.matrix(action_df)[helper_matrix]
    loss_matrix[i,2] <- sum(suboptimal_decisions[OPP_inds], na.rm = T)/sum(action_df$obs_total_n[OPP_inds], na.rm = T) 
    loss_matrix[i,3] <- sum(suboptimal_decisions[OWN_inds], na.rm = T)/sum(action_df$obs_total_n[OWN_inds], na.rm = T) 
  }
  colnames(loss_matrix) <- c("tau", "OPP", "OWN")
  loss_matrix[loss_matrix == 0] <- NA
  min_loss <- list()
  min_loss[[1]] <- as.data.frame(loss_matrix) |>
    dplyr::select(-OWN) |>
    filter(OPP == min(OPP, na.rm = T)) |>
    rename(min_loss = OPP) |>
    mutate(weight = 1/n(),
           field_region = "OPP")
  
  min_loss[[2]] <- as.data.frame(loss_matrix) |>
    dplyr::select(-OPP) |>
    filter(OWN == min(OWN, na.rm = T)) |>
    rename(min_loss = OWN) |>
    mutate(weight = 1/n(),
           field_region = "OWN")
  min_loss <- bind_rows(min_loss)
  
  if(!is.null(boot_index)){
    min_loss$boot_ind <- boot_index
  }
  min_loss
}


get_loss_curve <- function(pbp_dat, 
                           augment = T, 
                           fourth_states_,
                           tau_vec = round(seq(.2,.8,by = .01), digits = 2)){
  obs_policy <- get_observed_policy(dat = pbp_dat,
                                    fourth_state_df = fourth_states_)
  value_func <- get_value_function(pbp_dat)
  augmented_action_TPM <- get_action_TPMs(pbp_dat,
                                          augment_third = augment)
  q_policy_smooth <- list()
  for(i in 1:length(tau_vec)) {
    #cat(tau_vec[i],"\r") Uncomment to watch progress
    q_policy_smooth[[i]] <- get_tau_policy_smooth(value_func = value_func,
                                                  a_TPM = augmented_action_TPM,
                                                  tau = tau_vec[i],
                                                  num_knots = 4)
  }
  
  q_policy_smooth_df <- bind_rows(q_policy_smooth) |>
    mutate(max_index = as.numeric(factor(max_decision, levels = c("go", "fga", "punt"))))
  
  action_df <- obs_policy |>
    dplyr::select(obs_go_n:obs_total_n)
  
  loss_matrix <- matrix(NA, length(tau_vec), 3)
  loss_matrix[,1] <- tau_vec
  
  # Own half vs. Opp half
  OPP_inds <- which(as.numeric(obs_policy$yardline_100_group) <= 5)
  OWN_inds <- which(as.numeric(obs_policy$yardline_100_group) > 5)
  
  for(i in 1:length(tau_vec)){
    helper_matrix <- q_policy_smooth_df |>
      filter(tau == tau_vec[i]) |>
      pull(max_index) |>
      (\(.) cbind(1:100, .))() # 1:100 indexes the fourth down states
    
    suboptimal_decisions <- action_df$obs_total_n - as.matrix(action_df)[helper_matrix]
    loss_matrix[i,2] <- sum(suboptimal_decisions[OPP_inds], na.rm = T)/sum(action_df$obs_total_n, na.rm = T) 
    loss_matrix[i,3] <- sum(suboptimal_decisions[OWN_inds], na.rm = T)/sum(action_df$obs_total_n, na.rm = T) 
  }
  colnames(loss_matrix) <- c("tau", "OPP", "OWN")
  loss_matrix[loss_matrix == 0] <- NA
  loss_matrix
}

highlight = function(x, pat, color="black", family="") {
  ifelse(grepl(pat, x), glue("<b style='font-family:{family}; color:{color}'>{x}</b>"), x)
}

