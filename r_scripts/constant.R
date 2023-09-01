library(tidyverse)

ydstogo_bin = 1
MAX_ydstogo = 10
yardline_100_bin = 10
MAX_yardline_100 = 91

down_label = c(1,2,3,4)
ydstogo_label = c("1",   "2"  , "3" ,  "4"  , "5"  , "6"  , "7" ,  "8"  , "9"  , "10+")
yardline_label = c("1-10", "11-20","21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91+")
fix_yardline_label = c("1-10", "11-20","21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-99")

score_states = c("touchdown", "field_goal","safety")
kickoff_lines = c(20,25,30,35,40,50)
fourth_actions = c("go", "fga", "punt")

touchdown_value = 6.95

seasons <- 2014:2022
wp_groups <- c("(0,0.2]", "(0.2,0.8]", "(0.8,1]")
dec_limit <- 25

# Play state data frame  ---------------------------------
play_states <-
  expand.grid(down = down_label,
              ydstogo_group = ydstogo_label,
              yardline_100_group = yardline_label) |> 
  arrange(down,
          ydstogo_group,
          yardline_100_group) |> 
  unite(play_state,
        down,
        ydstogo_group,
        yardline_100_group,
        sep = "_", remove = F) |> 
  as_tibble()