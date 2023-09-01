clean_pbp <- readRDS("./data/pbp_2014_2022.rds")

list_of_game_id = sort(unique(clean_pbp$game_id))
length(list_of_game_id)

n_boot = 200
set.seed(2022)
index_mat = replicate(n_boot, sample(1:length(list_of_game_id), replace = T))
index_mat %>% dim

saveRDS(index_mat, file = "./data/boot_indices.rds")
