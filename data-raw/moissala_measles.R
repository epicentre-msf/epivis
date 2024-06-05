## code to prepare `moissala_measles` dataset goes here

moissala_measles <- read_rds("https://github.com/epicentre-msf/simulated-data/raw/main/data/clean/simulated_measles_ll.rds")

usethis::use_data(moissala_measles, overwrite = TRUE)
