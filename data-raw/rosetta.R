## code to prepare `rosetta` dataset goes here

library(gplyr)
library(tidyverse)
library(readxl)

rosetta <- read_excel("S:/data/databased/raw/rosetta/rosetta.xlsx") %>%
  filter_out_na() %>%
  filter_out_na(english) %>%
  fill(category, .direction = "down") %>%
  distinct()

databased::database_it(rosetta)

remove(rosetta)

#usethis::use_data(DATASET, overwrite = TRUE)
