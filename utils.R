library(tidyverse)
library(tidytext)

# this function converts the text file into a tidy table with one note per row

tidy_song <- function(filename){
  read.csv(file.path("songs", filename), header = F) |>
    unnest_tokens(notes, V1, to_lower = F) |>
    mutate(group = ceiling(row_number()/14)) |>
    group_by(group) |>
    mutate(id = row_number()) |>
    ungroup() |>
    left_join(notes)
}

#
