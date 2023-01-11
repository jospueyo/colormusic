library(tidyverse)
library(tidytext)

# CAPS are for the first octave and small letters are for the secon octave in the piano
notes <- tribble(
  ~ notes_a, ~ notes_la, ~ position, ~ color,
  "A",       "LA",      2.5,         "darkblue",
  "B",       "SI",      3,           "violet",
  "C",       "DO",      0,           "red",
  "D",       "RE",      0.5,         "orange",
  "E",       "MI",      1,           "yellow",
  "F",       "FA",      1.5,         "green",
  "G",       "SOL",     2,           "lightblue",
  "a",       "la",      6,           "darkblue",
  "b",       "si",      6.5,         "violet",
  "c",       "do",      3.5,         "red",
  "d",       "re",      4,           "orange",
  "e",       "mi",      4.5,         "yellow",
  "f",       "fa",      5,           "green",
  "g",       "sol",     5.5,         "lightblue"
)

# this function converts the text file into a tidy table with one note per row
tidy_song <- function(filename, notation = c("notes_a", "notes_la")){
  read.csv(file.path("songs", filename), header = F) |>
    unnest_tokens(notes, V1, to_lower = F) |>
    mutate(group = ceiling(row_number()/25)) |>
    group_by(group) |>
    mutate(id = row_number()) |>
    ungroup() |>
    left_join(notes, by= c(notes = notation))
}

create_out_score_segment <- function(tidy_song){
  tidy_song |>
    filter(notes %in% c("C", "a", "b", "DO", "la", "si")) |>
    mutate(line = if_else(notes %in% c("b", "si"), 6, position))
}

size_g_key <- function(n_lines){
  case_when(
    n_lines == 2 ~ c(50,3),
    n_lines == 3 ~ c(35, 3.1),
    n_lines == 4 ~ c(30, 3.2),
    n_lines == 5 ~ c(28, 3.35),
    TRUE ~ c(55, 3)
  )
}

# the function to create a plot
draw_score <- function(filename, notation){
  song <- tidy_song(filename, notation)

  out_score <- create_out_score_segment(song)

  g_key <- size_g_key(n_lines = max(song$group))

  score <- song |>
    ggplot(aes(x = id, y=position, color = color))+
    geom_point(show.legend = F, size=8)+
    geom_hline(yintercept = 1:5)+
    geom_segment(data = out_score,
                 mapping = aes(x = id-0.3, xend = id+0.3, y = line, yend = line), color = "black")+
    geom_text(x = -1.5, y = g_key[2],label = "\U1D11E", size = g_key[1], color="black")+
    scale_color_identity()+
    xlim(-3,25.5)+
    ylim(-2,9)+
    facet_wrap(vars(group), ncol = 1)+
    theme_void()+
    ggtitle(str_remove(filename, fixed(".txt")))+
    theme(plot.title = element_text(hjust=0.5, size = 20),
          strip.background = element_blank(),
          strip.text.x = element_blank())

  ggsave(filename = file.path("songs", str_replace(filename, "\\.txt$", ".jpg")), plot = score, width = 11, height = 5)
}
