library(tidyverse)

test_df <- tibble(notes = sample(c(LETTERS[1:7], letters[1:7]), 30L, replace = TRUE)) |>
  mutate(id = row_number())

notes <- tribble(
  ~ notes, ~ position, ~ color,
  "A", 2.5, "darkblue",
  "B", 3, "violet",
  "C", 0, "red",
  "D", 0.5, "orange",
  "E", 1, "yellow",
  "F", 1.5, "green",
  "G", 2, "lightblue",
  "a", 6, "darkblue",
  "b", 3, "violet",
  "c", 3.5, "red",
  "d", 4, "orange",
  "e", 4.5, "yellow",
  "f", 5, "green",
  "g", 5.5, "lightblue",

)



test_df |>
  left_join(notes) |>
  ggplot(aes(x = id, y=position, color = color))+
  geom_point(show.legend = F, size=10)+
  scale_color_identity()+
  ylim(-2,9)+
  theme_void()+
  geom_hline(yintercept = 1:5)+
  ggtitle("Test song")+
  theme(plot.title = element_text(hjust=0.5))
ggsave("songs/test.jpg", width = 11)
