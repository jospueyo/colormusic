library(tidyverse)
library(tidytext)




filename <- "test song.txt"

test_song <- read.csv(file.path("songs", filename), header = F) |>
  unnest_tokens(notes, V1, to_lower = F) |>
  mutate(group = ceiling(row_number()/14)) |>
  group_by(group) |>
  mutate(id = row_number()) |>
  ungroup()



song <- test_song |>
  left_join(notes)

out_score <- song |>
  filter(notes %in% c("C", "a", "b", "DO", "la", "si")) |>
  mutate(line = if_else(notes %in% c("b", "si"), 6, position))

g_key <- case_when(
  # max(song$group) == 1 ~ 50,
  max(song$group) == 2 ~ c(50,3),
  max(song$group) == 3 ~ c(35, 3.1),
  max(song$group) == 4 ~ c(30, 3.2),
  max(song$group) == 5 ~ c(28, 3.35),
)

song |>
  ggplot(aes(x = id, y=position, color = color))+
  geom_point(show.legend = F, size=6)+
  geom_hline(yintercept = 1:5)+
  geom_segment(data = out_score,
               mapping = aes(x = id-0.3, xend = id+0.3, y = line, yend = line), color = "black")+
  geom_text(x = -0.5, y = g_key[2],label = "\U1D11E", size = g_key[1], color="black")+
  scale_color_identity()+
  xlim(-1,14)+
  ylim(-2,9)+
  facet_wrap(vars(group), ncol = 1)+
  theme_void()+
  ggtitle(str_remove(filename, fixed(".txt")))+
  theme(plot.title = element_text(hjust=0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank())
ggsave(file.path("songs", str_replace(filename, "\\.txt$", ".jpg")), width = 5, height = 11)
