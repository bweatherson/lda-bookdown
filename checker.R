require(tidyverse)
id_journal <- filtered_metadata |> select(id, journal)
temp <- filtered_grams |>
#  mutate(id = str_sub(id, start = 29) |>  as.numeric()) |>
  left_join(id_journal, by = "id") |>
  group_by(journal) |>
  tally()

temp2 <- all_grams |>
  group_by(id) |>
  tally()