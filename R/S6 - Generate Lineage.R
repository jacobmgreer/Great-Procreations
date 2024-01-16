required <- c("rvest", "plyr", "tidyverse", "magrittr", "jsonlite",
              "qdapRegex", "fuzzyjoin", "readr", "tools",
              "textutils", "purrr", "igraph")
lapply(required, require, character.only = TRUE)
options(readr.show_col_types = FALSE)

plot.connections <-
  ascendancy %>%
  filter(ascendancy > 7) %>%
  filter(!is.na(Father)) %>%
  select(from=fid, to=Father) %>%
  bind_rows(
    ascendancy %>%
      filter(!is.na(Mother)) %>%
      select(from=fid, to=Mother)
  )

# plot.ancestry <-
#   graph_from_data_frame(plot.connections, directed=TRUE)

plot.trees <-
  map(V(plot.ancestry), ~ names(subcomponent(plot.ancestry, .x, mode="out"))) %>%
  map_df(~data.frame(from=.x), .id="to") %>%
  filter(to != from) %>%
  distinct() %>%
  group_by(from) %>%
  summarize(
    related_to = n_distinct(to),
    lineage = paste(to, collapse = ","))

rm(required)
