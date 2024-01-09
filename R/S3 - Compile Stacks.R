required <- c("rvest", "tidyverse", "magrittr", "stringi")
lapply(required, require, character.only = TRUE)
options(readr.show_col_types = FALSE)

QID <- read_csv("~/GitHub/Nitrate-SPARQL/output/fs/P2889 - FS.csv")

doubles <- sapply(c(0:50), function(x) 2 ** x) %>% as.array()
currentmax <- 2 ** 30 ## 2^N of current max list

# read_csv("input/base.csv") %>%
#   filter(gen == 3) %>%
#   select(gen, ascendency, id, gender) %>%
#   write.csv("input/layers/layer-3.csv", row.names = FALSE)
#
# for (i in 4:30) {
#   layer <-
#     read_csv(paste0("input/layers/layer-", i - 1, ".csv")) %>%
#     select(ascendency, id) %>%
#     left_join(read_csv(paste0("input/parents/rel-", i - 1, ".csv")),
#       by=c("id" = "fid"),
#       relationship = "many-to-many") %>%
#     filter(!is.na(parent)) %>%
#     reframe(
#       ascendency = ifelse(
#         gender == "Female", (ascendency * 2) + 1, (ascendency * 2)),
#       id = parent,
#       gender = gender,
#       gen = i
#     ) %T>%
#     write.csv(., paste0("input/layers/layer-", i, ".csv"), row.names = FALSE)
# }

people <-
  list.files(path = "input/people",
             pattern = "*.csv",
             full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  distinct(fid, .keep_all = TRUE) %>%
  left_join(QID, by=c("fid" = "value")) %>%
  select(fid, QID, name, everything())

relations <-
  list.files(path = "input/parents",
             pattern = "*.csv",
             full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  distinct()

pedigree_pie <-
  list.files(path = "input/layers",
           pattern = "*.csv",
           full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  #complete(ascendency = seq(4, currentmax-1, by=1)) %>%
  rowwise() %>%
  mutate(gen = min(findInterval(ascendency, doubles))) %>%
  group_by(gen) %>%
    mutate(sequence = ave(gen, FUN = seq_along)) %>%
  ungroup() %>%
  mutate(
    gen_count = 2^(gen-1),
    half = ifelse(between(sequence / gen_count, 0, .5), "Paternal", "Maternal"),
    quartile = case_when(
      between(sequence / gen_count, 0, 1/4) ~ "Paternal Grandfather",
      between(sequence / gen_count, 1/4, 1/2) ~ "Paternal Grandmother",
      between(sequence / gen_count, 1/2, 3/4) ~ "Maternal Grandfather",
      between(sequence / gen_count, 3/4, 1) ~ "Maternal Grandmother"
    ),
    start = 360 * sequence / gen_count - (360/gen_count),
    end = sequence * (360/gen_count),
    color = case_when(
      ## Paternal Grandfather, "Spring"
      quartile == "Paternal Grandfather" & gender == "Male" ~ "#B24C6D",
      quartile == "Paternal Grandfather" & gender == "Female" ~ "#FFC1D5",
      ## Paternal Grandmother, "Summer"
      quartile == "Paternal Grandmother" & gender == "Male" ~ "#07B2D9",
      quartile == "Paternal Grandmother" & gender == "Female" ~ "#88DFF2",
      ## Maternal Grandfather, "Fall"
      quartile == "Maternal Grandfather" & gender == "Male" ~ "#738B4F",
      quartile == "Maternal Grandfather" & gender == "Female" ~ "#A6D86F",
      ## Maternal Grandmother, "Winter
      quartile == "Maternal Grandmother" & gender == "Male" ~ "#8950A1",
      quartile == "Maternal Grandmother" & gender == "Female" ~ "#C881CF",
      .default = "#EEEEEE"
    )) %>%
  left_join(people %>% select(-gender), by=c("id" = "fid")) %>%
  select(ascendency, id, gender, gen, sequence, quartile, start, end, color, name, lifespan)

dupes.review <-
  pedigree_pie %>%
  filter(!is.na(id)) %>%
  dplyr::count(id) %>%
  filter(n > 1) %>%
  arrange(desc(n)) #%>% left_join(pedigree_pie, by=join_by(id))

generations <-
  pedigree_pie %>%
  group_by(gen) %>%
  reframe(
    identified = n_distinct(id[!is.na(id)]),
    total = n(),
    per.known = round(identified / total * 100, digits=2),
    missing = total - identified
  )

RAD.pedigree <-
  pedigree_pie %>%
  filter(!is.na(id)) %>%
  select(ascendency) %T>%
  write.csv(., "rad.csv", row.names = FALSE)

rm(required, i, currentmax, doubles)
