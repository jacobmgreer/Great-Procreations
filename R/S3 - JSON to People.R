required <- c("rvest", "tidyverse", "magrittr", "stringi", "maps")
lapply(required, require, character.only = TRUE)
options(readr.show_col_types = FALSE)

QID <- read_csv("~/GitHub/Nitrate-SPARQL/output/fs/P2889 - QID.csv")

## requires ascendancy from S2

people <-
  list.files(path = "input/people",
             pattern = "*.csv",
             full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  distinct(fid, .keep_all = TRUE) %>%
  left_join(QID, by=c("fid" = "value")) %>%
  mutate(
    birth = str_extract(birth, "\\d{4}"),
    death = str_extract(death, "\\d{4}"),
    ls = paste(ifelse(is.na(birth), "Unknown", birth), "-", ifelse(is.na(death), "Unknown", death)),
    birth_country = map.where(database="world", place_birth_lon, place_birth_lat)
  ) %>%
  select(
    fid, QID, name, birth, death, lifespan=ls, article,
    age_at_death, birth_country,
    children = count_children, siblings = count_siblings)

wiki.find <-
  list.files(path = "input/people",
             pattern = "*.csv",
             full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  distinct(fid, .keep_all = TRUE) %>%
  left_join(QID, by=c("fid" = "value")) %>%
  select(fid, name, lifespan, count_stories, count_sources, everything()) %>%
  filter(count_memories > 10) %>%
  filter(is.na(QID))

# landing <- ascendancy %>% filter(ascendancy < 252145) %>% pull(fid)
# people %>%
#   filter(fid %in% landing) %>%
#   write.csv("data/core-people.csv", row.names=FALSE)
#
# for (cut in 32:63) {
#   cuts <-
#     read_csv(paste0(paste0("data/cuts/slice-", str_pad(cut, 3, pad = "0"), ".csv"))) %>%
#     distinct(fid)
#   people %>%
#     filter(fid %in% cuts$fid) %>%
#     write.csv(., paste0("data/people/slice-", str_pad(cut, 3, pad = "0"), ".csv"), row.names = FALSE)
# }

wikitable <-
  ascendancy %>%
  left_join(people, by="fid", relationship = "many-to-many") %>%
  filter(!is.na(article)) %>%
  group_by(fid) %>%
  filter(ascendancy == min(ascendancy)) %>%
  select(maxasc = ascendancy, fid, QID, article, name, lifespan, birth_country) %T>%
  write.csv(., "data/wikitable.csv", row.names = FALSE)

rm(required, cuts, cut, landing)
