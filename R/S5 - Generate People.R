required <- c("rvest", "tidyverse", "magrittr", "stringi", "maps")
lapply(required, require, character.only = TRUE)
options(readr.show_col_types = FALSE)

QID <- read_csv("~/GitHub/Nitrate-SPARQL/output/fs/P2889 - FS.csv")

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
    fid, QID, name, lifespan=ls,
    age_at_death, birth_country,
    children = count_children, siblings = count_siblings)

nationality <-
  people %>%
  dplyr::count(birth_country) %>%
  arrange(desc(n))

landing <- ascendancy %>% filter(ascendancy < 16384)
core_people <- people %>%
  filter(fid %in% landing$fid) %T>%
  write.csv("data/core-people.csv", row.names=FALSE)

for (i in 1:length(cuts$slice)) {
    cut <- cut_ascendancy %>% filter(cut == i)
    bind_rows(core_people,
              people %>% filter(fid %in% cut$fid)) %>%
    write.csv(., paste0("data/people/slice-", str_pad(i, 3, pad = "0"), ".csv"), row.names = FALSE)
}

rm(required, QID, i, cut, core_people, landing)
