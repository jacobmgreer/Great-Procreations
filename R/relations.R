required <- c("rvest", "tidyverse", "magrittr", "stringi", "maps")
lapply(required, require, character.only = TRUE)
options(readr.show_col_types = FALSE)

# files <- list.files("../Family-Tree/records", ".json", full.names = TRUE)
# parents <- NULL
# for (i in files) {
#   person <- jsonlite::fromJSON(i)$person
#   if(length(person$parent$identifier) > 0) {
#     parents <-
#       parents %>%
#       bind_rows(
#         data.frame(fid = tools::file_path_sans_ext(basename(i)),
#                    parent = person$parent$identifier,
#                    gender = person$parent$gender))
#   }
# }
# rm(files, i, person)
# write.csv(parents, "../Family-Tree/reports/parents.csv", row.names=FALSE)

# #### build relationships
# error.parent <-
#   parents %>%
#   dplyr::count(fid, gender) %>%
#   filter(n > 1)
#
# rel.sex <-
#   parents %>%
#   filter(!fid %in% error.parent$fid) %>%
#   spread(gender, parent) %>%
#   select(Child=fid, Father=Male, Mother=Female) %T>%
#   write.csv(., "../Family-Tree/reports/relationships-sex.csv", row.names = FALSE)
#
# #### create ascendancy
ascendancy <-
  bind_rows(
    c(fid="GHVL-B4K", ascendancy=4),     # Paternal Grandfather
    c(fid="GHK6-8WT", ascendancy=5),     # Paternal Grandmother
    c(fid="L6L2-JJB", ascendancy=6),     # Maternal Grandfather
    c(fid="LNKH-8TG", ascendancy=7)) %>% # Maternal Grandmother
  left_join(rel.sex, by=c("fid" = "Child")) %>%
  mutate(
    ascendancy = as.numeric(ascendancy),
    gen = as.numeric(3))

for (i in 4:30) {
    ascendancy <-
      bind_rows(ascendancy,
                ascendancy %>%
                  filter(gen == i - 1) %>%
                  reframe(
                    fid = Father,
                    ascendancy = ascendancy * 2,
                    gen = i) %>%
                  filter(!is.na(fid)) %>%
                  left_join(rel.sex, by=c("fid" = "Child"))) %>%
      bind_rows(.,
                ascendancy %>%
                  filter(gen == i - 1) %>%
                  reframe(
                    fid = Mother,
                    ascendancy = (ascendancy * 2) + 1,
                    gen = i) %>%
                  filter(!is.na(fid)) %>%
                  left_join(rel.sex, by=c("fid" = "Child"))) %>%
      arrange(ascendancy)
}
rm(i)

ascendancy %>%
  filter(gen %in% 3:15) %>%
  select(ascendancy, fid) %>%
  write.csv("data/pedigree.csv", row.names = FALSE)

shared.ancestors <-
  ascendancy %>%
  filter(duplicated(fid)) %>%
  count(fid) %>%
  arrange(desc(n)) %>%
  filter(n > 1)

generations <-
  ascendancy %>%
  group_by(gen) %>%
  reframe(
    identified = n_distinct(fid[!is.na(fid)]),
    total = n(),
    per.known = round(identified / total * 100, digits=2),
    missing = total - identified
  )

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

landing <- ascendancy %>% filter(ascendancy < 16384)
people %>%
  filter(fid %in% landing$fid) %>%
  write.csv("data/people-core.csv", row.names=FALSE)
rm(landing)

nationality <-
  people %>%
  dplyr::count(birth_country) %>%
  arrange(desc(n))

rm(required)
