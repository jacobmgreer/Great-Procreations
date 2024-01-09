required <- c("rvest", "tidyverse", "magrittr", "stringi")
lapply(required, require, character.only = TRUE)
options(readr.show_col_types = FALSE)

# files <- list.files("records", ".json", full.names = TRUE)
#
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
# rm(files, i)

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
#   select(Child=fid, Father=Male, Mother=Female)
#
# #### create ascendancy
# ascendancy <-
#   bind_rows(
#     c(fid="GHVL-B4K", ascendancy=4),     # Paternal Grandfather
#     c(fid="GHK6-8WT", ascendancy=5),     # Paternal Grandmother
#     c(fid="L6L2-JJB", ascendancy=6),     # Maternal Grandfather
#     c(fid="LNKH-8TG", ascendancy=7)) %>% # Maternal Grandmother
#   left_join(rel.sex, by=c("fid" = "Child")) %>%
#   mutate(
#     ascendancy = as.numeric(ascendancy),
#     gen = as.numeric(3))
#
# for (i in 4:30) {
#     ascendancy <-
#       bind_rows(ascendancy,
#                 ascendancy %>%
#                   filter(gen == i - 1) %>%
#                   reframe(
#                     fid = Father,
#                     ascendancy = ascendancy * 2,
#                     gen = i) %>%
#                   filter(!is.na(fid)) %>%
#                   left_join(rel.sex, by=c("fid" = "Child"))) %>%
#       bind_rows(.,
#                 ascendancy %>%
#                   filter(gen == i - 1) %>%
#                   reframe(
#                     fid = Mother,
#                     ascendancy = (ascendancy * 2) + 1,
#                     gen = i) %>%
#                   filter(!is.na(fid)) %>%
#                   left_join(rel.sex, by=c("fid" = "Child"))) %>%
#       arrange(ascendancy)
# }
# rm(i)s

redo <- ascendancy %>%
  filter(gen %in% 3:15) %>%
  select(ascendancy, fid) %T>%
  write.csv("pedigree.csv", row.names = FALSE)

shared.ancestors <-
  ascendancy %>%
  filter(duplicated(fid)) %>%
  count(fid) %>%
  arrange(desc(n)) %>%
  filter(n > 1)

rm(required)
