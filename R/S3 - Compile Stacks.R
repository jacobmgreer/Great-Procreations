required <- c("rvest", "tidyverse", "magrittr", "stringi", "maps")
lapply(required, require, character.only = TRUE)
options(readr.show_col_types = FALSE)

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
# rm(i)


# relations <-
#   list.files(path = "input/parents",
#              pattern = "*.csv",
#              full.names = TRUE) %>%
#   lapply(read_csv) %>%
#   bind_rows() %>%
#   distinct()

# dupes.review <-
#   pedigree_pie %>%
#   filter(!is.na(id)) %>%
#   dplyr::count(id) %>%
#   filter(n > 1) %>%
#   arrange(desc(n)) #%>% left_join(pedigree_pie, by=join_by(id))

rm(required)
