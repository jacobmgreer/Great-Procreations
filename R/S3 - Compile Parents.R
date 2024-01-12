required <- c("rvest", "tidyverse", "magrittr", "stringi", "maps")
lapply(required, require, character.only = TRUE)
options(readr.show_col_types = FALSE)

files <- list.files("../Family-Tree/records", ".json", full.names = TRUE)

parents <- NULL
for (i in files) {
  person <- jsonlite::fromJSON(i)$person
  if(length(person$parent$identifier) > 0) {
    parents <-
      parents %>%
      bind_rows(
        data.frame(fid = tools::file_path_sans_ext(basename(i)),
                   parent = person$parent$identifier,
                   gender = person$parent$gender))
  }
}

rm(files, i, person)

write.csv(parents, "../Family-Tree/reports/parents.csv", row.names=FALSE)

#### build relationships
error.parent <-
  parents %>%
  dplyr::count(fid, gender) %>%
  filter(n > 1)

rel.sex <-
  parents %>%
  filter(!fid %in% error.parent$fid) %>%
  spread(gender, parent) %>%
  select(Child=fid, Father=Male, Mother=Female) %T>%
  write.csv(., "../Family-Tree/reports/relationships-sex.csv", row.names = FALSE)

rm(required)
