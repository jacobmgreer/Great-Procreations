required <- c("rvest", "tidyverse", "magrittr", "stringi")
lapply(required, require, character.only = TRUE)

for (j in 3:15) {
  files <-
    list.files(paste0("people/", j), ".json", recursive = TRUE, full.names = TRUE) %>%
    as_tibble() %>%
    reframe(
      file = value,
      fid = tools::file_path_sans_ext(basename(value))
    ) %>%
    distinct()

  rel.parent <- NULL

  for (i in files$file) {
    person <- jsonlite::fromJSON(i)$person
    if(length(person$parent$identifier) > 0) {
      rel.parent <-
        rel.parent %>%
        bind_rows(
          data.frame(fid = tools::file_path_sans_ext(basename(i)),
                     parent = person$parent$identifier,
                     gender = person$parent$gender)) %>%
        distinct()
    }
  }

  write.csv(rel.parent, paste0("input/parents/rel-", j, ".csv"), row.names = FALSE)

}

rm(i, required, rel.parent, files)
