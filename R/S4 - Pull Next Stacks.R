required <- c("rvest", "tidyverse", "magrittr", "stringi",
              "foreach", "doParallel", "parallel")
lapply(required, require, character.only = TRUE)
options(readr.show_col_types = FALSE)

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

for (s in 10:13) {

  dir.create(paste0("people/", s), showWarnings = FALSE)

  layer <-
    read_csv(paste0("input/parents/rel-", s - 1, ".csv")) %>%
    distinct(parent)

  foreach(file = layer$parent, .combine = 'c', .errorhandling = 'remove') %dopar% {
    if (!file.exists(paste0("people/", s, "/", file, ".json"))) {
      download.file(
        url = paste0("https://ancestors.familysearch.org/service/tree/tree-data/published/persons/", file),
        destfile = paste0("people/", s, "/", file, ".json"),
        quiet = FALSE)
    }
  }

  files <- list.files(paste0("people/", s), ".json", full.names = TRUE)
  new.parent <- NULL

  for (i in files) {
    person <- jsonlite::fromJSON(i)$person
    if(length(person$parent$identifier) > 0) {
      new.parent <-
        new.parent %>%
        bind_rows(
          data.frame(fid = tools::file_path_sans_ext(basename(i)),
                     parent = person$parent$identifier,
                     gender = person$parent$gender))
    }
  }

  message(paste(s, "complete! Next layer has", nrow(new.parent), "parents."))

  new.parent %>% filter(!is.na(parent)) %>%
    write.csv(., paste0("input/parents/rel-", s, ".csv"), row.names = FALSE)
}

stopCluster(cl)

rm(required, cl, i, s, new.parent, layer, files, person)
