required <- c("rvest", "tidyverse", "magrittr", "stringi",
              "foreach", "doParallel", "parallel", "data.table")
lapply(required, require, character.only = TRUE)
options(readr.show_col_types = FALSE)

bind_rows(
  c(fid="me", parent="dad", gender="Male",   ascendancy=2),     # Father
  c(fid="me", parent="mom", gender="Female", ascendancy=3)) %>% # Mother
  write.csv(., "~/GitHub/Great-Procreations/input/ascendancy/gen-001-1.csv", row.names = FALSE)

bind_rows(
  c(fid="dad", parent="GHVL-B4K", gender="Male",   ascendancy=4),     # Paternal Grandfather
  c(fid="dad", parent="GHK6-8WT", gender="Female", ascendancy=5),     # Paternal Grandmother
  c(fid="mom", parent="L6L2-JJB", gender="Male",   ascendancy=6),     # Maternal Grandfather
  c(fid="mom", parent="LNKH-8TG", gender="Female", ascendancy=7)) %>% # Maternal Grandmother
write.csv(., "~/GitHub/Great-Procreations/input/ascendancy/gen-002-1.csv", row.names = FALSE)

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

### reminder: this is a time consuming script, only run when updating files

for (s in 34) { ## 3:x runs from the above grandparents (in gen-002.csv) to x generations

  previous_run <-
    list.files(
      path = "~/GitHub/Great-Procreations/input/ascendancy",
      pattern = paste0("gen-", str_pad(s - 1, 3, pad = "0")),
      full.names = TRUE) %>%
    lapply(read_csv) %>%
    rbindlist() %>%
    mutate(
      file = paste0("~/GitHub/Family-Tree/records/", parent, ".json"),
      batch = ceiling(row_number()/5000))


  files <- previous_run %>% select(file) %>% distinct(file)

  foreach(file = files$file, .combine = 'c', .errorhandling = 'remove') %dopar% {
    if (!file.exists(file)) {
      download.file(
        url = paste0("https://ancestors.familysearch.org/service/tree/tree-data/published/persons/", tools::file_path_sans_ext(basename(file))),
        destfile = file,
        quiet = FALSE)
    }
  }

  rm(files)

  foreach(b = 1:max(previous_run$batch), .combine = 'c') %dopar% {

    required <- c("rvest", "tidyverse", "stringi", "data.table")
    lapply(required, require, character.only = TRUE)

    previous_run <-
      list.files(
        path = "~/GitHub/Great-Procreations/input/ascendancy",
        pattern = paste0("gen-", str_pad(s - 1, 3, pad = "0")),
        full.names = TRUE) %>%
      lapply(read_csv) %>%
      rbindlist() %>%
      mutate(
        file = paste0("~/GitHub/Family-Tree/records/", parent, ".json"),
        batch = ceiling(row_number()/5000)) %>%
      filter(batch == b)

    new.parent <- NULL

    for (i in previous_run$ascendancy) {
      if (file.exists(previous_run$file[previous_run$ascendancy == i])) {
        person <- jsonlite::fromJSON(previous_run$file[previous_run$ascendancy == i])$person

        if(length(person$parent$gender[person$parent$gender == "Male"]) == 1 |
           length(person$parent$gender[person$parent$gender == "Female"]) == 1) {
            new.parent <-
              new.parent %>%
              bind_rows(
                data.frame(fid = previous_run$parent[previous_run$ascendancy == i],
                           ascendancy = ifelse(person$parent$gender == "Male", i * 2, (i * 2) + 1),
                           parent = person$parent$identifier,
                           gender = person$parent$gender))
        }
      }
    }

    write.csv(new.parent, paste0("~/GitHub/Great-Procreations/input/ascendancy/gen-", str_pad(s, 3, pad = "0"), "-", b, ".csv"), row.names = FALSE)
  }

  latest_run <-
    list.files(
      path = "~/GitHub/Great-Procreations/input/ascendancy",
      pattern = paste0("gen-", str_pad(s, 3, pad = "0")),
      full.names = TRUE) %>%
    lapply(read_csv) %>%
    rbindlist()

  message(paste(s, "complete! Next layer has", length(unique(latest_run$parent)), "parents."))

}

stopCluster(cl)

rm(required, cl, s, i, person, new.parent)
