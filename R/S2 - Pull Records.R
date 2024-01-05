required <- c("rvest", "tidyverse", "magrittr", "jsonlite",
              "foreach", "doParallel", "parallel")
lapply(required, require, character.only = TRUE)
options(readr.show_col_types = FALSE)

#unlink("people/base/*")

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

for (i in 1:9) {

  files <-
    pedigree_pie %>%
    select(gen, id) %>%
    filter(!is.na(id)) %>%
    filter(gen == i)

  dir.create(paste0("people/base/", i), showWarnings = FALSE)

  foreach(file = files$id, .combine = 'c', .errorhandling = 'remove') %dopar% {
    if (!file.exists(paste0("people/base/", i, "/", file, ".json"))) {
      download.file(
        url = paste0("https://ancestors.familysearch.org/service/tree/tree-data/published/persons/", file),
        destfile = paste0("people/base/", i, "/", file, ".json"),
        quiet = FALSE)
    }
  }

  message(paste(i, "complete!"))
}

stopCluster(cl)
rm(required, i, cl, files)
