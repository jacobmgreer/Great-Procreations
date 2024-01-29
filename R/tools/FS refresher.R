required <- c("rvest", "tidyverse", "magrittr", "stringi",
              "foreach", "doParallel", "parallel")
lapply(required, require, character.only = TRUE)
options(readr.show_col_types = FALSE)

### reference a dataframe with a fid column
fid_list <- review.age.old

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

foreach(file = fid_list$fid, .combine = 'c', .errorhandling = 'remove') %dopar% {
  if (!file.exists(file)) {
    download.file(
      url = paste0("https://ancestors.familysearch.org/service/tree/tree-data/published/persons/", file),
      destfile = paste0("~/GitHub/Family-Tree/records/", file, ".json"),
      quiet = FALSE)
  }
}

stopCluster(cl)

rm(required)
