required <- c("rvest", "tidyverse", "magrittr", "stringi", "scales", "httr")
lapply(required, require, character.only = TRUE)
options(readr.show_col_types = FALSE)
options(timeout = 5*60)

fs_en_wikipedia <- # https://w.wiki/A7sd
  jsonlite::fromJSON("https://query.wikidata.org/sparql?query=SELECT%20%3Fitem%20%3Fvalue%20%3Farticle%0AWHERE%20%7B%0A%20%20%3Fitem%20wdt%3AP2889%20%3Fvalue.%0A%20%20OPTIONAL%20%7B%0A%20%20%20%20%3Farticle%20schema%3Aabout%20%3Fitem%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20schema%3AisPartOf%20%3Chttps%3A%2F%2Fen.wikipedia.org%2F%3E%20.%7D%0A%7D&format=json")$results$bindings %>%
  reframe(
    QID = basename(item$value),
    fs = value$value,
    article = article$value
  ) %T>%
  write.csv(., "data/wd_fs_en_article.csv", row.names = FALSE)
