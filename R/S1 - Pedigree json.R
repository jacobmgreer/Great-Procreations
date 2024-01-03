required <- c("rvest", "tidyverse", "magrittr", "jsonlite")
lapply(required, require, character.only = TRUE)
options(readr.show_col_types = FALSE)

pedigree_pie <-
  jsonlite::fromJSON("pedigree-pie.json")$persons %>%
  reframe(
    id,
    living,
    name = display$name,
    gender = display$gender,
    lifespan = display$lifespan,
    ascendency = as.numeric(display$ascendancyNumber)
  ) %>%
  complete(ascendency = seq(1, 511, by=1)) %>%
  mutate(
    gen = case_when(
      ascendency == 1 ~ 1,
      ascendency %in% 2:3 ~ 2,
      ascendency %in% 4:7 ~ 3,
      ascendency %in% 8:15 ~ 4,
      ascendency %in% 16:31 ~ 5,
      ascendency %in% 32:63 ~ 6,
      ascendency %in% 64:127 ~ 7,
      ascendency %in% 128:255 ~ 8,
      ascendency %in% 256:511 ~ 9),
    color = ifelse(is.na(id), "#eeeeee", "#333333")) %>%
  select(gen, ascendency, color, everything()) %T>%
  write.csv("pedigree-pie.csv", row.names = FALSE)
