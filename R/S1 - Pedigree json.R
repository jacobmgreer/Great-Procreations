required <- c("rvest", "tidyverse", "magrittr", "jsonlite")
lapply(required, require, character.only = TRUE)
options(readr.show_col_types = FALSE)

# /* Color Theme Swatches in Hex */
#   .Wilderness-1-hex { color: #091326; }
#       .Wilderness-2-hex { color: #84AEBF; }
#           .Wilderness-3-hex { color: #F29966; }
#               .Wilderness-4-hex { color: #BF5D39; }
#                   .Wilderness-5-hex { color: #59221D; }

pedigree_pie <-
  jsonlite::fromJSON("input/pedigree-pie.json")$persons %>%
  reframe(
    id,
    gender = display$gender,
    ascendency = as.numeric(display$ascendancyNumber)
  ) %>%
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
      ascendency %in% 256:511 ~ 9)) %>%
  filter(!gen %in% c(1,2)) %>%
  select(gen, ascendency, everything()) %T>%
  write.csv("input/layers/base.csv", row.names = FALSE)

rm(required)
