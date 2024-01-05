required <- c("rvest", "tidyverse", "magrittr", "stringi")
lapply(required, require, character.only = TRUE)
options(readr.show_col_types = FALSE)

read_csv("input/base.csv") %>%
  filter(gen == 3) %>%
  select(gen, ascendency, id, gender) %>%
  write.csv("input/layers/layer-3.csv", row.names = FALSE)

for (i in 4:13) {
  layer <-
    read_csv(paste0("input/layers/layer-", i - 1, ".csv")) %>%
    select(ascendency, id) %>%
    left_join(read_csv(paste0("input/parents/rel-", i - 1, ".csv")),
      by=c("id" = "fid"),
      relationship = "many-to-many") %>%
    filter(!is.na(parent)) %>%
    reframe(
      ascendency = ifelse(
        gender == "Female", (ascendency * 2) + 1, (ascendency * 2)),
      id = parent,
      gender = gender,
      gen = i
    ) %T>%
    write.csv(., paste0("input/layers/layer-", i, ".csv"), row.names = FALSE)
}

pedigree_pie <-
  list.files(path = "input/layers",
           pattern = "*.csv",
           full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows()

relations <-
  list.files(path = "input/parents",
             pattern = "*.csv",
             full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows()

dupes.review <-
  pedigree_pie %>%
  filter(!is.na(id)) %>%
  dplyr::count(id) %>%
  filter(n > 1) %>%
  arrange(desc(n)) %>%
  left_join(pedigree_pie, by=join_by(id))

pedigree_pie <- pedigree_pie %>%
  complete(ascendency = seq(4, 8191, by=1)) %>%
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
      ascendency %in% 256:511 ~ 9,
      ascendency %in% 512:1023 ~ 10,
      ascendency %in% 1024:2047 ~ 11,
      ascendency %in% 2048:4095 ~ 12,
      ascendency %in% 4096:8191 ~ 13)) %>%
  group_by(gen) %>%
    mutate(sequence = ave(gen, FUN = seq_along)) %>%
  ungroup() %>%
  mutate(
    gen_count = 2^(gen-1),
    half = ifelse(between(sequence / gen_count, 0, .5), "Paternal", "Maternal"),
    quartile = case_when(
      between(sequence / gen_count, 0, 1/4) ~ "Paternal Grandfather",
      between(sequence / gen_count, 1/4, 1/2) ~ "Paternal Grandmother",
      between(sequence / gen_count, 1/2, 3/4) ~ "Maternal Grandfather",
      between(sequence / gen_count, 3/4, 1) ~ "Maternal Grandmother"
    ),
    color = case_when(
      ## Paternal Grandfather, "Spring"
      quartile == "Paternal Grandfather" & gender == "Male" ~ "#B24C6D",
      quartile == "Paternal Grandfather" & gender == "Female" ~ "#FFC1D5",
      ## Paternal Grandmother, "Summer"
      quartile == "Paternal Grandmother" & gender == "Male" ~ "#07B2D9",
      quartile == "Paternal Grandmother" & gender == "Female" ~ "#88DFF2",
      ## Maternal Grandfather, "Fall"
      quartile == "Maternal Grandfather" & gender == "Male" ~ "#738B4F",
      quartile == "Maternal Grandfather" & gender == "Female" ~ "#A6D86F",
      ## Maternal Grandmother, "Winter
      quartile == "Maternal Grandmother" & gender == "Male" ~ "#8950A1",
      quartile == "Maternal Grandmother" & gender == "Female" ~ "#C881CF",
      .default = "#EEEEEE"
    )) %T>%
  write.csv(., "pedigree-pie-extended.csv", row.names = FALSE)

generations <-
  pedigree_pie %>%
  group_by(gen) %>%
  reframe(
    identified = n_distinct(id[!is.na(id)]),
    total = n(),
    per.known = round(identified / total * 100, digits=2),
    missing = total - identified
  )




# /* Color Theme Swatches in Hex */
#   .spring-wedding-1-hex { color: #B24C6D; }
#       .spring-wedding-2-hex { color: #FFC1D5; }
#           .spring-wedding-3-hex { color: #FF86AE; }
#               .spring-wedding-4-hex { color: #76B27C; }
#                   .spring-wedding-5-hex { color: #D6FFDA; }

# /* Color Theme Swatches in Hex */
#   .Transparent-blue-clear-water-surface-texture-with-ripples,-splashes-and-bubbles.-Abstract-summer-banner-background-Water-waves-in-sunlight-with-copy-space-Cosmetic-moisturizer-micellar-toner-emulsion-1-hex { color: #07B2D9; }
#       .Transparent-blue-clear-water-surface-texture-with-ripples,-splashes-and-bubbles.-Abstract-summer-banner-background-Water-waves-in-sunlight-with-copy-space-Cosmetic-moisturizer-micellar-toner-emulsion-2-hex { color: #88DFF2; }
#           .Transparent-blue-clear-water-surface-texture-with-ripples,-splashes-and-bubbles.-Abstract-summer-banner-background-Water-waves-in-sunlight-with-copy-space-Cosmetic-moisturizer-micellar-toner-emulsion-3-hex { color: #04C4D9; }
#               .Transparent-blue-clear-water-surface-texture-with-ripples,-splashes-and-bubbles.-Abstract-summer-banner-background-Water-waves-in-sunlight-with-copy-space-Cosmetic-moisturizer-micellar-toner-emulsion-4-hex { color: #50E2F2; }
#                   .Transparent-blue-clear-water-surface-texture-with-ripples,-splashes-and-bubbles.-Abstract-summer-banner-background-Water-waves-in-sunlight-with-copy-space-Cosmetic-moisturizer-micellar-toner-emulsion-5-hex { color: #04D9D9; }

# /* Color Theme Swatches in Hex */
#   .Fall-3-1-hex { color: #A6D86F; }
#       .Fall-3-2-hex { color: #738B4F; }
#           .Fall-3-3-hex { color: #795F78; }
#               .Fall-3-4-hex { color: #878068; }
#                   .Fall-3-5-hex { color: #838B7C; }

# /* Color Theme Swatches in Hex */
#   .true-winter-1-hex { color: #C881CF; }
#       .true-winter-2-hex { color: #8950A1; }
#           .true-winter-3-hex { color: #5E3579; }
#               .true-winter-4-hex { color: #A157A4; }
#                   .true-winter-5-hex { color: #442A59; }


rm(required, i)
