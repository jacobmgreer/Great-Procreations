required <- c("rvest", "tidyverse", "magrittr", "stringi")
lapply(required, require, character.only = TRUE)
options(readr.show_col_types = FALSE)

rel.sex <- read_csv("../Family-Tree/reports/relationships-sex.csv")

# #### create ascendancy
ascendancy <-
  bind_rows(
    c(fid="GHVL-B4K", ascendancy=4),     # Paternal Grandfather
    c(fid="GHK6-8WT", ascendancy=5),     # Paternal Grandmother
    c(fid="L6L2-JJB", ascendancy=6),     # Maternal Grandfather
    c(fid="LNKH-8TG", ascendancy=7)) %>% # Maternal Grandmother
  left_join(rel.sex, by=c("fid" = "Child")) %>%
  mutate(
    ascendancy = as.numeric(ascendancy),
    gen = as.numeric(3))

for (i in 4:30) {
    ascendancy <-
      bind_rows(ascendancy,
                ascendancy %>%
                  filter(gen == i - 1) %>%
                  reframe(
                    fid = Father,
                    ascendancy = ascendancy * 2,
                    gen = i) %>%
                  filter(!is.na(fid)) %>%
                  left_join(rel.sex, by=c("fid" = "Child"))) %>%
      bind_rows(.,
                ascendancy %>%
                  filter(gen == i - 1) %>%
                  reframe(
                    fid = Mother,
                    ascendancy = (ascendancy * 2) + 1,
                    gen = i) %>%
                  filter(!is.na(fid)) %>%
                  left_join(rel.sex, by=c("fid" = "Child"))) %>%
      arrange(ascendancy)
}
rm(i)

ascendancy <- ascendancy %>%
  mutate(
    max = 2^(gen-1),
    slice = ascendancy - max,
    start = 360 * slice / max - (360/max),
    end = slice * (360/max))

for (cut in 32:63) {
  start <- stop <- cut
  for (j in 2:30) {
    start[j] <- start[j-1] * 2
    stop[j] <- stop[j-1] * 2 + 1
  }
  ranges <- data.frame(start, stop) %T>%
    write.csv(., paste0("data/ranges/slice-", str_pad(cut, 3, pad = "0"), ".csv"), row.names = FALSE)

  ascendancy %>%
    rowwise() %>%
    filter(any(ascendancy >= ranges$start & ascendancy <= ranges$stop)) %>%
    select(fid, ascendancy) %>%
    write.csv(., paste0("data/cuts/slice-", str_pad(cut, 3, pad = "0"), ".csv"), row.names = FALSE)

  rm(cut, start, stop, ranges, j)
}

cut_ascendancy <-
  list.files(path = "data/cuts",
             pattern = "*.csv",
             full.names = TRUE) %>%
  read_csv(., id="path") %>%
  filter(ascendancy >= 32) %>%
  mutate(cut = abs(parse_number(path)))

core_ascendancy <-
  ascendancy %>%
  filter(gen %in% 3:15) %>%
  select(ascendancy, fid) %T>%
  write.csv("data/core-pedigree.csv", row.names = FALSE)

shared.ancestors <-
  ascendancy %>%
  filter(duplicated(fid)) %>%
  count(fid) %>%
  arrange(desc(n)) %>%
  filter(n > 1)

generations <-
  ascendancy %>%
  group_by(gen) %>%
  reframe(
    identified = n_distinct(fid[!is.na(fid)]),
  ) %>%
  rowwise() %>%
  mutate(
    total = 2 ** (gen - 1),
    per.known = round(identified / total * 100, digits=2),
    missing = total - identified,
    r1 = list(range(1, ceiling(1/32 * total))),
    r2 = list(range(ceiling(1/32 * total), ceiling(2/32 * total)))
  )

rm(required, rel.sex)
