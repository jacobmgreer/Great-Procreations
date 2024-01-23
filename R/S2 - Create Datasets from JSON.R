required <- c("rvest", "tidyverse", "magrittr", "stringi",
              "foreach", "doParallel", "parallel", "data.table")
lapply(required, require, character.only = TRUE)
options(readr.show_col_types = FALSE)

ascendancy <-
  list.files(path = "input/ascendancy",
             pattern = "*.csv",
             full.names = TRUE) %>%
  lapply(read_csv) %>%
  setNames(., list.files(path = "input/ascendancy", pattern = "*.csv")) %>%
  bind_rows(.id = "id") %>%
  select(gen=id, ascendancy, fid=parent) %>%
  mutate(gen = parse_number(gsub(".*-(.+)-.*", "\\1", gen)) + 1) %>%
  arrange(gen, ascendancy) %>%
  filter(nchar(fid) == 8)

generations <-
  ascendancy %>%
  group_by(gen) %>%
  reframe(
    identified = n_distinct(fid[!is.na(fid)])) %>%
  rowwise() %>%
  mutate(
    total = 2 ** (gen - 1),
    per.known = round(identified / total * 100, digits=2),
    missing = total - identified)

unique_people <-
  ascendancy %>%
  select(fid) %>%
  unique() %>%
  mutate(batch = ceiling(row_number()/5000)) %T>%
  write.csv(., "input/unique_people.csv", row.names = FALSE)

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

unlink("input/life-events/*")
unlink("input/lifespan-locations/*")
unlink("input/people/*")

foreach(b = 1:max(unique_people$batch), .combine = 'c') %dopar% {

  required <- c("rvest", "tidyverse", "stringi", "data.table")
  lapply(required, require, character.only = TRUE)

  unique_people <-
    read_csv("input/unique_people.csv") %>%
    filter(batch == b)

  fs.person <- NULL
  fs.events <- NULL
  fs.locations <- NULL

  for (i in unique_people$fid) {
    if (file.exists(paste0("~/GitHub/Family-Tree/records/", i, ".json"))) {
      person <- jsonlite::fromJSON(paste0("~/GitHub/Family-Tree/records/", i, ".json"))$person
      fs.person <-
        fs.person %>%
        bind_rows(
          data.frame(
            fid = person$identifier,
            name = person$name,
            gender = person$gender,
            lifespan = person$fullLifespan,
            age_at_death = ifelse(length(person$ageAtDeath) > 0, person$ageAtDeath, NA),
            birth = ifelse(length(person$birthDate) > 0, person$birthDate, NA),
            place_birth_lat = ifelse(length(person$birthPlace) > 0, as.character(person$birthPlace$geo$latitude), NA),
            place_birth_lon = ifelse(length(person$birthPlace) > 0, as.character(person$birthPlace$geo$longitude), NA),
            place_birth = ifelse(length(person$birthPlace) > 0, as.character(person$birthPlace$address), NA),
            death = ifelse(length(person$deathDate) > 0, person$deathDate, NA),
            place_death_lat = ifelse(length(person$deathPlace) > 0, as.character(person$deathPlace$geo$latitude), NA),
            place_death_lon = ifelse(length(person$deathPlace) > 0, as.character(person$deathPlace$geo$longitude), NA),
            place_death = ifelse(length(person$deathPlace) > 0, as.character(person$deathPlace$address), NA),
            count_memories = person$memories,
            count_stories = person$stories,
            count_photos = person$photos,
            count_sources = person$sources,
            count_parents = length(person$parent$identifier),
            count_siblings = ifelse(length(person$sibling$identifier) > 0, length(person$sibling$identifier) - 1, 0),
            count_spouses = ifelse(length(person$spouse$identifier) > 0, length(person$spouse$identifier), 0),
            count_children = ifelse(length(person$children$identifier) > 0, length(person$children$identifier), 0)))

      fs.locations <-
        fs.locations %>%
        bind_rows(
          data.frame(
            fid = person$identifier,
            place_birth_lat = ifelse(length(person$birthPlace) > 0, as.character(person$birthPlace$geo$latitude), NA),
            place_birth_lon = ifelse(length(person$birthPlace) > 0, as.character(person$birthPlace$geo$longitude), NA),
            place_birth = ifelse(length(person$birthPlace) > 0, as.character(person$birthPlace$address), NA),
            place_death_lat = ifelse(length(person$deathPlace) > 0, as.character(person$deathPlace$geo$latitude), NA),
            place_death_lon = ifelse(length(person$deathPlace) > 0, as.character(person$deathPlace$geo$longitude), NA),
            place_death = ifelse(length(person$deathPlace) > 0, as.character(person$deathPlace$address), NA)
          ))

      if(length(person$events) > 0) {
        fs.events <-
          fs.events %>%
          bind_rows(
            data.frame(fid = tools::file_path_sans_ext(basename(i)),
                       type = ifelse(length(person$events$name) > 0, as.character(person$events$name), NA),
                       year = ifelse(length(person$events$year) > 0, as.character(person$events$year), NA),
                       place = ifelse(length(person$events$place) > 0, as.character(person$events$place), NA)
            ))
        }


    }
  }

    write.csv(fs.person,
              paste0("input/people/people-", sprintf("%003d", b), ".csv"),
              row.names = FALSE)
    write.csv(fs.locations,
              paste0("input/lifespan-locations/locations-", sprintf("%003d", b), ".csv"),
              row.names = FALSE)
    write.csv(fs.events,
              paste0("input/life-events/events-", sprintf("%003d", b), ".csv"),
              row.names = FALSE)
}

stopCluster(cl)

### compile cuts for the D3 chart

# for (cut in 32:63) {
#   start <- stop <- cut
#   for (j in 2:30) {
#     start[j] <- start[j-1] * 2
#     stop[j] <- stop[j-1] * 2 + 1
#   }
#   ranges <- data.frame(start, stop) %T>%
#     write.csv(., paste0("data/ranges/slice-", str_pad(cut, 3, pad = "0"), ".csv"), row.names = FALSE)
#
#   ascendancy %>%
#     rowwise() %>%
#     filter(gen <= 30) %>%
#     filter(any(ascendancy >= ranges$start & ascendancy <= ranges$stop)) %>%
#     select(fid, ascendancy) %>%
#     write.csv(., paste0("data/cuts/slice-", str_pad(cut, 3, pad = "0"), ".csv"), row.names = FALSE)
#
#   rm(cut, start, stop, ranges, j)
# }
#
# ascendancy %>%
#   filter(gen %in% 3:18) %>%
#   select(ascendancy, fid) %T>%
#   write.csv("data/core-pedigree.csv", row.names = FALSE)

rm(required, cl, unique_people)
