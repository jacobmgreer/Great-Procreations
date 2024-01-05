required <- c("rvest", "tidyverse", "magrittr", "stringi",
              "foreach", "doParallel", "parallel")
lapply(required, require, character.only = TRUE)
options(readr.show_col_types = FALSE)

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

for (s in 3:18) {

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
  fs.person <- NULL

  for (i in files) {
    person <- jsonlite::fromJSON(i)$person

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
  write.csv(fs.person, paste0("input/people/people-", s, ".csv"), row.names = FALSE)
}

stopCluster(cl)

rm(required, cl, i, s, new.parent, layer, files, person, fs.person)
