required <- c("rvest", "tidyverse", "magrittr", "stringi", "data.table",
              "maps", "geosphere", "leaflet")
lapply(required, require, character.only = TRUE)
options(readr.show_col_types = FALSE)

migrations <-
  list.files(path = "input/lifespan-locations",
             pattern = "*.csv",
             full.names = TRUE) %>%
  lapply(read_csv) %>%
  rbindlist() %>%
  distinct(fid, .keep_all = TRUE) %>%
  filter(!is.na(place_birth_lat) & !is.na(place_death_lat)) %>%
  filter(!(place_birth_lat == place_death_lat & place_birth_lon == place_death_lon)) %>%
  mutate(
    distance = geosphere::distHaversine(
                cbind(place_birth_lon, place_birth_lat),
                cbind(place_death_lon, place_death_lat)),
    birth = validateCoords(place_birth_lon, place_birth_lat, mode = "point"),
    death = validateCoords(place_death_lon, place_death_lat, mode = "point")) %>%
  select(fid, distance, birth, death, everything()) %>%
  select(-c(birth, death)) %>%
  filter(distance > 100000)

# travel_map <- leaflet() %>%
#   addTiles() %>%
#   addMarkers(data = migrations, lng = ~birth$lng, lat = ~birth$lat, clusterOptions = markerClusterOptions())
#   #addMarkers(data = migrations, lng = ~death$lng, lat = ~death$lat, popup = "Point 2")

write.csv(migrations, "migrations.csv", row.names = FALSE)
