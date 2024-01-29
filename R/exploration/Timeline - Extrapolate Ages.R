required <- c("rvest", "tidyverse", "magrittr", "stringi", "zoo")
lapply(required, require, character.only = TRUE)
options(readr.show_col_types = FALSE)

gen_ranges <-
  ascendancy %>%
  left_join(people %>% select(fid, birth, death, age_at_death), by="fid") %>%
  group_by(gen) %>%
  summarize(
    identified = n(),
    birth.median = round(median(as.numeric(birth), na.rm=TRUE), digits=0),
    death.median = round(median(as.numeric(death), na.rm=TRUE), digits=0),
    age.median = median(age_at_death, na.rm=TRUE),
    age.youngest = min(age_at_death, na.rm = TRUE),
    age.oldest = max(age_at_death, na.rm = TRUE),
    birth.earliest = min(as.numeric(birth), na.rm=TRUE),
    birth.latest = max(as.numeric(birth), na.rm=TRUE),
    death.earliest = min(as.numeric(death), na.rm=TRUE),
    death.latest = max(as.numeric(death), na.rm=TRUE),
  )

review.age.young <-
  people %>%
  filter(age_at_death < 15) %>%
  select(fid, name, birth, death, lifespan, age_at_death)

review.age.old <-
  people %>%
  filter(age_at_death > 120) %>%
  select(fid, name, birth, death, lifespan, age_at_death)


t_data <- na.approx(zoo(c(1, NA, NA, NA, 1500, 1930, NA, 1940, 1928)))

t_graph <- autoplot(t_data)
t_graph
