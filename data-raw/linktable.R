library(dplyr)


# Baseball ----------------------------------------------------------------

baseballTable <- readr::read_csv(here::here("data-raw/baseballSchedule-COMPLETE.csv")) %>% 
  mutate(team = case_when(
    stringr::str_detect(team, "NCAA|ACC") ~ "Postseason",
    stringr::str_detect(team, "Game \\d+") ~ "Virginia",
    TRUE ~ team),
    date = stringr::str_squish(date)
  ) %>% 
  mutate(roster = if_else(grepl("^http", roster) & !is.na(roster),roster, paste0(link,roster)) %>% 
           #remove double-slash, if present
           stringr::str_replace("(?<!:)\\/\\/","/"),
         stats = if_else(!grepl("^http", stats) & !is.na(stats), paste0(link, stats),
                           stats) %>% 
           stringr::str_replace("(?<!:)\\/\\/","/")) %>% 
  distinct(team, roster,stats, site_type)


usethis::use_data(baseballTable, overwrite = T)

# Softball ----------------------------------------------------------------

softballTable <- readr::read_csv(here::here("data-raw/softballSchedule-COMPLETE.csv")) %>% 
  mutate(team = case_when(
    stringr::str_detect(team, "NCAA|ACC") ~ "Postseason",
    stringr::str_detect(team, "Game \\d+") ~ "Virginia",
    TRUE ~ team),
    date = stringr::str_squish(date)
  ) %>% 
  mutate(roster = if_else(grepl("^http", roster) & !is.na(roster),roster, paste0(link,roster)) %>% 
           #remove double-slash, if present
           stringr::str_replace("(?<!:)\\/\\/","/"),
         stats = if_else(!grepl("^http", stats) & !is.na(stats), paste0(link, stats),
                         stats) %>% 
           stringr::str_replace("(?<!:)\\/\\/","/")) %>% 
  distinct(team, roster,stats, site_type)


usethis::use_data(softballTable, overwrite = T)
