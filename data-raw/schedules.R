## code to prepare baseball and softball schedule datasets goes here
library(dplyr)
library(broadcastR)


softball <- get_schedule("softball",2020)

softball %>% 
  mutate(team = case_when(
    stringr::str_detect(team, "NCAA|ACC") ~ "Postseason",
    stringr::str_detect(team, "Game \\d+") ~ "Virginia",
    TRUE ~ team)) %>% 
  filter(team != "Postseason") %>% 
  write.csv("data-raw/softballSchedule.csv")

baseball <- get_schedule("baseball",2020)

baseball %>% 
  mutate(team = case_when(
    stringr::str_detect(team, "NCAA|ACC") ~ "Postseason",
    stringr::str_detect(team, "Game \\d+") ~ "Virginia",
    TRUE ~ team)) %>% 
  filter(team != "Postseason") %>% 
  write.csv("data-raw/baseballSchedule.csv")

# Fill out with roster and stat links by hand.