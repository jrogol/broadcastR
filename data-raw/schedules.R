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
  write.csv("data/softballSchedule.csv")

baseball <- get_schedule("baseball",2020)

baseball %>% 
  mutate(team = case_when(
    stringr::str_detect(team, "NCAA|ACC") ~ "Postseason",
    stringr::str_detect(team, "Game \\d+") ~ "Virginia",
    TRUE ~ team)) %>% 
  filter(team != "Postseason") %>% 
  write.csv("data/baseballSchedule.csv")
