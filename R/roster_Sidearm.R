fetchPlayerNodes_Sidearm <- function(url) {
  page <- xml2::read_html(url)
  
  players <- rvest::html_nodes(page, css = "li.sidearm-roster-player")
  
  return(players)
}



# These should be moved into their own, smaller functions....
fetchPlayer_Sidearm <- function(node) {
  position <- node %>% 
    rvest::html_node(css = "span.text-bold") %>%
    rvest::html_text(trim = T)
  
  number <- node %>% 
    rvest::html_node(css = "span.sidearm-roster-player-jersey-number") %>%
    rvest::html_text(trim = T)
  
  name <- node %>% 
    rvest::html_node(css = "p") %>%
    rvest::html_text(trim = T)
  
  bats <- node %>% 
    rvest::html_node(css = "span.sidearm-roster-player-custom2") %>%
    rvest::html_text(trim = T)
  
  hometown <- node %>% 
    rvest::html_node(css = "span.sidearm-roster-player-hometown") %>%
    rvest::html_text(trim = T)
  
  height <- node %>% 
    rvest::html_node(css = "span.sidearm-roster-player-height") %>%
    rvest::html_text(trim = T)
  
  weight <- node %>% 
    rvest::html_node(css = "span.sidearm-roster-player-weight") %>%
    rvest::html_text(trim = T)
  
  year <- node %>% 
    rvest::html_nodes(css = "span.sidearm-roster-player-academic-year:not(.hide-on-large)") %>%
    rvest::html_text(trim = T)
  
  
  
  # Add proper formatting for columns
  player <- dplyr::bind_cols(Number = readr::parse_number(number),
                             Name = name,
                             Position1 = position,
                             bats = bats,
                             hometown = hometown,
                             Height = height,
                             Weight = weight,
                             Year = year)
  
  return(player)
}



fetchRoster_Sidearm <- function(teamName, url, sport){
  players <- fetchPlayerNodes_Sidearm(url)
  
  roster <- purrr::map_df(players, fetchPlayer_Sidearm)
  
  return(roster)
}


getRoster_Sidearm <- function(teamName, url, sport) {
  roster <- fetchRoster_Sidearm(teamName, url, sport)
  
  rosterClean <- cleanRoster_Sidearm(roster)
  
  return(rosterClean)
}



cleanRoster_Sidearm <- function(rosterTable){
  # Split bats into bats/throws, hometown into hometown/state, remove lbs from weight.
  
  player_df <- rosterTable %>% 
    tidyr::separate(bats,
                    into = c("Bats","Throws"),
                    sep = "/") %>% 
    tidyr::separate(hometown,
                    into = c("Hometown","State"),
                    sep = ", +") %>% 
    dplyr::mutate(Weight = readr::parse_number(Weight))
  
  return(player_df)
}
