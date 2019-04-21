getRoster_Liberty <- function(teamName, url, sport){
  roster <- fetchRoster_Liberty(teamName, url, sport)
  
  cleaned <- cleanRoster_Liberty(roster)
  
  return(cleaned)
}



fetchRoster_Liberty <- function(teamName, url, sport){
  page <- xml2::read_html(url)
  
  players <- rvest::html_node(page, "#roster") %>%
    rvest::html_children()
  
  roster <- purrr::map_df(players, broadcastR:::fetchPlayer_Liberty)
  
  return(roster)
}



fetchPlayer_Liberty <- function(node){
  name <- node %>%
    rvest::html_node("h3") %>%
    rvest::html_text()
  number <- node %>%
    rvest::html_node("span.jersey") %>%
    rvest::html_text()
  details <- node %>%
    rvest::html_nodes("span:not(span.jersey)") %>%
    rvest::html_text() %>% 
    purrr::reduce(paste, sep = " / ")
  
  player <- dplyr::bind_cols(Number = number,
                      Name = name,
                      details=details)
  return(player)
}



# Cleaning ----------------------------------------------------------------


cleanRoster_Liberty <- function(rosterTable){
  # extract details, make number numeric.
  rosterTable %>% 
    dplyr::mutate(Number = as.integer(Number),
                Bats = as.character(NA),
                Throws = as.character(NA),
                Position1 = stringr::str_extract(details,"^[[:alnum:]/]+"),
                Height = stringr::str_extract(details, "\\d' \\d{1,2}\""),
                Weight = stringr::str_extract(details, "\\d{2,3}(?= lbs)"),
                Year = stringr::str_extract(details, "(?<= / )[A-z- ]+(?= / )"),
                Hometown = stringr::str_extract(details, "(?<= / )[[:alnum:]- \\.]+(?=, |$)"),
                State = stringr::str_extract(details, "(?<=, ).*$")) %>% 
    dplyr::select(-details)
}
