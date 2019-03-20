# reads roster into a table
fetchRoster_NL <- function(url){
  page <- xml2::read_html(url)
  
  table <- rvest::html_node(page,"#roster-list-table")
  
  table <- rvest::html_table(table, trim = TRUE)
  
  dplyr::as_tibble(table)
  
}

cleanRoster_NL <- function(rosterTable) {
  player_df <- rosterTable %>% 
    dplyr::rename(Number = No.,
                  Position1 = Pos.,
                  Height = Ht.,
                  Weight = Wt.,
                  Year = Yr.) %>% 
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("home")),
                     ~stringr::str_remove_all(., " \\(.*")) %>% 
    tidyr::separate(`B/T`, into = c("Bats","Throws"), sep = "/") %>% 
    tidyr::separate(names(roster)[grepl("home",names(roster), ignore.case = T)],
                    into = c("Hometown","State"),
                    sep = ", +") %>% 
    # Extract the last text after all the newlines and tabs
    dplyr::mutate_all(stringr::str_extract,"[\\w -\\.,\\d]+$") %>% 
    dplyr::mutate(Year = dplyr::case_when(Year == "Fr." ~ "Freshman",
                                          Year == "So." ~ "Sophomore",
                                          Year == "Jr." ~ "Junior",
                                          Year == "Sr." ~ "Senior",
                                          Year == "Gr." ~ "Graduate Student",
                                          TRUE ~ Year),
                  Height = stringr::str_replace(Height, "(\\d)-(\\d+)","\\1'\\2\\\""))
  
  return(player_df)
}


getRoster_NL <- function(teamName, url, sport){
  roster <- fetchRoster_NL(url)
  
  rosterClean <- cleanRoster_NL(roster)
  
  return(rosterClean)
}