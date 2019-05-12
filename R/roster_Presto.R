getRoster_Presto <- function(teamName, url, sport) {
  roster <- fetchRoster_Presto(teamName, url, sport)
  
  rosterClean <- cleanRoster_Presto(roster)
  
  return(rosterClean)
}


fetchRoster_Presto <- function(teamName, url, sport){
  page <- xml2::read_html(url)
  
  players <- page %>% 
    rvest::html_node("div.roster") %>% 
    rvest::html_node("table") %>% 
    rvest::html_table()
  
  return(players)
}


cleanRoster_Presto <- function(rosterTable){
  roster <- dplyr::mutate_all(rosterTable,~gsub("^.+?:","",.))
  
  roster <- dplyr::mutate_all(roster, ~gsub("\\s+"," ",.))
  roster <- tidyr::separate(roster, "B/T", into = c("Bats","Throws"),
                      sep = "/")  
  roster <- tidyr::extract(roster, "Hometown/Previous School",into = c("Hometown","State"),
                     "(^[\\w -]+), ([\\w, -\\.]+)(?= /)")
      # Format height appropriately.
  roster <- dplyr::mutate(roster, Cl. = stringr::str_replace(Cl.,"Fr.", "Freshman") %>% 
                    stringr::str_replace("So." , "Sophomore") %>% 
                    stringr::str_replace("Jr.", "Junior") %>% 
                    stringr::str_replace("Sr.", "Senior") %>% 
                    stringr::str_replace("Gr.", "Graduate Student") %>% 
                    stringr::str_replace("R-|Redshirt ?", "RS "),
                  Ht. = stringr::str_replace(Ht., "(\\d)-(\\d+)","\\1'\\2\\\""))
    
  roster <- roster %>% 
      dplyr::rename(Year = Cl.,
                    Height = Ht.,
                    Weight = Wt.,
                    Position1 = Pos.,
                    Number = No.)
  
  return(roster)
}