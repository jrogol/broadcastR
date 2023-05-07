getRoster_Sidearm <- function(teamName, url, sport) {
  roster <- fetchRoster_Sidearm(teamName, url, sport)
  
  rosterClean <- cleanRoster_Sidearm(roster)
  
  return(rosterClean)
}


fetchPlayerNodes_Sidearm <- function(url) {
  page <- xml2::read_html(url)
  
  players <- rvest::html_nodes(page, css = "li.sidearm-roster-player")
  
  return(players)
}



# These should be moved into their own, smaller functions....
fetchPlayer_Sidearm <- function(node) {
  position <- node %>% 
    rvest::html_node(css = "span.text-bold,
                   span.sidearm-roster-list-item-position") %>%
    rvest::html_text(trim = T)
  
  number <- node %>% 
    rvest::html_node(css = "span.sidearm-roster-player-jersey-number,
                   div.sidearm-roster-list-item-photo-number") %>%
    rvest::html_text(trim = T)
  
  name <- node %>% 
    rvest::html_elements(css = "p,h3,
                   div.sidearm-roster-player-name") %>%
    rvest::html_text(trim = T) %>% 
    (function(txt){
      txt[which.max(stringr::str_length(txt))]
    })
  
  # Pitt uses custom1 - not custom2 - the syntax below enables fuzzy matching
  bats <- node %>% 
    rvest::html_node(css = "span[class^=sidearm-roster-player-custom],
                     span[class^=sidearm-roster-list-item-custom]") %>%
    rvest::html_text(trim = T)
  
  hometown <- node %>% 
    rvest::html_node(css = "span.sidearm-roster-player-hometown,
                   div.sidearm-roster-list-item-hometown") %>%
    rvest::html_text(trim = T)
  
  height <- node %>% 
    rvest::html_node(css = "span.sidearm-roster-player-height,
                   span.sidearm-roster-list-item-height") %>%
    rvest::html_text(trim = T)
  
  weight <- node %>% 
    rvest::html_node(css = "span.sidearm-roster-player-weight,
                   span.sidearm-roster-list-item-weight") %>%
    rvest::html_text(trim = T)
  
  year <- node %>% 
    rvest::html_nodes(css = "span.sidearm-roster-player-academic-year:not(.hide-on-large),
                           span.sidearm-roster-list-item-year") %>%
    rvest::html_text(trim = T)
  
  
  # Add proper formatting for columns
  player <- dplyr::bind_cols(Number = readr::parse_number(number),
                             Name = name,
                             Position = position,
                             bats = bats,
                             hometown = hometown,
                             Height = height,
                             Weight = weight,
                             Year = year)
  
  return(player)
}



fetchRoster_Sidearm <- function(teamName, url, sport){
  players <- fetchPlayerNodes_Sidearm(url)
  
  if(length(players) > 0){
    roster <- purrr::map_df(players, fetchPlayer_Sidearm)
    return(roster)
  }
  
  message("Loading Page Found, Starting Selenium")
  
  serv <- startChromeServer()
  
  browser <- startSelenium(serv,headless = T)
  
  browser$open(silent = T)
  
  browser$navigate(url)
  
  Sys.sleep(2)
  
  # Note: this is the same logic used in fetch_SeleniumStats()
  page <- browser$getPageSource()[[1]]
  
  parsed <- rvest::read_html(page)
  tab <- rvest::html_table(parsed)[[1]]
  
  browser$close()
  
  serv$stop()
  
  names(tab)[grepl("^(No\\.?)",names(tab),ignore.case = T)] <- "Number"
  names(tab)[grepl("^Pos",names(tab),ignore.case = T)] <- "Position"
  names(tab)[grepl("B/T",names(tab),ignore.case = T)] <- "bats"
  names(tab)[grepl("^H(eigh)?t",names(tab),ignore.case = T)] <- "Height"
  names(tab)[grepl("^W(eigh)?t",names(tab),ignore.case = T)] <- "Weight"
  names(tab)[grepl("^(Cl(ass)?|Y(ea)?r)",names(tab),ignore.case = T)] <- "Year"
  names(tab)[grepl("Hometown",names(tab),ignore.case = T)] <- "hometown"
  
  if(!any(names(tab) == "Weight")) tab$Weight <- NA_character_
  
  roster <-dplyr::mutate(tab,
                          dplyr::across(hometown,
                                        ~stringr::str_extract(.,"^.+(?=\\s+/)"))) %>% 
    dplyr::select(dplyr::any_of(c("Number","Name",
                                  "Position",
                                  "bats",
                                  "hometown",
                                  "Height",
                                  "Weight",
                                  "Year")))
  
  
  return(roster)
}






cleanRoster_Sidearm <- function(rosterTable){
  # Split bats into bats/throws, hometown into hometown/state, remove lbs from weight.
  
  player_df <- rosterTable %>% 
    tidyr::separate(bats,
                    into = c("Bats","Throws"),
                    sep = "/") %>% 
    tidyr::separate(hometown,
                    into = c("Hometown","State"),
                    sep = ", +",
                    extra = "merge") %>% 
    dplyr::mutate(dplyr::across(where(is.character) & dplyr::contains("Weight"),
                                readr::parse_number),
                  Position = stringr::str_extract(Position,"[A-Z123/]+$"))
  
  return(player_df)
}
