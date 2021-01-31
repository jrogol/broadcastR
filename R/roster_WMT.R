

#' Title
#'
#' @param teamName 
#' @param url 
#' @param sport 
#'
#' @return
#'
#' @examples
fetchRoster_WMT <- function(teamName, url, sport){
  page <- xml2::read_html(url)
  
  players <- rvest::html_node(page,
                              "table#roster_sort, table[class*='roster'], table#players-table")  
  roster <- rvest::html_table(players)
  
  return(roster)
}


#' Title
#'
#' @param roster_df 
#'
#' @return
#'
#' @examples
cleanRoster_WMT <- function(roster_df){
  # Newer WMT sites may omit Batting/Throwing
  if ("B/T" %in% names(roster_df)) {
    roster_df <- tidyr::separate(roster_df,
                                 "B/T",
                                 into = c("Bats", "Throws"),
                                 sep = "/")
  } else {
    roster_df[["Throws"]] <- NA_character_
    roster_df[["Bats"]] <- NA_character_
  }
  
  
  # Split bats into bats/throws, hometown into hometown/state, remove lbs from weight.
  
  player_df <- roster_df %>% 
    dplyr::rename_at(dplyr::vars(dplyr::starts_with("Pos")),~"Position") %>% 
    tidyr::separate("Hometown",
                    into = c("Hometown","State"),
                    sep = ", +") %>% 
    dplyr::rename(Year = Class) %>% 
    # dplyr::rename(Height = "Ht.",
                  # Weight = "Wt.") %>% 
    dplyr::mutate(Position = abbreviatePosition(Position),
                  Position1 = stringr::str_extract(Position,"[A-z/123]+$"),
                  Height = stringr::str_replace(Height, "(\\d)-(\\d+)","\\1'\\2\\\"")) %>%
    dplyr::select(-Position)
  
  return(dplyr::as_tibble(player_df))

}


getRoster_WMT <- function(teamName, url, sport){
  
  roster <- fetchRoster_WMT(teamName, url, sport)
  
  rosterClean <- cleanRoster_WMT(roster)
  
  return(rosterClean)
}
