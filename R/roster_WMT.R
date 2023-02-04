

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
  
  players <- rvest::html_node(page, "table#roster_sort,table[class*='roster'],table[id*='player'],table[id*=person]")
  
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
cleanRoster_WMT <- function(roster_df) {
  # Split bats into bats/throws, hometown into hometown/state, remove lbs from weight.
  
  player_df <- purrr::possibly(tidyr::separate,
                               otherwise = roster_df)(roster_df,
                                                      `B/T`,
                                                      into = c("Bats", "Throws"),
                                                      sep = "[/-]") %>%
    dplyr::rename_all(stringr::str_to_title) %>%
    dplyr::rename_at(dplyr::vars(dplyr::starts_with("Pos", ignore.case = T)),  ~ "Position") %>%
    tidyr::separate("Hometown",
                    into = c("Hometown", "State"),
                    sep = ", +") %>%
    dplyr::rename(dplyr::any_of(c(Height = "Ht.",
                                  Weight = "Wt."))) %>%
    dplyr::mutate(
      Position = stringr::str_extract(Position, "[A-z/123]+$"),
      Height = stringr::str_replace(Height, "(\\d)-(\\d+)", "\\1'\\2\\\"")
    )
  
  return(dplyr::as_tibble(player_df))
  
}


getRoster_WMT <- function(teamName, url, sport){
  
  roster <- fetchRoster_WMT(teamName, url, sport)
  
  rosterClean <- cleanRoster_WMT(roster)
  
  return(rosterClean)
}
