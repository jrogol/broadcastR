#' Parse schedule details
#'
#' @param sport name of sport
#' @param year numeric, four-digit year
#'
#' @return vector with team name, game location, game date, and opponent url
#'
#' @examples
schedule_details <- function(game_node) {
  # Team name
  opp <- game_node %>% 
    rvest::html_node(".schedule__item-state") %>% 
    rvest::html_text(trim = T)
  
  # Team website - not present in 21-22 site
  link <- game_node %>%
    rvest::html_node(".sidearm-schedule-game-opponent-name") %>%
    rvest::html_node("a") %>%
    rvest::html_attr("href")
  
  # Game Date
  date <- game_node %>%
    rvest::html_node("time") %>% 
    rvest::html_text() %>% 
    as.Date("%a %b %e") %>% 
    format()
  
  
  # Game Location
  loc <- game_node %>%
    rvest::html_node(".schedule__item-place") %>%
    rvest::html_node("span:not([class])") %>% 
    rvest::html_text(trim = T) %>% 
    stringr::str_extract("(?<=(\\r[[:space:]])|^).+$") %>% 
    stringr::str_trim()
  
  c(team = opp,
    location = loc,
    date = date,
    link = link)
}


#' Get schedule for a single sport
#'
#' @param team name of sport
#' @param year numeric, four-digit year
#'
#' @return data frame with four columns for the opponent, game location, game date, and opponent url
#' @export
#'
#' @examples
getSchedule <- function(team, year) {
  # Year must be numeric
  if(!is.character(year)) stop("year must be a string")
  # team must be baseball or softball
  team <- match.arg(tolower(team),
            c("baseball","softball"))
  
  baseurl <- "https://virginiasports.com/sports/%s/schedule/%s/"
  teamurl <- sprintf(baseurl,team, year)
  
  games <- xml2::read_html(teamurl) %>% 
    #pull in home games only!
    rvest::html_nodes("div.schedule__list-item.home")
  
  games %>% 
    purrr::map(schedule_details) %>% 
    purrr::reduce(dplyr::bind_rows) %>% 
    distinct() %>% 
    dplyr::bind_rows(c(team = sprintf("%s Virginia %s",year, stringr::str_to_title(team)),
                location = NA,
                date = NA,
                link = "http://www.virginiasports.com"))
}




