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
    rvest::html_node(".sidearm-schedule-game-opponent-text") %>%
    rvest::html_node("a") %>%
    rvest::html_text() %>% 
    stringr::str_remove_all("\\*")
  
  # Team website
  link <- game_node %>%
    rvest::html_node(".sidearm-schedule-game-opponent-name") %>%
    rvest::html_node("a") %>%
    rvest::html_attr("href")
  
  # Game Date
  date <- game_node %>%
    rvest::html_node(".sidearm-schedule-game-opponent-date") %>%
    rvest::html_text("span") %>%
    stringr::str_squish() %>% 
    stringr::str_extract(".+?(?=[[:space:]]\\()")
  
  # Game Location
  loc <- game_node %>%
    rvest::html_node(".sidearm-schedule-game-location") %>%
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
  if(!is.numeric(year)) stop("year must be numeric.")
  # team must be baseball or softball
  team <- match.arg(tolower(team),
            c("baseball","softball"))
  
  baseurl <- "https://virginiasports.com/sports/%s/schedule/%i"
  teamurl <- sprintf(baseurl,team, year)
  
  games <- xml2::read_html(teamurl) %>% 
    rvest::html_nodes("div.sidearm-schedule-game-row")
  
  games %>% 
    purrr::map(schedule_details) %>% 
    purrr::reduce(bind_rows) %>% 
    distinct() %>% 
    bind_rows(c(team = sprintf("%i Virginia %s",year, stringr::str_to_title(team)),
                location = NA,
                date = NA,
                link = "http://www.virginiasports.com"))
}




