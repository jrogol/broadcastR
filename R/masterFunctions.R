#' Title
#'
#' @param teamName 
#' @param rosterURL 
#' @param statsURL 
#' @param source 
#' @param sport 
#'
#' @return
#' @export
#'
#' @examples

getData <- function(teamName,
                    rosterURL,
                    statURL,
                    source,
                    sport,
                    output.dir = NULL,
                    exportRoster = F,
                    ...) {
  source <- match.arg(source,
                      c("sidearm", "wmt", "neulion", "presto", "liberty"))
  sport <- match.arg(sport,
                     c("baseball", "softball"))
  
  roster_df <- getRoster(teamName, rosterURL, source, sport)
  
  stats_df <- getStats(teamName, statURL, roster_df, source = source)
  
  if (source == "d1") {
    joined <- stats_df
} else {
  joined <- joinStatCrew(roster_df, stats_df)
  
}
  
  stats_out <- formatStats(joined, col.names)
  
  if (exportRoster) {
    exportRoster(teamName, roster_df, output.dir = output.dir)
  }
  
  exportStats(teamName, stats_out, output.dir = output.dir)
  }


#' Title
#'
#' @param teamName
#' @param url
#' @param source
#' @param sport
#'
#' @return
#' @export
#'
#' @examples
getRoster <- function(teamName, url, source, sport) {

  source <- match.arg(source,
                      c("sidearm","wmt","liberty"))
  sport <- match.arg(sport,
                     c("baseball","softball"))
  # Error Handling for url

  roster <- switch(source,
    sidearm = getRoster_Sidearm(teamName,url,sport),
    wmt = getRoster_WMT(teamName,url,sport),
    liberty = getRoster_Liberty(teamName,url,sport)
  )
  
  # Cleaning Steps
  roster <- separateName(roster)
  
  roster <- joinStates(roster)
  
  if(any(grepl("^Y(ea)?r\\.?",names(roster)))){
    names(roster)[grepl("^Y(ea)?r\\.?",names(roster))] <- "Class"
  }
  
  roster <- encodeYear(roster)
  
  roster <- purrr::possibly(pitcherThrows, otherwise = roster)(roster)
  
  roster <- dplyr::mutate(roster,Name = stringr::str_squish(stringr::str_remove_all(Name,"\\d+")
))
  
  if(any(names(roster) == 'Weight') & is.character(roster$Weight)){
    roster <- dplyr::mutate(roster, Weight = readr::parse_number(Weight))
  }
  
  return(roster)
}



    #' Title
#'
#' @param team 
#' @param statURL 
#' @param roster_df 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
getStats <- function(team, statURL, roster_df, col.names = format ,...) {
  source <- fetchStatSource(statURL)
  
  stats <- switch(source,
         sidearm = getStats_Sidearm(statURL,...),
         statcrew = getStats_StatCrew(statURL),
         pdf = getStats_PDF(statURL),
         d1 = getStats_D1(statURL, roster_df))
  
  return(stats)
}




#' Title
#'
#' @param team 
#' @param stats_df 
#' @param na.str 
#' @param output.dir 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
exportStats <- function(team, stats_df, na.str = "", output.dir = NULL, ...){
  if (is.null(output.dir)){
    readr::write_csv(stats_df, sprintf("%s_Stats.csv", team), na = na.str, ...)
  } else {
    readr::write_csv(stats_df, sprintf("%s/%s_Stats.csv", output.dir, team), na = na.str, ...)
  }
}


#' Title
#'
#' @param team 
#' @param roster_df 
#' @param na.str 
#' @param output.dir 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
exportRoster <- function(team, roster_df, na.str = "", output.dir = NULL, ...){
  if (is.null(output.dir)){
    readr::write_csv(roster_df, sprintf("%s_Roster.csv", team), na = na.str, ...)
  } else {
    readr::write_csv(roster_df, sprintf("%s/%s_Roster.csv", output.dir, team), na = na.str, ...)
  }
}