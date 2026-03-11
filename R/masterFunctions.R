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

getData <- function(teamName,rosterURL,statURL, source, sport, 
                    output.dir = NULL,
                    exportRoster = F, ...) {
  source <- match.arg(source,
                      c("sidearm","wmt","presto","liberty"))
  sport <- match.arg(sport,
                     c("baseball","softball"))

  roster_df <- getRoster(teamName, rosterURL,source, sport)

  stats_df <- getStats(teamName,statURL,roster_df,source = source)

  if(exportRoster){
    exportRoster(teamName, roster_df, output.dir = output.dir)
  }
  
  exportStats(teamName, stats_df, output.dir = output.dir)
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
getRoster <- function(teamName, url, source, sport) {

  source <- match.arg(source, c("sidearm", "wmt", "presto", "liberty", "espn"))
  sport <- match.arg(sport,
                     c("baseball","softball"))
  # Error Handling for url

  roster <- switch(
    source,
    sidearm = getRoster_Sidearm(teamName, url, sport),
    wmt = getRoster_WMT(teamName, url, sport),
    presto = getRoster_Presto(teamName, url, sport),
    liberty = getRoster_Liberty(teamName, url, sport),
    espn = getRoster_ESPN(url)
  )
  
  # Cleaning Steps
  roster <- separateName(roster)
  
  if (source != 'espn') {
    roster <- joinStates(roster)
  }
  
  roster <- encodeYear(roster)
  
  roster <- pitcherThrows(roster)
  
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
  
  stats <- switch(
    source,
    sidearm = getStats_Sidearm(statURL, roster_df),
    statcrew = getStats_StatCrew(statURL, roster_df),
    pdf = getStats_PDF(statURL, roster_df),
    stop(sprintf("Unrecognized stats source for URL: %s", statURL))
  )
  
  return(formatStats(stats, col.names))
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