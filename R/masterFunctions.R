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
getRoster <- function(teamName, url, source = c("sidearm","wmt","neulion","presto","liberty"), sport = "baseball") {

  source <- match.arg(source)
  sport <- match.arg(source)
  # Error Handling for url

  roster <- switch(source,
    sidearm = getRoster_Sidearm(teamName,url,sport),
    # wmt = getRoster_WMT(teamName,url,sport),
    neulion = getRoster_NL(teamName,url,sport)#,
    # presto = getRoster_Presto(teamName,url,sport),
    # liberty = getRoster_Liberty(teamName,url,sport)
  )
  
  roster <- separateName(roster)
  
  roster <- joinStates(roster)
  
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
         sidearm = getStats_Sidearm(statURL, roster_df),
         statcrew = getStats_StatCrew(statURL, roster_df),
         d1 = getStats_D1(statURL, roster_df))
  
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