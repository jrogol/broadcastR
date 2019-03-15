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
getStats <- function(team, statURL, roster_df,...) {
  source <- fetchStatSource(statURL)
  
  switch(source,
         sidearm = getStats_Sidearm(statURL, roster_df),
         statcrew = getStats_StatCrew(statURL, roster_df),
         d1 = getStats_D1(statURL, roster_df))
}
