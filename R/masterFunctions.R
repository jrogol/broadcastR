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

  switch (source,
    "sidearm" = getSidearmRoster(teamName,url,sport),
    "wmt" = getWMTRoster(teamName,url,sport),
    "neulion" = getNeulionRoster(teamName,url,sport),
    "presto" = getPrestoRoster(teamName,url,sport),
    "liberty" = getLibertyRoster(teamName,url,sport)
  )
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
getStats <- function(teamName, url, source = c("sidearm","statcrew","statcrewPDF"), sport = "baseball"){
  source <- match.arg(source)
  sport <- match.arg(source)
  # Error Handling for url

  switch (source,
          "statcrew" = getStatCrewStats(teamName,url,sport),
          "sidearm" = getSidearmStats(teamName,url,sport),
          "statcrewPDF" = getStatCrewPDF(teamName,url,sport)
  )
}
