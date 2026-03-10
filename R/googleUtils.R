#' Standardize roster columns for export
#'
#' Helper function to build a canonical roster-column template and binds incoming roster data to it,
#' keeping only supported fields and enforcing column order.
#'
#' @param df A data frame or tibble of roster data. Extra columns are ignored.
#'   Missing supported columns are added as `NA`.
#'
#' @returns A tibble with roster variables in this order:
#'   `Name`, `Last`, `First`, `Number`, `Position`, `Bats`, `Throws`,
#'   `Hometown`, `State`, `Height`, `Weight`, `Class`.
#'
output_rosterVariables <- function(df){
  rosterBase <- dplyr::tribble(~Name,
                        ~Last,
                        ~First,
                        ~Number,
                        ~Position,
                        ~Bats,
                        ~Throws,
                        ~Hometown,
                        ~State,
                        ~Height,
                        ~Weight,
                        ~Class)
  out <- dplyr::bind_rows(rosterBase,
                   dplyr::select(df,
                          dplyr::any_of(names(rosterBase))))
  
  return(out)
}
#' Check for an existing roster in a Google Sheet and fetch/write if missing
#'
#' Attempts to read a roster for a given team from a Google Sheet. If the sheet read succeeds and contains more than one row (and overwrite is FALSE), the existing roster is returned. Otherwise the roster is fetched from the provided source via getRoster(), processed with output_rosterVariables(), and written back to the Google Sheet using googlesheets4::sheet_write().
#'
#' @param teamName character. Name of the team / the sheet name within the Google Sheet.
#' @param url character. URL (or file path) used by getRoster() to fetch roster data.
#' @param source character. Source identifier passed to getRoster() to control how the roster is fetched. Currently supported for "sidearm", "wmt", "presto", "liberty", "espn".
#' @param sport character. One of "baseball" or "softball"; passed to getRoster() to select sport-specific parsing.
#' @param sheetid character. Google Sheet id or value acceptable to googlesheets4::read_sheet and googlesheets4::sheet_write.
#' @param overwrite logical(1). If TRUE, force fetching the roster and overwriting the sheet even if an existing roster is present. Default FALSE.
#'
#' @return A data.frame (tibble) containing the roster for the requested team after any processing by output_rosterVariables().
#'
#' @details
#' This function first attempts a safe read of the sheet (using purrr::safely around googlesheets4::read_sheet).
#' If the read returns an error or the existing sheet is empty (or overwrite is TRUE), it will:
#' - call getRoster(teamName, url, source, sport) to obtain raw roster data,
#' - call output_rosterVariables() to produce a standardized roster data.frame,
#' - write the resulting roster to the specified sheet with googlesheets4::sheet_write().
#'
#' @seealso getRoster, output_rosterVariables, googlesheets4::read_sheet, googlesheets4::sheet_write
#'
#' @examples
#' \dontrun{
#' # Attempt to read "Team A" from Google Sheet and fetch if missing:
#' roster <- rosterCheck(
#'   teamName = "Team A",
#'   url = "https://example.com/roster.csv",
#'   source = "website",
#'   sport = "baseball",
#'   sheetid = "1A2b3C4d5E6fG7h8I9j0",
#'   overwrite = FALSE
#' )
#' }
#'
#' @export

rosterCheck <- function(teamName,
                        url,
                        source,
                        sport = c("baseball","softball"),
                        sheetid,
                        overwrite = F) {
  
  # Try Reading the existing
  existing <- purrr::safely(googlesheets4::read_sheet)(sheetid, teamName)
  
  # If data exists
  if (is.null(existing$error) &&
      nrow(existing$result > 1) & !overwrite) {
    roster_df <- existing$result
  } else {
    # Fetch the roster if not
    roster <- getRoster(teamName, url, source, sport)
    roster_df <- output_rosterVariables(roster)
    
    googlesheets4::sheet_write(roster_df,ss = sheetid,
                sheet = teamName)
  }
  
  return(roster_df)
}