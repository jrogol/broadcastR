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
  
  dplyr:::bind_rows(rosterBase,
            dplyr::select(df,
                   dplyr::any_of(names(rosterBase))))
  
}

# Re-initializes sheet to be Virginia, or adds the team if it doesn't exist.
insert_TeamSheet <- function(ss,team) {
  n <- sheet_names(ss)
  # Sheet1 should be removed in favour of virginia.
  if (length(n) == 1 & all(n == "Sheet1")) {
    sheet_rename(ss, n, "Virginia")
    n <- c(n,"Virginia")
  }
}



# amended the following to add type
write_TeamSheet <-
  function(team,
           sport = c("baseball", "softball"),
           type = c("Stats", "Roster"),
           data) {
    # ERROR HANDLING
    type <- stringr::str_to_title(type)
    type <- match.arg(type)
    
    sport <- tolower(sport)
    sport <- match.arg(sport)
    
    flag <- paste0(sport, type)
    
    ss <- switch(flag,
                 baseballRoster = "1CtmfBGAMPSeoSxImSJinBB3-8sXFj4B1nAZzPOW_VJY",
                 softballRoster = "1z1OcoShl16npgMti_eY35GgLz17py5JhjMW0RBUR3J4",
                 baseballStats = "1BxpXlNZGr0tfEDxy9RRG70nkjeEpazDdD5hkVSYMlUQ",
                 softballStats = "1djf6tBRGEuhwSfqhO8TQKRt0q094u-V2ykJFhAT_DV4")
    
    stopifnot(
      # "Invalid Sport" = !is.null(ss),
      "Invalid Data" = !is.data.frame(data[[1]])
    )
    
    insert_TeamSheet(ss, team)
    
    # if (team sheet exists and 
    
    sheet_write(data, ss, sheet = team)
  }
