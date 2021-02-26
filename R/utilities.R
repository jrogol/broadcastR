# H/T: https://community.rstudio.com/t/a-confusing-result-from-tidyeval-can-you-believe-it/13025/7

joinStates <- function(roster_df, from = "state", to = "USPS"){
  
  roster <-  dplyr::left_join(roster_df,
                              dplyr::select(states,  
                                            from,
                                            to),
                              by = c("State" = from)) %>% 
    dplyr::mutate(!!rlang::enquo(to) := dplyr::if_else(is.na(!!rlang::sym(to)),State,!!rlang::sym(to))) %>% 
    dplyr::select(-State) %>%
    dplyr::rename("State" = to)
  
  return(roster)
  
}


# The regEx looks for a space, followed by a (possibly) hypenated
# word and an end line, e.g. the last word.
separateName <- function(roster_df,
                         sep = "([[:alnum:]-' \\.]+[^ ]) +([[:alnum:]-']+(?:,? [JSr\\.I]+)?)$",
                         ...){
  roster <- tidyr::extract(roster_df,Name,
                           into= c("First","Last"),
                           regex = sep,
                           remove = FALSE,
                           ...)
  
  return(roster)
}



write_roster <- function(df,team,sport, ...){
  # Add error handling
  readr::write_csv(df, sprintf("%s_%s.csv", team, sport), col_names=F)
}



fetchStatSource <- function(statURL){
  if (grepl("stats/season|cumestats(\\.aspx|/season)?|/sports/.+/stats(/\\d{0,4})?$",statURL)) {
    "sidearm"
  } else if (grepl("teamcume|teamstat", statURL)) {
    "statcrew"
  } else {
    "d1"
  }
}


formatStats <- function(stats_df, col.names){
  
  col <- gsub("_PlayerDemographics","",col.names)
  
  output <- stats_df %>% 
    dplyr::rename(Position= Position1) %>% 
    `is.na<-` (setdiff(col, names(.))) %>% 
    dplyr::select(col)
  
  return(output)
}


# Change years to numbers.
encodeYear <- function(df, yrCol = "Year"){
  dplyr::mutate(df,
                !!rlang::enquo(yrCol) := dplyr::case_when(
                  grepl("Fr",!!rlang::sym(yrCol)) ~ 1,
                  grepl("Soph",!!rlang::sym(yrCol)) ~ 2,
                  grepl("J.+r",!!rlang::sym(yrCol)) ~ 3,
                  TRUE ~ 4))
}

# Change Pitching Throwing to RHP/LHP
pitcherThrows <- function(df, posCol = "Position1", throwCol = "Throws") {
  if(throwCol %in% names(df)) {
    dplyr::mutate(df,!!rlang::enquo(throwCol) := dplyr::case_when(
      grepl("[RL]HP", !!rlang::sym(posCol)) ~ stringr::str_extract(!!rlang::sym(posCol),
                                                                   "[RL]HP"),
      grepl("P", !!rlang::sym(posCol)) ~ paste0(!!rlang::sym(throwCol), "HP"),
      TRUE ~ !!rlang::sym(throwCol)
    ))
  } else df
}

# Vectorized replacement of positions.
abbreviatePosition <- function(pos) {
  Position <- gsub("Catcher", "C", pos)
  Position <- gsub("(In|O)(ut)?field", "\\1F", Position)
  Position <- gsub("Utility", "UT", Position)
  Position <- gsub("([RL])\\w+[ \\-]hand(ed)? Pitcher",
                   "\\1HP",
                   Position,
                   ignore.case = T)
  
  toupper(Position)
}



# export_stats <- function(stats_df, na.str = "", output.dir = NULL, team, col.names = broadcastR:::xlnames, silent = F, ...){
#   
#   output <- df %>% 
#     `is.na<-` (setdiff(col.names, names(.))) %>% # broadcastR:::xlnames
#     select(col.names)
#   
#   if (is.null(output.dir)){
#     readr::write_csv(output, sprintf("%s_Stats.csv", team), na = na.str)
#   } else {
#     readr::write_csv(output, sprintf("%s/%s_Stats.csv", output.dir, team), na = na.str)
#   }
#   
#   if (!silent) return(output)
# }