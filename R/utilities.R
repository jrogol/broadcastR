joinStates <- function(roster_df, from = "AP", to = "USPS"){
  
  roster <-  dplyr::left_join(roster_df,
                              dplyr::select(states,  
                                            from,
                                            to),
                              by = c("State" = from)) %>% 
    dplyr::select(-State) %>%
    dplyr::rename("State" = to)
  
  return(roster)
  
}


# The regEx looks for a space, followed by a (possibly) hypenated
# word and an end line, e.g. the last word.
separateName <- function(roster_df, sep = " +(?=[\\w-]+$)",...){
  roster <- tidyr::separate(roster_df,Name, 
                            into= c("First", "Last"),
                            sep = sep,
                            extra = "merge",
                            merge = "right",
                            remove = FALSE,
                            ...)
  
  return(roster)
}



write_roster <- function(df,team,sport, ...){
  # Add error handling
  readr::write_csv(df, sprintf("%s_%s.csv", team, sport), col_names=F)
}



fetchStatSource <- function(statURL){
  if (grepl("cumestats\\.aspx",statURL)) {
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
    `is.na<-` (setdiff(col, names(.))) %>% 
    dplyr::select(col)
  
  return(output)
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