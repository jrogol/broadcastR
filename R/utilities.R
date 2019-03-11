joinStates <- function(roster_df, from = "AP", to = "USPS"){
  
  roster <-  dplyr::left_join(roster_df,
                              dplyr::select(broadcastR:::states,  #broadcastR:::states
                                            from,
                                            to),
                              by = c("State" = from)) %>% 
    dplyr::select(-State) %>%
    dplyr:::rename("State" = to)
  
  return(roster)
  
}


# The regEx looks for a space, followed by a (possibly) hypenated
# word and an end line, e.g. the last word.
separateName <- function(roster_df, sep = " (?=[\\w-]+$)",...){
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