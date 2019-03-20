getStats_D1 <- function(statsURL, player_df, ...){
  tables <- fetchD1_Stats(statsURL)
  
  clean <- cleanStats_D1(tables)
  
  joined <- joinD1(player_df,clean,...)
  
}

fetchD1_Stats <- function(url) {
  page <- xml2::read_html(url)
  
  batting <- page %>% 
    rvest::html_node("table#batting-stats") %>% 
    rvest::html_table()
  
  pitching <- page %>% 
    rvest::html_node("table#pitching-stats") %>% 
    rvest::html_table()
  
  tables_out <- list(batting = batting, pitching = pitching)
  
  return(tables_out)
}


# Doesn't in count SH, SF or GDP.
cleanBatting_D1 <- function(table) {
  batting <- dplyr::rename_all(table,toupper) %>% 
    dplyr::rename("HBP" = "HP") %>% 
    dplyr::select(PLAYER, base::intersect(battingStats,names(.)))
  
  return(batting)
}

cleanPitching_D1 <- function(table){
  pitching <- dplyr::rename_all(stats$pitching, toupper) %>% 
    dplyr::rename(Win = W,
                  Loss = L,
                  HBP = HP) %>% 
    # Take CG shutouts only, not combined ones.
    dplyr::mutate(SHO = stringr::str_extract(SHO,"^\\d+")) %>% 
    dplyr::select(PLAYER, base::intersect(pitchingStats,names(.))) %>%  #broadcastR:::battingStats
    dplyr::rename_at(dplyr::vars(-PLAYER),~paste0(.,"_PitchingSeason"))
  
  pitching <- dplyr::mutate(pitching, IP_PitchingSeason = format(IP_PitchingSeason, nsmall = 1))
  
  return(pitching)
}


cleanStats_D1 <- function(tableList) {
  batting <- cleanBatting_D1(tableList$batting)
  
  pitching <- cleanPitching_D1(tableList$pitching)
  
  return(list(batting = batting, pitching = pitching))
  
}

joinD1 <- function(player_df, tableList){
  output <- dplyr::rename_all(player_df,stringr::str_to_title) %>% 
    dplyr::rename(FirstName = First,
                  LastName = Last) %>% 
    dplyr::arrange(Number) %>%
    dplyr::left_join(tableList$batting, by  = c("Name" = "PLAYER")) %>% 
    dplyr::left_join(tableList$pitching, by = c("Name" = "PLAYER")) %>% 
    dplyr::arrange(as.numeric(Number))
  
  return(output)
}