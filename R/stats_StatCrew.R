getStats_StatCrew <- function(statsURL, player_df, ...) {
  data <- fetchStatCrew(statsURL, ...)
  
  clean <- cleanStats_StatCrew(data)
  
  joined <- joinStatCrew(player_df,clean)
  
  return(joined)
}


fetchStatCrew <- function(url, header = T, fill = T) {
  # Error handling for url
  
  page <- xml2::read_html(url)
  
  tables <- rvest::html_nodes(page,"table[cellpadding]")
  
  tables <- rvest::html_table(tables, header = header, fill = fill)
  
  batting <- tables[1] %>% 
    unlist(recursive = F) %>% 
    dplyr::as_tibble()
  
  pitching <- tables[2] %>% 
    unlist(recursive = F) %>% 
    dplyr::as_tibble()
  
  tables_out <- list(batting = batting, pitching = pitching)
  
  return(tables_out)
  
}




# cleanStats_StatCrew ----

cleanStats_StatCrew <- function(tableList) {
  
  ## Add in name formatting - need conditionals!
  
  if(sum(grepl("([A-Z -']+), ([[:alpha:]\\. '-]+)",tableList$batting$Player)) > 0){
        tableList <- purrr::map(tableList,
          ~dplyr::mutate(.,Player = gsub("([A-Z -']+), ([[:alpha:]\\. '-]+)","\\2 \\1",Player))
            )
  }
  tableList <- purrr::map(tableList, 
               ~dplyr::mutate(Player = tolower(Player)))
  
  batting <- cleanBatting_StatCrew(tableList$batting)
  
  pitching <- cleanPitching_StatCrew(tableList$pitching)
  
  
  return(list(batting = batting, pitching = pitching))
  
}



# cleanBatting_StatCrew ----

cleanBatting_StatCrew <- function(table) {
  batting <- table %>% 
    tidyr::separate("sb-att",
                    into = c("sb","att"),
                    sep = "-",
                    convert = T)
  
  # Caught stealing can be easily calculated:
  
  batting <- batting %>% 
    dplyr::mutate(cs = att-sb)
  
  
  # select the appropriate stats 
  
  batting <- dplyr::rename_all(batting,toupper) %>% 
    dplyr::select(PLAYER, base::intersect(battingStats,names(.))) #broadcastR:::battingStats
  
  # lastly, we'll need to filter out incomplete rows, the Total, and Opposition
  # Stats
  batting <- dplyr::filter(batting, !is.na(AB),
                           !grepl("--|Totals?|Opponents?",PLAYER)) %>% 
    dplyr::rename_at(dplyr::vars(-PLAYER),~paste0(.,"_BattingSeason"))
  
  return(batting)
  
}





cleanPitching_StatCrew <- function(table) {
  pitching <- dplyr::rename_all(table, toupper) %>% 
    dplyr::filter(!is.na(ERA),
           !grepl("--|Totals?|Opponents?",PLAYER)) %>% 
    tidyr::separate("W-L",
                    into = c("Win", "Loss"),
                    sep = "-") %>% 
    tidyr::separate("APP-GS",
                    into = c("G","GS"),
                    sep = "-") %>% 
    # Take CG shutouts only, not combined ones.
    dplyr::mutate(SHO = stringr::str_extract(SHO,"^\\d+")) %>% 
    dplyr::select(PLAYER, base::intersect(pitchingStats,names(.))) %>%
    dplyr::rename_at(dplyr::vars(-PLAYER),~paste0(.,"_PitchingSeason"))
  
  pitching <- dplyr::mutate(pitching, IP_PitchingSeason = format(IP_PitchingSeason, nsmall = 1))
  
  return(pitching)
}




joinStatCrew <- function(player_df,tableList) {
  
  player_df$Name <- tolower(player_df$Name)
  
  output <- dplyr::rename_all(player_df,stringr::str_to_title) %>% 
    dplyr::rename(FirstName = First,
                  LastName = Last) %>% 
    dplyr::arrange(Number) %>%
    dplyr::left_join(tableList$batting, by  = c("Name" = "PLAYER")) %>% 
    dplyr::left_join(tableList$pitching, by = c("Name" = "PLAYER")) %>% 
    dplyr::arrange(as.numeric(Number))
  
  return(output)
}