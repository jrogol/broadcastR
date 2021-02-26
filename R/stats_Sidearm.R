

getStats_Sidearm <- function(statsURL, player_df, ...){
  tables <- fetchStats_Sidearm(statsURL,...)
  
  clean <- cleanStats_Sidearm(tables)
  
  joined <- joinStatCrew(player_df, clean)
  
  return(joined)
}


fetchStats_Sidearm <- function(url){
  page <- xml2::read_html(url)
  
  battingSection <- page %>% 
    rvest::html_node("section#individual-overall-batting, div#individual-batting")
  
  batting <- battingSection %>% 
    rvest::html_node("table") %>% 
    rvest::html_table()
  
  pitchingSection <- page %>% 
    rvest::html_node("section#individual-overall-pitching, div#individual-pitching")
  
  pitching <- pitchingSection %>% 
    rvest::html_node("table") %>% 
    rvest::html_table()
  
  table <- list(batting = batting, pitching = pitching)
  
  table <- purrr::map(table, function(tab){
    if(nchar(names(tab)[2]) == 0){
      names(tab)[2] <- "Player"
    }
    
    return(tab)
  })
  
  return(table)
}

cleanStats_Sidearm <- function(listTable){
  
  listTable <- purrr::map(listTable, cleanPlayer_SidearmStats)
  
  batting <- cleanBatting_Sidearm(listTable)
  
  pitching <- cleanPitching_Sidearm(listTable)
  
  tables_out <- list(batting = batting, pitching = pitching)
  
  return(tables_out)
}



cleanPlayer_SidearmStats <- function(table, col = "Player") {

  table[,col] <- stringr::str_trim(gsub("\\r\\n.*","",table[,col]))
  
  table[,col] <- sub("([\\w-' \\.]+), ([\\w-' ]+)", "\\2 \\1", table[,col], perl = T)
  
  table <- dplyr::rename_all(table,toupper)
  
  return(table)
}

cleanBatting_Sidearm <- function(listTable,...){
  batting <- listTable$batting %>% 
    tidyr::separate("SB-ATT", into = c("SB","ATT"), convert = T) %>% 
    dplyr::mutate(CS = ATT-SB)
  
  if("GP-GS" %in% names(listTable$batting)){
    batting <- tidyr::separate(batting,
                               "GP-GS", into = c("GP","GS"), convert = T)
  }
  
  batting <- batting %>% 
    dplyr::filter(!is.na(AB),
                  !grepl("--|Total|Opponent",PLAYER)) %>% 
    dplyr::select(PLAYER, base::intersect(battingStats,names(.))) %>% 
    dplyr::rename_at(dplyr::vars(-PLAYER),~paste0(.,"_BattingSeason"))
  
  return(batting)
}

cleanPitching_Sidearm <- function(listTable,...) {
  pitching <- listTable$pitching %>% 
    # tidyr::separate("W-L", into = c("Win","Loss"),
    #                 sep = "-") %>% 
    # tidyr::separate("APP-GS",
    #                 into = c("G","GS"),
    #                 sep = "-") %>% 
    dplyr::mutate(SHO = stringr::str_remove(SHO,"-.*"))
  
  if("W-L" %in% names(listTable$pitching)){
    pitching <- tidyr::separate(pitching,
                                "W-L", into = c("Win","Loss"),
                                sep = "-")
  }
  
  if("APP-GS" %in% names(listTable$pitching)){
    pitching <-  tidyr::separate(pitching,
                                 "APP-GS", into = c("G","GS"),
                                 sep = "-")
  } else {
    pitching <- try(dplyr::rename(pitching, "G" = GP),silent = T)
  }
  
  pitching <- try(dplyr::rename_at(pitching, dplyr::vars(dplyr::matches("win|loss")),
                                   stringr::str_to_title), silent = T)
  
  pitching <- pitching %>% 
    dplyr::select(PLAYER, pitchingStats) %>% 
    dplyr::filter(!is.na(G), 
                  !grepl("--|Total|Opponent",PLAYER)) %>% 
    dplyr::rename_at(dplyr::vars(-PLAYER),~paste0(.,"_PitchingSeason"))
  
  pitching <- dplyr::mutate(pitching, IP_PitchingSeason = format(IP_PitchingSeason, nsmall = 1))
  
  return(pitching)
}


