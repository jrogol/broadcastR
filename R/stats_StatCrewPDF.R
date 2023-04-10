getStats_PDF <- function(statsURL,player_df,...){
  data <- fetchStatCrew_PDF(statsURL, ...)
  
  clean <- cleanStats_StatCrew(data)
  
  return(clean)
}




fetchStatCrew_PDF <- function(pdfLink){
  text <- pdftools::pdf_text(pdfLink)
  # Only grab the first page
  byLine <- text[[1]] %>% 
    stringr::str_split("\\n+") %>% 
    unlist() %>% 
    stringr::str_trim() 
  
  # Which rows contain "player" in the header?
  
  header <- grep("player\\s+(avg|era)\\s",byLine,ignore.case = T)
  
  
  # Which rows contain the totals?
  totals <- grep("(^|\\s?)totals\\s+",byLine,ignore.case = T)
  
  
  batting <- byLine[(header[1]):(totals[1]-1)]
  pitching <- byLine[(header[2]):(totals[2]-1)]
  
  
  tableList <- list(batting = parseTable_PDF(batting), pitching = parseTable_PDF(pitching))
  
  return(tableList)
}


parseTable_PDF <- function(df){
  headers <- df[1] %>% 
    stringr::str_split("\\s+") %>% 
    unlist() %>% 
    (function(x){
      x[grep("player",x,ignore.case = T):length(x)]
    })
  
  
  # Not the first row, nor rows with "------"
  out <- df[-c(1,grep("\\-{2,}",df))] %>% 
    # Split into colums by spaces, but not between words.
    stringr::str_split("(?<=\\w)\\s+(?!\\w+)|(?<=\\d)\\s+") %>% 
    # From the end of each row, get n elements, where n is the number of columns
    # name the vector accordingly, and bind the rows.
    purrr::map(function(row){
      x <- tail(row,length(headers)) %>% 
        stringr::str_trim()
      
      names(x) <- headers
      x
    }) %>% 
    dplyr::bind_rows() %>%
    # (function(df){
    #   if(length(grep("w(in)?-l(oss)?",headers,ignore.case = T)) == 1){
    #     tidyr::separate(df,col = grep("w(in)?-l(oss)?",headers,ignore.case = T),
    #                     into = c("w","l"))
    #   } else df}) %>% 
    dplyr::mutate(dplyr::across(-dplyr::matches("player|^w\\-l|^sb\\-att"),readr::parse_number))
  
  return(out)
}