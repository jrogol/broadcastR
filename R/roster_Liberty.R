getRoster_Liberty <- function(url){
  roster <- fetchRoster_Liberty(url)
  
  cleaned <- cleanRoster_Liberty(roster)
  
  return(cleaned)
}



fetchRoster_Liberty <- function(url){
  page <- xml2::read_html(url)
  
  players <- rvest::html_elements(page,".rosterExcerpt")
  
  roster <- players %>% 
    purrr::map(fetchPlayer_Liberty) %>% 
    dplyr::bind_rows()
}


fetchPlayer_Liberty <- function(l){
  nodes <- rvest::html_elements(l,"h3, p")
  
  headers <- rvest::html_attr(nodes,"class")
  
  headers <- stringr::str_extract(headers, "[[:alpha:]]+$")
  
  headers[is.na(headers)] <- "Other"
  
  text <- rvest::html_text(nodes, trim = T)
  
  names(text) <- headers
  
  text
}


# Cleaning ----------------------------------------------------------------

cleanRoster_Liberty <- function(roster,
                                attrs = list(Position1 = "^[^[:space:]]+",
                                             ht = "\\d-\\d{1,2}",
                                             wt = "\\d{3}",
                                             bt = "[SRL] - [RL]")) {
  roster %>%
    dplyr::bind_cols(purrr::map(attrs,
                                function(attr) {
                                  roster$playerDetails %>%
                                    stringr::str_extract(attr)
                                }) %>%
                       dplyr::bind_cols()) %>%
    dplyr::select(-playerDetails) %>%
    dplyr::rename_with( ~ "Name", dplyr::one_of("title")) %>%
    dplyr::rename_with( ~ "No", dplyr::one_of("jersey")) %>%
    tidyr::separate(
      col = "Other",
      into = c("Year", "Hometown", "PriorSchool"),
      sep = "[[:space:]]{2,}/[[:space:]]+"
    ) %>%
    tidyr::separate(Hometown,
                    into = c("Hometown", "State"),
                    sep = ", ") %>%
    tidyr::separate(bt,
                    into = c("Bats", "Throws"))
}
