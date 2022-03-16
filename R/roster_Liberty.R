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
    bind_cols(map(attrs,
                  function(attr) {
                    roster$playerDetails %>%
                      str_extract(attr)
                  }) %>%
                bind_cols()) %>%
    select(-playerDetails) %>%
    rename_with( ~ "Name", one_of("title")) %>%
    rename_with( ~ "Number", one_of("jersey")) %>%
    mutate(across(any_of("Number"),as.integer),
           Name = gsub("[[:space:]]+"," ",Name)) %>% 
    separate(
      col = "Other",
      into = c("Year", "Hometown", "PriorSchool"),
      sep = "[[:space:]]{2,}/[[:space:]]+"
    ) %>%
    separate(Hometown,
             into = c("Hometown", "State"),
             sep = ", ") %>%
    separate(bt,
             into = c("Bats", "Throws"))
}