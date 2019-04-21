link <- "http://vcuathletics.com/sports/bsb/2018-19/roster"

page <- xml2::read_html(link)

players <- page %>% 
  rvest::html_node("div.roster") %>% 
  rvest::html_node("table") %>% 
  rvest::html_table()

players %>% 
  dplyr::mutate_all(~gsub("^.+?:","",.)) %>% 
  dplyr::mutate_all(~gsub("\\s+"," ",.)) %>% 
  tidyr::separate("B/T", into = c("Bats","Throws"),
                  sep = "/") %>% 
  tidyr::extract("Hometown/Previous School",into = c("Hometown","State"),
                 "(^[\\w -]+), ([\\w, -\\.]+)(?= /)") %>% 
  # Format height appropriately.


dplyr::mutate(Year = stringr::str_replace(Cl.,"Fr.", "Freshman") %>% 
                stringr::str_replace("So." , "Sophomore") %>% 
                stringr::str_replace("Jr.", "Junior") %>% 
                stringr::str_replace("Sr.", "Senior") %>% 
                stringr::str_replace("Gr.", "Graduate Student") %>% 
                stringr::str_replace("R-|Redshirt ?", "RS "),
              Height = stringr::str_replace(Ht., "(\\d)-(\\d+)","\\1'\\2\\\""))
