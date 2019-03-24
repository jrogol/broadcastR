# Per the documentation "Read the list of abbreviations of states and
# territories of the United States from the relevant Wikipedia article"
states <- Ecfun::readUSstateAbbreviations()

# Reshape to the 50 states and Washington, D.C.
states <- dplyr::as_tibble(states) %>% 
  dplyr::filter(USPS %in% c(state.abb, "DC")) %>% 
  dplyr::select(Name, USPS, AP, Other)


# Reshape to long, with multiple entries per state.
states <- states %>% 
  tidyr::gather("type","state",-USPS) %>% 
  dplyr::mutate(state = strsplit(state, ", ")) %>% 
  tidyr::unnest() %>%
  dplyr::distinct(USPS, state)


# Export for use
usethis::use_data(states, overwrite = T)
