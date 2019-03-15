# Per the documentation "Read the list of abbreviations of states and
# territories of the United States from the relevant Wikipedia article"
states <- Ecfun::readUSstateAbbreviations()

# Reshape to the 50 states and Washington, D.C.
states <- dplyr::as_tibble(states) %>% 
  dplyr::filter(USPS %in% c(state.abb, "DC")) %>% 
  dplyr::select(Name, USPS, AP, Other)


# Clean "Other" to introduce NAs
states <- states %>% 
  dplyr::mutate(Other = if_else(Other == "",
                         as.character(NA),
                         Other))

# Export for use
usethis::use_data(states, overwrite = T)
