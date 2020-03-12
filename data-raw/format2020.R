library(dplyr)
# All Names ---------------------------------------------------------------

# Player biographic information form the first 15 columns. The statistics start
# at column 54, and the names repeat, which makes the column names non-unique.

# Unlike the previous version of the sheet, the headers and column names appear
# in different locations. Column names for the Player Demographic data and
# Season Stats reside on line 3. Series stats are on line 2, but are not needed
# for this exercise.

file <- here::here("data-raw","ESPN_BASEBALL_AWAY.xlsx")

template <- readxl::read_excel(file,
                             sheet = "MAIN",
                             skip = 1,
                             n_max = 2,
                             col_names = F,
                             .name_repair = 'universal')

# Only select the names (columns 1-15) and stats (columns 54-end)
lastBio <- 15
firstStat <- 54
gap <- seq(lastBio+1,firstStat-1)

# Remove the gap
template <- template %>% 
  select(-gap)

# A temporary column is added to the start, identifying the headers in row 1 and
# names proper in row 2. Then the data is reshaped to long.
template <- tibble("temp" = c("header","name")) %>% 
  bind_cols(template) %>% 
  tidyr::gather(column,name,-temp) %>% 
  mutate(column = as.integer(gsub("\\.{3}","",column))) %>% 
  tidyr::spread(temp,name) %>% 
  select(-column) %>% 
  filter(!is.na(name))

# Add "PlayerDemographic" as the header for the first 15 items.

template$header[1:15] <- "PlayerDemographics"


# The other headers are filled down. A title is formed by name and header,
# separated by an underscore.
template <- template %>% 
  tidyr::fill(header) %>% 
  mutate(name = gsub("(?<=First|Last)[Nn]ame$","Name",name, perl = T),
         title = paste(name,header,sep = "_"))
# concatenating the existing column names:
format <- pull(template, title)

# Remove any extra spaces.
format <- gsub(" ","",format)


# Season Batting Stats -----------------------------------------------------------
# the following are the stats we're looking for:
battingStats <- format %>% 
  stringr::str_subset("BattingSeason") %>% 
  gsub("_\\w+","",.)


# We don't have data for Bases Loaded nor RISP situations.

battingStats <- battingStats[!grepl("RISP|Bases",battingStats)]


# Season Pitching Stats ----------------------------------------------------------

pitchingStats <- format %>% 
  stringr::str_subset("PitchingSeason") %>% 
  gsub("_\\w+","",.)



# Formatted List ----------------------------------------------------------

usethis::use_data(format, battingStats, pitchingStats, internal = T,
                  overwrite = T)


