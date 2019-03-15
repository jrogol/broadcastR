# All Names ---------------------------------------------------------------
file <- here::here("data-raw","ESPN_BASEBALL_TEMPLATE.xls")

template <- readxl::read_excel(file,
                             sheet = "MAIN", 
                             skip = 1,
                             .name_repair = 'universal')

# Player biographic information form the first 11 columns. The statistics
# repeat, which makes the column names non-unique.


# The first row of the file contains the header.
suffix <- readxl::read_excel(file,
                             sheet = "MAIN",
                             n_max = 1,
                             col_names = F)

# this can be reshaped to long, and filled forwards to generate a vector of
# suffixes.
suffix <- suffix %>% 
  tidyr::gather(column, name) %>% 
  # Fill with previous
  tidyr::fill(name) %>% 
  dplyr::select(-column)

# To complete the suffix, an underscore will be added to the beginning of the string.

##### NOTE: This doesn't add _PitchingSeries to the 14 other columns here...
suffix <- suffix %>% 
  dplyr::mutate(name = paste0("_",name))

# The suffixes aren't the same length as the formatted names, as the suffixes
# truncate the empty columns at the end. We can add them my repeating the last
# suffix "n" times (where "n" is equal to the number of overall columns, less
# those in the `suffix` vector)


suffixes <- c(suffix$name,
              rep(suffix$name[nrow(suffix)],
                  length(names(template)) - nrow(suffix))
)


# concatenating the existing column names:
format <- paste0(names(template),suffixes)

# The names are messy - two periods prefix numbers, and (along with the column
# number) are used to differentiate between similar columns. These should be removed.


format <- gsub("\\.\\.(\\d{2,})?","",format,perl = T)

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


