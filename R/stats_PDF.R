getStats_PDF <- function(statsURL, player_df, ...) {
  data <- fetchStats_PDF(statsURL)

  clean <- cleanStats_PDF(data)

  joined <- joinStatCrew(player_df, clean)

  return(joined)
}


fetchStats_PDF <- function(url) {
  raw_text <- pdftools::pdf_text(url)[[1]]
  lines <- stringr::str_split(raw_text, "\n")[[1]]

  # Find the PLAYER header line for each table
  batting_header_idx  <- which(stringr::str_detect(lines, "PLAYER\\s+AVG"))[1]
  pitching_header_idx <- which(stringr::str_detect(lines, "PLAYER\\s+ERA\\s+W-L"))[1]

  # Detect the character offset where the stats section starts.
  # In a two-column layout (schedule on left, stats on right) PLAYER begins
  # at some offset > 1.  In a stats-only layout it begins at 1.
  # Either way the substring logic below is identical.
  stat_col_start <- stringr::str_locate(lines[batting_header_idx], "PLAYER")[1, "start"]

  # Strip the schedule column (or nothing, if stats-only layout)
  stat_lines <- stringr::str_sub(lines, stat_col_start)

  # Re-locate header indices in the trimmed lines (same positions, just narrower)
  batting_header  <- stat_lines[batting_header_idx]
  pitching_header <- stat_lines[pitching_header_idx]

  # Totals rows
  totals_idxs <- which(stringr::str_detect(stat_lines, "\\bTotals\\b"))
  # Batting rows: between the two headers (exclusive)
  batting_rows  <- stat_lines[(batting_header_idx + 1):(totals_idxs[1] - 1)]

  # Pitching rows: from after pitching header up to (not including) "Totals"
  pitching_rows <- stat_lines[(pitching_header_idx + 1):(totals_idxs[2] - 1)]

  batting  <- parse_stat_table(batting_rows,  batting_header)
  pitching <- parse_stat_table(pitching_rows, pitching_header)

  return(list(batting = batting, pitching = pitching))
}


# Parse a block of text lines into a tibble.
# header_line: the column-header line for this table (already trimmed)
# rows: character vector of data lines (already trimmed)
parse_stat_table <- function(rows, header_line) {
  # Drop separator lines (---) and blank lines
  rows <- rows[!stringr::str_detect(rows, "^[-\\s]*$") & stringr::str_trim(rows) != ""]

  # Extract column names from the header line.
  # "PLAYER" is always first; the rest are single tokens separated by whitespace.
  col_names <- c("PLAYER", stringr::str_split(
    stringr::str_trim(stringr::str_remove(header_line, "^PLAYER\\s+")),
    "\\s+"
  )[[1]])

  # Parse each data row.
  # Name format: "LASTNAME, Firstname"  followed by 2+ spaces then numeric stats.
  name_pat <- "^\\s?([A-Z][A-Z\\-' ]*,\\s*[A-Za-z\\-'. ]+?)\\s{2,}"

  parsed <- purrr::map(rows, function(line) {
    m <- regmatches(line, regexpr(name_pat, line, perl = TRUE))
    if (length(m) == 0 || m == "") return(NULL)

    player_name <- stringr::str_trim(stringr::str_remove(m, "\\s{2,}$"))
    remainder   <- stringr::str_sub(line, nchar(m) + 1)
    stats_vals  <- stringr::str_split(stringr::str_trim(remainder), "\\s+")[[1]]

    # Build named list: PLAYER + as many stat columns as values present
    n_stats <- min(length(stats_vals), length(col_names) - 1)
    vals <- c(list(PLAYER = player_name),
              stats::setNames(as.list(stats_vals[seq_len(n_stats)]),
                              col_names[seq_len(n_stats) + 1]))
    vals
  })

  parsed <- purrr::compact(parsed)
  dplyr::bind_rows(parsed)
}


cleanStats_PDF <- function(tableList) {
  # "LASTNAME, Firstname" → "Firstname LASTNAME" (same logic as cleanStats_StatCrew)
  if (sum(grepl("([A-Z -']+), ([[:alpha:]\\. '-]+)", tableList$batting$PLAYER)) > 0) {
    tableList <- purrr::map(
      tableList,
      ~dplyr::mutate(., PLAYER = gsub("([A-Z -']+), ([[:alpha:]\\. '-]+)",
                                      "\\2 \\1", PLAYER))
    )
  }
  tableList <- purrr::map(tableList,
                          ~dplyr::mutate(., PLAYER = tolower(PLAYER)))

  batting  <- cleanBatting_StatCrew(dplyr::rename_all(tableList$batting,tolower))
  pitching <- cleanPitching_PDF(tableList$pitching)

  return(list(batting = batting, pitching = pitching))
}


cleanBatting_PDF <- function(table) {
  batting <- dplyr::rename(table, dplyr::any_of(c(SO = "K", HBP = "HP"))) %>%
    # Coerce stat columns to numeric
    dplyr::mutate(dplyr::across(-PLAYER, as.numeric)) %>%
    # Select only the stats present in battingStats (extras like AVG, SLUG, E ignored)
    dplyr::select(PLAYER, base::intersect(battingStats, names(.))) %>%
    # Drop totals / opponents / incomplete rows
    dplyr::filter(!is.na(AB), !grepl("--|totals?|opponents?", PLAYER)) %>%
    dplyr::rename_at(dplyr::vars(-PLAYER), ~ paste0(., "_BattingSeason"))

  return(batting)
}


cleanPitching_PDF <- function(table) {
  pitching <- table %>%
    dplyr::filter(!is.na(ERA), !grepl("--|totals?|opponents?", PLAYER)) %>%
    dplyr::rename(any_of(c(G = "APP", SO = "K", HBP = "HP"))) %>%
    tidyr::separate("W-L", into = c("Win", "Loss"), sep = "-") %>%
    dplyr::mutate(dplyr::across(-PLAYER, as.numeric)) %>%
    dplyr::select(PLAYER, base::intersect(pitchingStats, names(.))) %>%
    dplyr::rename_at(dplyr::vars(-PLAYER), ~ paste0(., "_PitchingSeason"))

  pitching <- dplyr::mutate(pitching,
                            IP_PitchingSeason = format(IP_PitchingSeason, nsmall = 1))

  return(pitching)
}
