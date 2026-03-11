#' Read a roster from an ESPN production Excel template
#'
#' @param teamName Team name (passed through; not used for reading)
#' @param filePath Path to the .xlsx file
#' @param sport "baseball" or "softball"
#'
#' @return A tibble with columns: Number, Name, Position1, Bats, Throws,
#'   Height, Weight, Year, Hometown, State
getRoster_ESPN <- function(filePath, sheet = "MAIN", skip = 2, ...) {
  df <- readxl::read_excel(filePath, sheet = sheet, skip = skip, ...) |>
    dplyr::select(1:11) |>
    dplyr::filter(!is.na(LastName)) |>
    dplyr::transmute(
      Name = paste(Firstname, LastName),
      Number,
      Position1 = Position,
      Bats,
      Throws,
      Hometown,
      State,
      Height,
      Weight,
      Year
    )

  df$Number <- suppressWarnings(as.numeric(df$Number))
  df
}
