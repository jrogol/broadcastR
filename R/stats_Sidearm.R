

getStats_Sidearm <- function(statsURL, player_df, headless = T, ...) {
  on.exit({
    sess$close()
    sess$parent$close()
  })

  sess <- startBrowser(...)

  sess$set_viewport_size(1280, 800, mobile = FALSE)

  if (!headless) {
    sess$view()
  }

  tables <- fetchStats_WMT(sess, statsURL, ...)
  # tables <- fetchStats_Sidearm(statsURL, ...)

  clean <- cleanStats_Sidearm(tables)

  joined <- joinStatCrew(player_df, clean)

  return(joined)
}


fetchStats_Sidearm <- function(url, session) {
  page <- xml2::read_html(url)

  # If... iframe w/class: cumulative-statistics__content-wrapper

  # Else If...table.w-full tbody tr td:not(:empty)

  battingSection <- page %>%
    rvest::html_element(
      "section#individual-overall-batting, div#individual-batting"
    )

  batting <- battingSection %>%
    rvest::html_element("table") %>%
    rvest::html_table()

  pitchingSection <- page %>%
    rvest::html_element(
      "section#individual-overall-pitching, div#individual-pitching"
    )

  pitching <- pitchingSection %>%
    rvest::html_element("table") %>%
    rvest::html_table()

  table <- list(batting = batting, pitching = pitching)

  table <- purrr::map(table, function(tab) {
    if (nchar(names(tab)[2]) == 0) {
      names(tab)[2] <- "Player"
    }

    return(tab)
  })

  return(table)
}

cleanStats_Sidearm <- function(listTable){
  
  listTable <- purrr::map(listTable, cleanPlayer_SidearmStats)
  
  batting <- cleanBatting_Sidearm(listTable)
  
  pitching <- cleanPitching_Sidearm(listTable)
  
  tables_out <- list(batting = batting, pitching = pitching)
  
  return(tables_out)
}

cleanPlayer_SidearmStats <- function(table, col = c("Player", "Name")) {
  if (!is.data.frame(table)) {
    stop("'table' must be a data.frame or tibble")
  }

  out <- table |>
    dplyr::rename(Player = dplyr::any_of(col)) |>
    dplyr::mutate(
      Player = Player |>
        as.character() |>
        stringr::str_remove("\\r\\n.*") |>
        stringr::str_trim() |>
        stringr::str_replace(
          "^([\\w\\-'\\. ]+),\\s*([\\w\\-' ]+)$",
          "\\2 \\1"
        )
    )

  out |>
    dplyr::rename_with(toupper)
}

cleanBatting_Sidearm <- function(listTable,...){
  batting <- listTable$batting %>% 
    tidyr::separate("SB-ATT", into = c("SB","ATT"), convert = T) %>% 
    dplyr::mutate(CS = ATT-SB)
  
  if("GP-GS" %in% names(listTable$batting)){
    batting <- tidyr::separate(batting,
                               "GP-GS", into = c("GP","GS"), convert = T)
  }
  
  batting <- batting %>% 
    dplyr::filter(!is.na(AB),
                  !grepl("--|Total|Opponent",PLAYER)) %>% 
    dplyr::select(PLAYER, base::intersect(battingStats,names(.))) %>% 
    dplyr::rename_at(dplyr::vars(-PLAYER),~paste0(.,"_BattingSeason"))
  
  return(batting)
}

cleanPitching_Sidearm <- function(listTable,...) {
  pitching <- listTable$pitching %>% 
    # tidyr::separate("W-L", into = c("Win","Loss"),
    #                 sep = "-") %>% 
    # tidyr::separate("APP-GS",
    #                 into = c("G","GS"),
    #                 sep = "-") %>% 
    dplyr::mutate(SHO = stringr::str_remove(SHO,"-.*"))
  
  if("W-L" %in% names(listTable$pitching)){
    pitching <- tidyr::separate(pitching,
                                "W-L", into = c("Win","Loss"),
                                sep = "-")
  }
  
  if("APP-GS" %in% names(listTable$pitching)){
    pitching <-  tidyr::separate(pitching,
                                 "APP-GS", into = c("G","GS"),
                                 sep = "-")
  } else {
    pitching <- try(dplyr::rename(pitching, "G" = GP),silent = T)
  }
  
  pitching <- try(dplyr::rename_at(pitching, dplyr::vars(dplyr::matches("win|loss")),
                                   stringr::str_to_title), silent = T)
  
  pitching <- pitching %>% 
    dplyr::select(PLAYER, pitchingStats) %>% 
    dplyr::filter(!is.na(G), 
                  !grepl("--|Total|Opponent",PLAYER)) %>% 
    dplyr::rename_at(dplyr::vars(-PLAYER),~paste0(.,"_PitchingSeason"))
  
  pitching <- dplyr::mutate(pitching, IP_PitchingSeason = format(IP_PitchingSeason, nsmall = 1))
  
  return(pitching)
}

fetchStats_WMT <- function(sess, url, timeout_ms = 10000, ...) {
  sess$go_to(url)

  # Wait for WMT iframe (short probe timeout; rejection = no iframe = Vue page)
  iframe_probe_ms <- 3000
  iframe_result <- sess$Runtime$evaluate(
    wait_for_element_js(
      "iframe.cumulative-statistics__content-wrapper",
      return_expr = "el.src",
      timeout_ms = iframe_probe_ms
    ),
    awaitPromise = TRUE,
    timeout = iframe_probe_ms
  )

  iframe_src <- iframe_result$result$value # NULL if timed out / not found

  if (!is.null(iframe_src) && startsWith(iframe_src, "https://wmt.games")) {
    # Nuxt/WMT path: navigate into the iframe, use button + advanced-table selector
    sess$go_to(iframe_src)

    table_js <- wait_for_element_js(
      "table.advanced-table__table",
      "el.outerHTML",
      timeout_ms
    )

    batting_result <- sess$Runtime$evaluate(
      table_js,
      awaitPromise = TRUE,
      timeout = timeout_ms
    )
    batting <- rvest::read_html(batting_result$result$value) |>
      rvest::html_table()

    selectButton(sess, "Pitching")
    pitching_result <- sess$Runtime$evaluate(
      table_js,
      awaitPromise = TRUE,
      timeout = timeout_ms
    )
    pitching <- rvest::read_html(pitching_result$result$value) |>
      rvest::html_table()
  } else {
    # Vue/Sidearm path: combobox toggle, generic table selector
    table_js <- wait_for_element_js("table", "el.outerHTML", timeout_ms)

    selectComboBox(sess, 2, "Batting")
    batting_result <- sess$Runtime$evaluate(
      table_js,
      awaitPromise = TRUE,
      timeout = timeout_ms
    )
    batting <- rvest::read_html(batting_result$result$value) |>
      rvest::html_table()

    selectComboBox(sess, 2, "Pitching")
    pitching_result <- sess$Runtime$evaluate(
      table_js,
      awaitPromise = TRUE,
      timeout = timeout_ms
    )
    pitching <- rvest::read_html(pitching_result$result$value) |>
      rvest::html_table()
  }

  list(batting = batting[[1]], pitching = pitching[[1]])
}

# fetchStats_WMT <- function(sess, url, headless = T, ...) {
#   js <- "vue"
#   navigate_and_wait(sess, url)

#   # get iframe src from parent DOM
#   iframe_src <- sess$Runtime$evaluate(
#     "document.querySelector('iframe.cumulative-statistics__content-wrapper')?.src"
#   )$result$value

#   if (!is.null(iframe_src) && startsWith(iframe_src, "https://wmt.games")) {
#     # Navigate chromote directly to the iframe src
#     js <- "nuxt"
#     navigate_and_wait(sess, iframe_src)
#   }

#   toggleStats_WMT(sess, js, "Batting")

#   result <- sess$Runtime$evaluate(
#     '
#     document.querySelector("table").outerHTML
#   '
#   )

#   batting_table <- rvest::read_html(result$result$value) |>
#     rvest::html_table()

#   # NUXT
#   toggleStats_WMT(sess, js, "Pitching")
#   result <- sess$Runtime$evaluate(
#     '
#     document.querySelector("table").outerHTML
#   '
#   )

#   pitching_table <- rvest::read_html(result$result$value) |>
#     rvest::html_table()

#   return(list(batting = batting_table[[1]], pitching = ptiching_table[[1]]))
# }


toggleStats_WMT <- function(
  sess,
  js_type = c("vue", "nuxt"),
  option_type = c("Batting", "Pitching")
) {
  js_type = match.arg(js_type)
  option_type = match.arg(option_type)
  switch(
    js_type,
    vue = selectComboBox(sess, 2, option_type),
    nuxt = selectButton(sess, option_type)
  )

  invisible(TRUE)
}

# NUXT
selectButton <- function(sess, value) {
  sess$Runtime$evaluate(glue::glue(
    '
  Array.from(document.querySelectorAll("button"))
    .find(b => b.innerText.trim() === "{value}")
    .click()
'
  ))
  invisible(TRUE)
}

## VUE. idx 2 is Batting/Pitching/Fielding. idx 1 is Overall/Conference
selectComboBox <- function(sess, index, option_name, wait_ms = 300) {
  # ── Step 1: Click the combobox to open it ───────────────────────────────────
  open_result <- sess$Runtime$evaluate(sprintf(
    "
    (function() {
      var boxes = document.querySelectorAll('[role=\"combobox\"]');
      if (!boxes[%d]) return 'combobox index %d not found';
      boxes[%d].click();
      return 'opened';
    })()
  ",
    index,
    index,
    index
  ))$result$value

  if (open_result != "opened") {
    stop(sprintf("selectComboBox: %s", open_result))
  }

  # ── Step 2: Brief pause for listbox to register as open ─────────────────────
  Sys.sleep(wait_ms / 1000)

  # ── Step 3: Click the matching option ───────────────────────────────────────
  click_result <- sess$Runtime$evaluate(sprintf(
    "
    (function() {
      var option = Array.from(document.querySelectorAll('[role=\"option\"]'))
        .find(o => o.innerText.trim() === '%s');
      if (!option) return 'option not found: %s';
      option.click();
      return 'selected';
    })()
  ",
    option_name,
    option_name
  ))$result$value

  if (click_result != "selected") {
    stop(sprintf("selectComboBox: %s", click_result))
  }

  invisible(TRUE)
}