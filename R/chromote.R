getBrowserPath <- function(browser = c("chrome", "brave")) {
  browser <- match.arg(browser)

  sysname <- Sys.info()[["sysname"]]

  paths <- if (sysname == "Darwin") {
    list(
      chrome = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
      brave  = "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"
    )
  } else if (sysname == "Windows") {
    list(
      chrome = "C:/Program Files/Google/Chrome/Application/chrome.exe",
      brave  = "C:/Program Files/BraveSoftware/Brave-Browser/Application/brave.exe"
    )
  } else {
    stop("Unsupported platform: ", sysname)
  }

  path <- paths[[browser]]

  if (!file.exists(path)) {
    stop(sprintf("%s not found at:\n  %s", tools::toTitleCase(browser), path))
  }

  return(path)
}



startBrowser <- function(browser = c("brave", "chrome"), headless = FALSE) {
  browser <- match.arg(browser)

  path <- getBrowserPath(browser)

  # chromote::Chrome$new() reads the "chromote.headless" option internally via
  # default_chrome_args() and injects --headless regardless of the args we pass.
  # Setting "" suppresses that injection entirely for windowed mode.
  old_opts <- options(chromote.headless = if (headless) "new" else "")
  on.exit(options(old_opts), add = TRUE)

  extra_args <- c("--window-size=1280,800", if (headless) "--disable-gpu")

  chr  <- chromote::Chrome$new(path = path, args = extra_args)
  b    <- chromote::Chromote$new(browser = chr)
  sess <- b$new_session()

  sess
}

navigate_and_wait <- function(sess, target_url, wait_event = c("load", "domcontentloaded")) {
  wait_event <- match.arg(wait_event)

  p <- switch(
    wait_event,
    load = sess$Page$loadEventFired(wait_ = FALSE),
    domcontentloaded = sess$Page$domContentEventFired(wait_ = FALSE)
  )

  sess$Page$navigate(target_url, wait_ = FALSE)
  sess$wait_for(p)

  invisible(sess)
}




#' Wait for JavaScript-rendered elements in Chromote
#'
#' @param selector CSS selector string (e.g. "table.advanced-table__table")
#' @param return_expr JS expression using el (e.g. "el.outerHTML", "el.src")
#' @param timeout_ms passed to both setTimeout and sess$Runtime$evaluate(timeout = ...)
#' @returns Templated Javascript formatted as a string for evaluation in Chromote
#'
wait_for_element_js <- function(selector, return_expr = "el.outerHTML", timeout_ms = 10000) {
  sprintf("
new Promise((resolve, reject) => {
  const timeout = setTimeout(() => reject('Timeout waiting for: %s'), %d);
  const getEl = () => document.querySelector('%s');
  const resolve_if_found = (el) => {
    if (el) {
      clearTimeout(timeout);
      observer.disconnect();
      resolve(%s);
    }
  };
  const observer = new MutationObserver(() => resolve_if_found(getEl()));
  const el = getEl();
  if (el) {
    clearTimeout(timeout);
    resolve(%s);
  } else {
    observer.observe(document.body, { childList: true, subtree: true });
  }
});
  ", selector, timeout_ms, selector, return_expr, return_expr)
}
