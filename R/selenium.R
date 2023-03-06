getChromeVersion <- function(){
  if(xfun::is_windows()) {
    
    regVersion <- system2('reg', 'query "HKEY_CURRENT_USER\\Software\\Google\\Chrome\\BLBeacon" /v version',
                          stdout = T,
                          stderr = T)
    
    chromeVer <- na.omit(stringr::str_extract(regVersion,"(\\d+\\.?){4}"))
    
    chromeVer <- as.numeric_version(chromeVer)
    
  }
  
  return(chromeVer)
}


getDriverVersion <- function(){
  cdVer <- binman::list_versions("chromedriver")
  
  cdVers <- as.numeric_version(unlist(cdVer))
  
  chromeVer <- getChromeVersion()
  
  version <- max(cdVers[cdVers <= chromeVer])
  
  as.character(version)
}

startChromeServer <- function(version = NULL, verbose = F, check = F, port = 4444L){
  if(is.null(version)) version <- getDriverVersion()
  
  cD <- wdman::chrome(verbose = verbose,
                      version = version,
                      check = check,
                      port = port)
  
  logs <- cD$log()
  
  stopifnot(length(logs$stderr) == 0 & any(logs$stdout == "ChromeDriver was started successfully."))
  
  return(cD)
  
}


startSelenium <- function(server,
                          headless = T){
  
  portInUse <- as.numeric(na.omit(stringr::str_extract(server$log()$stdout,
                                                       "(?<=port )\\d+")))
  
  
  if(!headless) {
    return(RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                   port = portInUse,
                                   browserName = "chrome"))
  }
  
  RSelenium::remoteDriver(remoteServerAddr = "localhost",
                          port = portInUse,
                          browserName = "chrome",
                          # headless!
                          extraCapabilities = list(chromeOptions = list(
                            args = c('--headless','--disable-gpu',
                                     '--window-size=1280,800')
                          )))
}



fetch_SeleniumStats <- function(url,
                                verbose = F,
                                headless = T,
                                ...){
  serv <- startChromeServer(...)
  
  browser <- startSelenium(serv,headless = headless)
  
  browser$open(silent = !verbose)
  
  browser$navigate(url)
  
  Sys.sleep(2)
  
  # Find the stats iframe
  statsFrame <- browser$findElement("css","wmt-stats-iframe")
  
  # Locate the iframe wit hthe actual data
  iFrame <- statsFrame$findChildElement("css","iframe")
  
  # swtich to the Iframe
  browser$switchToFrame(iFrame)
  
  buttons <- browser$findElements("xpath","//button[normalize-space()='Batting' or normalize-space()='Pitching']")
  
  statsFrames <- purrr::map(buttons,
                            function(b) {
                              # Name the Button
                              tabType <- unlist(b$getElementText())
                              
                              # Click The Button
                              b$clickElement()
                              
                              # Get the Source of the new page
                              page <- b$getPageSource()[[1]]
                              
                              # Extract the Table
                              
                              parsed <- rvest::read_html(page)
                              tab <- rvest::html_table(parsed)
                              # return the table as a names list.
                              names(tab) <- tabType
                              
                              return(tab)
                            })
  
  browser$close()
  
  serv$stop()
  
  table <- unlist(statsFrames,recursive = F)
  
  table <- purrr::map(table,function(t){
    names(t)[names(t) == ""] <- "Player"
    names(t)[names(t) == "#"] <- "Number"
    return(t)
  })
  
  names(table) <- tolower(names(table))
  
  return(table)
  
}