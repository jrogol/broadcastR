getChromeVersion <- function(){
  if(xfun::is_windows()) {
    
    regVersion <- system2('reg', 'query "HKEY_CURRENT_USER\\Software\\Google\\Chrome\\BLBeacon" /v version',
                          stdout = T,
                          stderr = T)
    
    chromeVer <- na.omit(stringr::str_extract(regVersion,"(\\d+\\.?){4}"))
    
    
  }
  
  if(xfun::is_macos()) {
    chromeVer <- system2("defaults",
                         c("read",
                           "/Applications/Google\\ Chrome.app/Contents/Info.plist",
                           "CFBundleShortVersionString"),
                         stdout =T)
  }
  
  chromeVer <- as.numeric_version(chromeVer)
  
  return(chromeVer)
}

fetchDriver <- function(chromeVer = NULL){
  # List of Chrome for Testing Assets
  knownDL <- "https://googlechromelabs.github.io/chrome-for-testing/known-good-versions-with-downloads.json"
  
  rsp <- jsonlite::fromJSON(knownDL)
  
  ver <- rsp$version
  
  # Find latest release for the milestone (first digits, or item of the hidden list)
  latest <- ver[max(grep(paste0("^",unlist(chromeVer)[[1]],"\\b"),
                         ver$version)),]
  
  # Chrome drivers for the latest release
  drivers <- latest$downloads$chromedriver[[1]]
  
  # determine platform & architecture
  # "linux64"   "mac-arm64" "mac-x64"   "win32"     "win64"
  if(xfun::is_macos()){
    platform <- ifelse(xfun::is_arm64(),"mac-arm64","mac-x64")
    
    arch <- "mac64"
  }
  
  if(xfun::is_windows()){
    platform <- "win32"
    
    arch <- platform
  }
  
  ix <- which(grepl(platform,drivers$platform))
  
  url <- drivers$url[ix]
  
  
  
  # Where is the driver located?
  driverDir <- binman::app_dir("chromedriver",check = T)
  
  
  # Make a new subdir for the version, and unzip there!
  subdir <- file.path(driverDir,arch,chromeVer)
  
  if(!dir.exists(subdir)){
    dir.create(subdir,recursive = T)
  }
  
  zipName <- tail(unlist(strsplit(url,"/")),1)
  
  zipPath <- file.path(subdir,zipName)
  
  download.file(url,
                destfile = zipPath)
  
  files <- unzip(zipPath,list = T)
  
  # Get the name of the file that's NOT the license.
  drvName <- grep("LICENSE.chromedriver",
                  files$Name,
                  ignore.case = T,
                  invert = T,
                  value = T)
  
  #unzips to working directory by default
  unzip(zipPath,
        files = NULL,
        exdir = subdir
  )
  # Need to make the file executable!
  fs::file_chmod(file.path(subdir,drvName),
                 "755")
  
  # Move contents of folder
  zipFolder <- list.dirs(subdir,recursive = F)
  
  for (f in list.files(zipFolder)){
    fs::file_move(file.path(zipFolder,f),
                  subdir)
  }
  
  fs::dir_delete(zipFolder)
  
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
  
  Sys.sleep(5)
  
  # Find the stats iframe
  statsFrame <- tryCatch(browser$findElement("css","wmt-stats-iframe"),
                         error= function(e) {
                           attr(e,"try-error") <- e
                           return(e)})
  
  if(!inherits(statsFrame,"error")) {
    # Locate the iframe wit hthe actual data
    iFrame <- statsFrame$findChildElement("css","iframe")
    # swtich to the Iframe
    browser$switchToFrame(iFrame) 
  }
  
  buttons <- browser$findElements("xpath","//button[normalize-space()='Batting' or normalize-space()='Pitching']")
  
  statsFrames <- purrr::map(buttons,
                            function(b) {
                              # Name the Button
                              tabType <- unlist(b$getElementText())
                              
                              # Click The Button
                              b$clickElement()
                              Sys.sleep(2)
                              
                              # Get the Source of the new page
                              page <- b$getPageSource()[[1]]
                              
                              # Extract the Table
                              
                              parsed <- rvest::read_html(page)
                              tab <- rvest::html_table(parsed)
                              
                              
                              if(inherits(tab, 'list') & length(tab) == 2){
                                names(tab) <- c("Batting","Pitching")
                                return(tab)
                              }
                              
                              # return the table as a names list.
                              names(tab) <- tabType
                              
                              return(tab)
                            })
  
  browser$close()
  
  serv$stop()
  
  table <- unlist(unique(statsFrames),recursive = F)
  
  table <- purrr::map(table,function(t){
    names(t)[names(t) %in% c("","Player")] <- "PLAYER"
    names(t)[names(t) %in% c("#","NUMBER")] <- "Number"
    names(t)[names(t) == "K"] <- "SO"
    
    if(any(grepl("\\n",t$PLAYER))){
      t$PLAYER <- stringr::str_extract(t$PLAYER,".+(?=\\n)")
    }
    
    return(t)
  })
  
  names(table) <- tolower(names(table))
  
  return(table)
  
}