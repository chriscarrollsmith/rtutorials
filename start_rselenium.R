#This function starts the RSelenium driver regardless of whether or not the port is already open
#It attempts to start the driver, and if a port is already open, it kills the task that's using that port and then tries to start the driver again

#Note that if you encounter a "session not created" error, you will need to specify a Chrome version in the rsDriver() function
#For instructions on how to do that, see here: https://stackoverflow.com/a/55212731/14831364


start_selenium <- function(attempted = 0, condition = "Success starting Selenium web driver!", browserpreference = "chrome"){
  if(attempted >= 2){
    return("Failure starting Selenium web driver: Port in use and Java task kill didn't fix it!")
  }
  tryCatch({
    .GlobalEnv$rD <- rsDriver(browser = c(browserpreference))
    .GlobalEnv$driver <- rD[["client"]]
  }, error = function(error_condition) {
    if(grepl("already in use",error_condition, fixed = TRUE)){
      tryCatch(driver$close(),error = function(error_condition){message(error_condition)})
      rD[["server"]]$stop()
      system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
      attempted <- attempted + 1
      condition <<- start_selenium(attempted)
    }
  })
  return(condition)
}
