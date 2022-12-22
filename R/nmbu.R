.onAttach <- function(libname, pkgname){
  libs <- .libPaths()
  ok  <- 0
  for(i in length(libs):1){
    if(file.access(libs[i], 2) == 0){
      ok <- i
    }
  }
  if(ok == 0){
    userdir <- unlist(strsplit(Sys.getenv("R_LIBS_USER"),.Platform$path.sep))[1L]
    if(grepl("\\\\", userdir, fixed=TRUE) || grepl("////", userdir, fixed=TRUE)){
      userdir <- "Z:\\R\\win-library\\3.3"}
    if(!file.exists(userdir)){
      dir.create(userdir, recursive = TRUE)}
    lib <- userdir
    .libPaths(c(lib,.libPaths()))
  } else {
    lib <- libs[ok]
    if(grepl("\\\\", lib, fixed=TRUE))
      lib <- "Z:\\R\\win-library\\3.3"
    .libPaths(c(lib,.libPaths()))
  }
  
  # Test for internet connection
  has.connection <- testURL()
  repo <- testUIB()
  
  if(has.connection && (!("RcmdrPlugin.NMBU"%in%installed.packages()[,"Package"]) || ("RcmdrPlugin.NMBU"%in%old.packages(repos=repo)[,"Package"]))){
    cat("Updating RcmdrPlugin.NMBU\n")
    install.packages("RcmdrPlugin.NMBU", repos=repo, lib=lib, dependencies=TRUE, quiet=TRUE)
    
    cat("Loading RcmdrPlugin.NMBU\n")
    try.message <- try(library(RcmdrPlugin.NMBU))
    if(class(try.message)=="try-error") {
      cat("\nRe-trying with more extensive update\n")
      install.packages("car", repos = repo, lib=lib, dependencies=TRUE)
      install.packages("lme4", repos = repo, lib=lib, dependencies=TRUE)
      install.packages("pls", repos = repo, lib=lib, dependencies=TRUE)
      install.packages("mixlm", repos = repo, lib=lib, dependencies=TRUE)
      install.packages("Rcmdr", repos = repo, lib=lib, dependencies=TRUE)
      library(RcmdrPlugin.NMBU, quietly = TRUE)
    }
  } else {
    library(RcmdrPlugin.NMBU, quietly = TRUE)
  }
  cat("\nRe-open a closed R Commander with the command: Commander()\nPlease report bugs to kristian.liland@nmbu.no\n")
  cat("\nCurrently installed:\n", R.Version()$version.string, "\n", sep="")
  cat("mixlm version ", utils:::installed.packages()["mixlm","Version"], "\n", sep="")
  cat("R Commander version ", utils:::installed.packages()["Rcmdr","Version"], "\n", sep="")
  cat("RcmdrPlugin.NMBU version ", utils:::installed.packages()["RcmdrPlugin.NMBU","Version"], "\n\n", sep="")
  
  if(has.connection){
    the.url <- url('http://repository.umb.no/R/nmbu.message.txt')
    cat(readLines(the.url)[-(1:4)], "\n", sep="\n")
    close(the.url)
  }
  if(as.numeric(R.Version()$major) < 4){
    cat("\nYou are advised to upgrade R to version 4.2.2 (or newer) (https://cran.uib.no)")
    cat(paste("\nCurrent ", R.Version()$version.string, "\n\n", sep=""))}
  if(!has.connection){
    cat("\nNo internet connection. Using latest local packages.\n")
  }
}

testURL <- function(){
  a <- NULL
  warn <- getOption("warn")
  options(warn=-1)
  the.url <- url('https://repository.umb.no/R/nmbu.message.txt')
  try(a<-readLines(the.url),TRUE)
  close(the.url)
  options(warn=warn)
  ifelse(is.null(a),FALSE,TRUE)
}
testUIB <- function(){
  warn <- getOption("warn")
  options(warn=-1)
  try(O <- open(U <- url('https://cran.uib.no/src/contrib/PACKAGES')))
  close(U)
  options(warn=warn)
  if(exists("O")){
	return('https://cran.uib.no')
  } else {
    return('https://cran.r-project.org')
  }
}