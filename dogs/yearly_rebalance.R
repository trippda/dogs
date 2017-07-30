##############################################################################################
# Figures out what to do for the yearly "rebalance" of my "small dogs of the dow" investments.
# (and gives me an excuse to play with R)
# 
# Usage: Rscript yearly_rebalance.R <TotalValueOfDogsToday>
##############################################################################################

# TO DO: Get TotalValueOfDogsToday from Ameritrade. Api access is very expensive, but this article says I can use httr:
#   https://stackoverflow.com/questions/10692066/how-to-webscrape-secured-pages-in-r-https-links-using-readhtmltable-from-xml
# TO DO: option to run interactively, prompting for input rather than cmd line--though, from what I understand, that is not really what R scripts do.

checkForPackages <- function()
{
  # Load packages.
  bPackagesFound <- TRUE
  pkgs <- c("XML","quantmod","xlsx")
  for(pkg in pkgs)
  {
    if(!suppressPackageStartupMessages(require(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)))
    {
      if(bPackagesFound)
      {
        bPackagesFound <- FALSE
      }
      missingPackageMessage <- paste0("***Package ", pkg, " is not installed. Install and try again.***")
      warning(missingPackageMessage)
    } 
  }
  
  return(bPackagesFound)
}

main <- function(currentTotalValue)
{
  # set up
  thisYear <- strftime(Sys.Date(),"%Y")
  lastYear <- as.character(as.integer(thisYear) - 1)
  
  # get last year's small dogs
  # *****TO DO******
  # separate sheets for each year, separate sheet for summary / calculations - perhaps that is the first sheet
  
  # working directory.
  # TO DO: configurable
  # This doesn't work since Rscript has pwd as HOME
  #setwd(paste0(Sys.getenv("HOME"),"/Documents/dogs"))
  #message(paste0("home is ", Sys.getenv("HOME")))
  setwd("C:/Users/Dan/Documents/dogs")
  
  # working file
  # TO DO: configurable
  smallDogsWorkingFileName <- "SmallDogs.xlsx"
  
  previousSmallDogs <- read.xlsx(smallDogsWorkingFileName,sheetName = lastYear)
  # TO DO: remove
  message(previousSmallDogs)
  
  # URL to scrape (current year)
  currentDogsURL <- paste0("http://www.dogsofthedow.com/",thisYear,"-dogs-of-the-dow.htm")
  
  # get the page. 
  # fyi I read that useInternalNodes = TRUE is ignored for HTML docs, but readHTMLTable below fails without it.
  currentDogs.html <- htmlTreeParse(currentDogsURL , useInternalNodes = TRUE)

  # parse, just to figure out which table. Leave commented for run time.
  # tables <- readHTMLTable(currentDogs.html,as.data.frame = FALSE)
  
  # get table #12 as a dataframe
  # (skip the row below the header row - it is meta info I do not want/need
  tables <- readHTMLTable(currentDogs.html,skip.rows = 2,stringsAsFactors=FALSE, which = 12)
  
  # select only the small dogs.
  # spaces in column names are problematic, so get rid of those first
  names(tables) <-gsub(" ",".",names(tables))
  currentSmallDogs <- subset(tables, Small.Dog == "Yes", Symbol)
  
  # convert currentSmallDogs dataframe into a character vector 
  currentSmallDogsCharVector <- currentSmallDogs[['Symbol']]
  
  # get a quote for each small dog
  # TO DO where do I include the library/package? library(quantmod)
  currentSmallDogsQuotes <- getQuote( currentSmallDogsCharVector, verbose = TRUE)
  
  # I only need Last, not the full quote
  currentSmallDogsQuotesLast <- currentSmallDogsQuotes$Last
  
  # write these to excel
  currentSmallDogsDataFrame <- data.frame(currentSmallDogsCharVector,currentSmallDogsQuotesLast)
  write.xlsx(currentSmallDogsDataFrame,smallDogsWorkingFileName,sheetName = thisYear, append = TRUE)

  # TO DO
  # some way to run multiple times without having to manually clean up the dogs excel file
  # make the format of the previous / current dogs tables consistent
  # compare the lists of dogs, create an action list from that
  # do the calculations showing the change in portfolio value
  # handle multiple years
  # organize into functions, called from main
  
}

# get required input
args <- commandArgs(TRUE)
if (length(args) != 1)
{
  message("Usage: ")
  message("       Rscript yearly_rebalance.R <TotalValueOfDogsToday>")
} else {
  # confirm dependencies
  goToMain <- checkForPackages()
  if(goToMain)
  {
    main(args[1])
    # TBD remove
    message("DONE MAIN")
  }
  #TBD: remove this
  message("***DONE ALL***")
}
