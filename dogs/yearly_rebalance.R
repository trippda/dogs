##############################################################################################
# Figures out what to do for the yearly "rebalance" of my "small dogs of the dow" investments.
# (and gives me an excuse to play with R)
# 
# Usage: Rscript yearly_rebalance.R
##############################################################################################

# TO DO start here, this function is very wrong - wont return properly
checkForPackages <- function()
{
  # Load packages.
  # TO DO: bash / Rscript is loading every time, with status message. Why?
  bPackagesFound <- TRUE
  pkgs <- c("XML","quantmod","httr")
  for(pkg in pkgs)
  {
    if(!require(pkg, character.only = TRUE))
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
  # For now, just prompt for the total value of the current investment.
  # TO DO: error checking, reprompt, retry option
  # TO DO: Get this from Ameritrade. Api access is very expensive, but this article says I can use httr:
  #   https://stackoverflow.com/questions/10692066/how-to-webscrape-secured-pages-in-r-https-links-using-readhtmltable-from-xml
  readCurrentTotalValue <- function()
  {
    currentTotalValue <- readline(prompt = "Enter total value of the dogs today: ")
    return(currentTotalValue)
  }
  # TBD: what does interactive mean, why didn't the script pause after input prompt
  # TBD: support both prompted input and cmd line params
  if(interactive())
  {
    currentTotalValue <- readCurrentTotalValue()
  }
  
  # URL to scrape (current year)
  currentDogsURL <- paste0("http://www.dogsofthedow.com/",strftime(Sys.Date(),"%Y"),"-dogs-of-the-dow.htm")
  
  # get the page. TO DO: what does useInternalNodes do?
  currentDogs.html <- htmlTreeParse(currentDogsURL , useInternalNodes = TRUE)
  
  # parse, just to figure out which table. Leave commented for run time.
  # tables <- readHTMLTable(currentDogs.html,as.data.frame = FALSE)
  
  # get table #12 as a dataframe
  # (skip the row below the header row - it is meta info I do not want/need
  tables <- readHTMLTable(currentDogs.html,skip.rows = 2,stringsAsFactors=FALSE, which = 12)
  
  # just for fun. TO DO remove this, I don't need all dow tickers
  # dowTickers <- tables[1]
  
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
}

# required input
args <- commandArgs(TRUE)
if (length(args) != 1)
{
  message("Usage: ")
  message("       Rscript yearly_rebalance.R <TotalValueOfDogsToday>")
} else {
  #bPackagesFound <- TRUE
  goToMain <- checkForPackages()
  #if (goToMain <- checkForPackages(bPackagesFound))
  #if (TRUE)
  if(goToMain)
  {
    main(args[1])
    # TBD remove
    message("DONE MAIN")
  }
  #TBD: remove this
  message("***DONE ALL***")
}
