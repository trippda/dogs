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

processDogs <- function(previousDogs,currentDogs,portfolioValue)
{
  #retuns a data frame with "actions": how many of each stock to buy or sell
  
  # calculate target value for each small dog: total value of the portfolio today divided by 5 
  target <- as.numeric(portfolioValue)/5
  
  sellAll <- setdiff(previousDogs,currentDogs) # iflength greater than 0, these are previous dogs and I DO NOT have a quote yet
  # TO DO process sellAll
  
  
  buyAll <- setdiff(currentDogs,previousDogs) # if length greater than 0, these are current dogs and I already have the quote
  # TO DO clean  up / implement below
  # TO DO: What if length = o?
  # TO DO remove this:
  #pricesOfBuyAllDogs <- currentDogs[currentDogs$symbol %in% buyAll,"price.per.share"]
  pricesAndSymbolsOfBuyAllDogs <- currentDogs[currentDogs$symbol %in% buyAll, ] 
  
  # add column indicating number of shares to buy for each stock
  # number of shares to buy is 1/5 the portfolio value ("target") / current price
  pricesAndSymbolsOfBuyAllDogs$num.shares.to.buy <- round(target/pricesAndSymbolsOfBuyAllDogs$price.per.share)
  
  
  buyOrSell <- intersect(previousDogs,currentDogs)
  # TO DO process buyOrSell
  
  # TO DO combine the actions data frames
  
  # TO DO return the actions data frame (just using currentDogs for now)
  # TO DO *****************something wrong with pricesAndSymbolsOfBuyAllDogs
  return(currentDogs)
  #return(pricesAndSymbolsOfBuyAllDogs)
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
  #message(previousSmallDogs)
  
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
  currentSmallDogsQuotes <- getQuote( currentSmallDogsCharVector, verbose = TRUE)
  
  # I only need Last, not the full quote
  currentSmallDogsQuotesLast <- currentSmallDogsQuotes$Last
  
  # Create a clean data frame
  currentSmallDogsDataFrame <- data.frame(currentSmallDogsCharVector,currentSmallDogsQuotesLast)
  colnames(currentSmallDogsDataFrame) <- c("symbol","price.per.share")
  
  # compare the previous and current dogs TO DO: how???
  # sellAll <- setdiff(previous,current) # iflength greater than 0, these are previous dogs and I DO NOT have a quote yet
  # buyAll <- setdiff(current,previous) # if length greater than 0, these are current dogs and I already have the quote
  #   pricesOfBuyAllDogs <- currentSmallDogsDataFrame[currentSmallDogsDataFrame$symbol %in% buyAll,"price.per.share"]
  #   pricesAndSymbolsOfBuyAllDogs <- currentSmallDogsDataFrame[currentSmallDogsDataFrame$symbol %in% buyAll, ]
  #   add column indicating how many shares to buy:
  #   pricesAndSymbolsOfBuyAllDogs$num.shares.to.buy <- round(target/pricesAndSymbolsOfBuyAllDogs$price.per.share)
  # buyOrSell <- intersect(previous,current)
  
  # write the new (current) dogs to excel, in a new worksheet
  # TO DO clean up / remove readonly flag
  # TO DO don't do this here (?)
  if((as.character(args[2])!="readonly"))
  {
    write.xlsx(currentSmallDogsDataFrame,smallDogsWorkingFileName,sheetName = thisYear, append = TRUE)
  }

  # figure out what to buy or sell
  # TO DO do not do the write above (?)
  actionsDataFrame <- processDogs(previousSmallDogs,currentSmallDogsDataFrame,currentTotalValue)
  
  # TO DO: decide if I want to write this now
  write.xlsx(actionsDataFrame,smallDogsWorkingFileName,sheetName = paste(thisYear,"actions", sep = "-"),append = TRUE)
  
  
  
  # TO DO
  # compare the lists of dogs, create an action list from that
  # do the calculations showing the change in portfolio value
  # handle multiple years
  # organize into functions, called from main
  # clean up the command line dev options ('readonly'). Move it to configuration.
  
}

# get required input
# TO DO: remove dev flag options (move to config)
args <- commandArgs(TRUE)
if (length(args) != 2)
{
  message("Usage: ")
  message("       Rscript yearly_rebalance.R <TotalValueOfDogsToday> <readonly|foo>")
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
