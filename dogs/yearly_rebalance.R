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

# gets a quote for each symbol, returns LAST
getLast <- function(symbolCharVector)
{
  # get a quote for each
  # TO DO: do I need that verbose flag?
  quoteCharVector <- getQuote( symbolCharVector, verbose = TRUE)
  
  # I only need Last, not the full quote
  quoteCharVectorLast <- quoteCharVector$Last
  
  return(quoteCharVectorLast)
}

#retuns a data frame with "actions", which is the number of each stock to buy or sell
processDogs <- function(previousDogs,currentDogs,portfolioValue)
{
  # calculate target value for each small dog: total value of the portfolio today divided by 5 
  target <- as.numeric(portfolioValue)/5
  
  # Get current price.per.share for previousDogs
  previousDogs$price.per.share <- getLast(as.character(previousDogs$symbol))

  ####################################################### 
  # First, the dogs I own that are no longer small dogs: sell all I have
  ####################################################### 
  
  # if length greater than 0, these are previous dogs (I got a current quote above)
  # *****TO DO: What if length = o?*****
  sellAll <- setdiff(previousDogs$symbol,currentDogs$symbol) 
  # create a new data frame of sellAll dogs symbols, number of shares I own, and SELL action
  # include the current price per share, just for a sanity check calculation before selling.
  sellAllDogsAndActions <- previousDogs[previousDogs$symbol %in% sellAll,c("symbol","price.per.share","num.shares")]
  sellAllDogsAndActions$action <- "SELL"
  # include the total I will own after executing the trade action (0, since I sell all)
  sellAllDogsAndActions$num.shares.after.action <- 0
 
  ####################################################### 
  # Next, the new small dogs--ones I do not own any of yet: buy all (target value = 1/5 portfolio value)
  #######################################################  

  # if length greater than 0, these are current dogs. I already have the quote
  # *****TO DO: What if length = o?*****
  buyAll <- setdiff(currentDogs$symbol,previousDogs$symbol) 

  # make a new data frame from the new ("buy all") symbols and quotes
  # include the current price per share, just for a sanity check calculation before selling.
  buyAllDogsAndActions <- currentDogs[currentDogs$symbol %in% buyAll, ] 

  # add column indicating number of shares to buy for each stock
  # number of shares to buy is 1/5 the portfolio value ("target") / current price, rounded
  buyAllDogsAndActions$num.shares <- round(target/buyAllDogsAndActions$price.per.share)
  buyAllDogsAndActions$action <- "BUY"
  # include the total I will own after executing the trade action (same as num.shares, since I buy all)
  buyAllDogsAndActions$num.shares.after.action <- buyAllDogsAndActions$num.shares

  ####################################################### 
  # Then, the dogs I own that are still small dogs: buy or sell some
  ####################################################### 
  
  # *****TO DO: What if length = o?*****
  buyOrSell <- intersect(previousDogs$symbol,currentDogs$symbol)

  # Make a new data frame from the symbols where I may need to buy or sell. Include the current
  # current price per share, just for a sanity check calculation before selling.
  # TO DO: not sure why, but I must specify columns c("symbol","price.per.share","num.shares") else I get a NA. column.
  # NOTE: this is num.shares I currently own. I will overwrite this with the num.shares to buy or sell.
  buyOrSellDogsAndActions <- previousDogs[previousDogs$symbol %in% buyOrSell,c("symbol","price.per.share","num.shares")]
  
  # TO DO
  # hold on to num.shares I currently own in num.shares.after.action for now
  buyOrSellDogsAndActions$num.shares.after.action <- buyOrSellDogsAndActions$num.shares
  
  # Calculate num.shares to buy or sell, overwrite the current num.shares column.
  # ((current.num.shares * current.price) - target)/current.price
  # leave positive or negative for now, since I need it to derive "action"
  buyOrSellDogsAndActions$num.shares <- round(((buyOrSellDogsAndActions$num.shares*buyOrSellDogsAndActions$price.per.share) - target)/buyOrSellDogsAndActions$price.per.share)
  # TO DO clean up, comment
  # update num.shares.after.action to be the total
  # TO DO understand the negative / subtraction - ** probable reverse the math above: target - foo, not foo - target
  buyOrSellDogsAndActions$num.shares.after.action <- buyOrSellDogsAndActions$num.shares.after.action - buyOrSellDogsAndActions$num.shares
  # if negative, buy.
  # if positive, sell.
  # if 0, N/A
  # TO DO: 0 is SELL....fix that
  buyOrSellDogsAndActions$action <- ifelse(buyOrSellDogsAndActions$num.shares<0,"BUY","SELL")
  # Now remove the positive or negative
  buyOrSellDogsAndActions$num.shares <- abs(buyOrSellDogsAndActions$num.shares)
  # TO DO!!
  # include the total I will own after executing the trade action (same as num.shares, since I buy all)
  #buyOrSellDogsAndActions$num.shares.after.action <- 1

  ####################################################### 
  # Combine the results and return
  #######################################################   
  
  # combine the actions data frames
  actionsDataFrame <- rbind(sellAllDogsAndActions,buyAllDogsAndActions)
  actionsDataFrame <- rbind(actionsDataFrame,buyOrSellDogsAndActions)
  # TO DO: first column in this actions data frame has odd numbers. fix that for now with this hack.
  rownames(actionsDataFrame) <- c(1:nrow(actionsDataFrame))

  # return the uber actions data frame
  return(actionsDataFrame)
}

# calculates the number of shares I will have of each current small dog, after I execute the actions (buy or sell)
calcCurrentNumShares <- function (previousDogs,currentDogs,currentActions)
{
  # TO DO this
  # instead of below, 
  # - copy currentActions to 
  currentActionsWorkingCopy <- currentActions
  # - order foo by symbol
  currentActionsWorkingCopy <- currentActionsWorkingCopy[order(currentActionsWorkingCopy$symbol), ]
  # - remove rows where num.shares.after.action = 0
  #currentActionsWorkingCopy <- subset(currentActionsWorkingCopy, !symbol %in% unique)
  currentActionsWorkingCopy <- currentActionsWorkingCopy[!(currentActionsWorkingCopy$num.shares.after.action == 0), ]
  # - order currentDogs by symbol
  currentDogs <- currentDogs[order(currentDogs$symbol), ]
  # - put put foo$num.shares.after.action onto currentDogs$num.shares
  message("currentDogs$num.shares: ")
  message(currentDogs$num.shares)
  message("currentActionsWorkingCopy$num.shares.after.action: ")
  message(currentActionsWorkingCopy$num.shares.after.action)
  currentDogs$num.shares <- currentActionsWorkingCopy$num.shares.after.action
  # clean up column 1
  rownames(currentDogs) <- c(1:nrow(currentDogs))
  # - return currentDogs
  # (function no longer needs previousDogs)
  
  
  ## TO DO move setdiff, intersect, other to a function? 
  #
  ## first the new ("buy all") dogs
  ## *****TO DO: What if length = o?*****
  #buyAll <- setdiff(currentDogs$symbol,previousDogs$symbol)
  ## get num.shares for those new dogs from currentActions, write them in currentDogs
  #message("buyAll: ")
  #message(buyAll)
  # 
  #foo <- currentActions[currentActions$symbol %in% buyAll,c("symbol","num.shares")]
  #message("foo: ")
  #message(foo)
  #message("currentDogs: ")
  #message(currentDogs)
  
  
  
  ## then the buy or sell dogs
  ## *****TO DO: What if length = o?*****
  #buyOrSell <- intersect(previousDogs$symbol,currentDogs$symbol)
  #
  ## Nothing to do for the old ("sell all") dogs
  
  return(currentDogs)
}

main <- function(currentTotalValue)
{
  #######################################################   
  # set up
  ####################################################### 
    thisYear <- strftime(Sys.Date(),"%Y")
    lastYear <- as.character(as.integer(thisYear) - 1)
    # working directory.
    # TO DO: configurable
    # This doesn't work since Rscript has pwd as HOME
    #setwd(paste0(Sys.getenv("HOME"),"/Documents/dogs"))
    #message(paste0("home is ", Sys.getenv("HOME")))
    setwd("C:/Users/Dan/Documents/dogs")
    # working file
    # TO DO: configurable
    smallDogsWorkingFileName <- "SmallDogs.xlsx"
  
  # get last year's small dogs
  # *****TO DO******
  # separate sheets for each year, separate sheet for summary / calculations - perhaps that is the first sheet
  
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
  
  # get a quote (LAST only) for each small dog
  currentSmallDogsQuotesLast <- getLast(currentSmallDogsCharVector)
  
  # Create a clean data frame
  currentSmallDogsDataFrame <- data.frame(currentSmallDogsCharVector,currentSmallDogsQuotesLast)
  colnames(currentSmallDogsDataFrame) <- c("symbol","price.per.share")

  # figure out what to buy or sell
  actionsDataFrame <- processDogs(previousSmallDogs,currentSmallDogsDataFrame,currentTotalValue)
  
  # write the symbols and actions for the year to a separate tab
  # TO DO: move "readonly" to configuration
  if((as.character(args[2])!="readonly"))
  {
    write.xlsx(actionsDataFrame,smallDogsWorkingFileName,sheetName = paste(thisYear,"actions", sep = "-"),append = TRUE)
  }  
  
  # add num.shares to the current dogs. num.shares is the number of shares I will have AFTER I
  # execute the actions (buy or sell)
  currentSmallDogsDataFrame <- calcCurrentNumShares(previousSmallDogs,currentSmallDogsDataFrame,actionsDataFrame)
  
  # write the new (current) dogs to excel, in a new worksheet
  # TO DO clean up / remove readonly flag
  # TO DO don't do this here (?)
  # TO DO: fill in the num.shares column
  if((as.character(args[2])!="readonly"))
  {
    write.xlsx(currentSmallDogsDataFrame,smallDogsWorkingFileName,sheetName = thisYear, append = TRUE)
  }
  
  
  
  
  # TO DO
  # add num.shares to the current (2017) tab
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
