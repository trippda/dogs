##############################################################################################
# Figures out what to do for the yearly "rebalance" of my "small dogs of the dow" investments.
# (and gives me an excuse to play with R)
# 
# Usage: Rscript yearly_rebalance.R <TotalValueOfDogsToday>
##############################################################################################

#######################################  
# TO DO
#######################################
# handle / test multiple years
# organize into functions, called from main
# get TotalValueOfDogsToday from Ameritrade. Api access is very expensive, but this article says I can use httr:
#   https://stackoverflow.com/questions/10692066/how-to-webscrape-secured-pages-in-r-https-links-using-readhtmltable-from-xml

checkForPackages <- function()
{
  # Load packages.
  bPackagesFound <- TRUE
  pkgs <- c("XML","quantmod","xlsx","futile.logger","config")
  for(pkg in pkgs)
  {
    if(!suppressPackageStartupMessages(require(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)))
    {
      if(bPackagesFound)
      {
        bPackagesFound <- FALSE
      }
      flog.fatal("***Package %s is not installed. Install and try again.***",pkg)
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
processDogs <- function(previousDogs,currentDogs,portfolioValue,config)
{
  # calculate target value for each small dog: total value of the portfolio today divided by 5 
  target <- as.numeric(portfolioValue)/5
  
  # Get current price.per.share for previousDogs
  previousDogs$price.per.share <- getLast(as.character(previousDogs$symbol))

  ####################################################### 
  # First, the dogs I own that are no longer small dogs: sell all I have
  ####################################################### 
  
  # if length greater than 0, these are previous dogs. I got a current quote above.
  # if length = 0, small dogs remained the same.
  sellAll <- setdiff(previousDogs$symbol,currentDogs$symbol)
  flog.debug("length(sellAll): %s",length(sellAll))
  if (length(sellAll)>0)
  {
    # create a new data frame of sellAll dogs symbols, number of shares I own, and SELL action
    # include the current price per share, just for a sanity check calculation before selling.
    sellAllDogsAndActions <- previousDogs[previousDogs$symbol %in% sellAll,c("symbol","price.per.share","num.shares")]
    sellAllDogsAndActions$action <- "SELL"
    # include the total I will own after executing the trade action (0, since I sell all)
    sellAllDogsAndActions$num.shares.after.action <- 0  
  }

  ####################################################### 
  # Next, the new small dogs--ones I do not own any of yet: buy all (target value = 1/5 portfolio value)
  #######################################################  

  # if length greater than 0, these are current dogs. I already have the quote
  # if length = 0, small dogs remained the same.
  buyAll <- setdiff(currentDogs$symbol,previousDogs$symbol) 
  flog.debug("length(buyAll): %s", length(buyAll))
  if(length(buyAll)>0)
  {
    # make a new data frame from the new ("buy all") symbols and quotes
    # include the current price per share, just for a sanity check calculation before selling.
    buyAllDogsAndActions <- currentDogs[currentDogs$symbol %in% buyAll, ] 
    
    # add column indicating number of shares to buy for each stock
    # number of shares to buy is 1/5 the portfolio value ("target") / current price, rounded
    buyAllDogsAndActions$num.shares <- round(target/buyAllDogsAndActions$price.per.share)
    buyAllDogsAndActions$action <- "BUY"
    # include the total I will own after executing the trade action (same as num.shares, since I buy all)
    buyAllDogsAndActions$num.shares.after.action <- buyAllDogsAndActions$num.shares 
  }

  ####################################################### 
  # Then, the dogs I own that are still small dogs: buy or sell some
  ####################################################### 
  
  # if length > 0, these are dogs that I own that are still small dogs.
  # if length = 0, all small dogs have changed this year -- very unlikely, but possible.
  buyOrSell <- intersect(previousDogs$symbol,currentDogs$symbol)
  flog.debug("length(buyOrSell): %s",length(buyOrSell))
  if(length(buyOrSell)>0)
  {
    # Make a new data frame from the symbols where I may need to buy or sell. Include the current
    # current price per share, just for a sanity check calculation before selling.
    # TO DO: not sure why, but I must specify columns c("symbol","price.per.share","num.shares") else I get a NA. column.
    # NOTE: this is num.shares I currently own. I will overwrite this with the num.shares to buy or sell.
    buyOrSellDogsAndActions <- previousDogs[previousDogs$symbol %in% buyOrSell,c("symbol","price.per.share","num.shares")]
    
    # hold on to num.shares I currently own in num.shares.after.action for now
    buyOrSellDogsAndActions$num.shares.after.action <- buyOrSellDogsAndActions$num.shares
    
    # Calculate num.shares to buy or sell, overwrite the current num.shares column.
    # ((current.num.shares * current.price) - target)/current.price
    # Leave positive / negative sign for now.
    buyOrSellDogsAndActions$num.shares <- round(((buyOrSellDogsAndActions$num.shares*buyOrSellDogsAndActions$price.per.share) - target)/buyOrSellDogsAndActions$price.per.share)
    
    # update num.shares.after.action to be the total
    buyOrSellDogsAndActions$num.shares.after.action <- buyOrSellDogsAndActions$num.shares.after.action - buyOrSellDogsAndActions$num.shares
    
    # set the action based on num.share negative or positive.
    # if negative, buy.
    # if positive, sell.
    # if 0, N/A
    # TO DO: 0 is SELL....fix that
    buyOrSellDogsAndActions$action <- ifelse(buyOrSellDogsAndActions$num.shares<0,"BUY","SELL")
    
    # Now remove the positive or negative
    buyOrSellDogsAndActions$num.shares <- abs(buyOrSellDogsAndActions$num.shares)
  }

  ####################################################### 
  # Combine the results and return
  #######################################################   
  
  # combine the actions data frames
  
  if (length(buyOrSell) == config$totalNumSmallDogs)
  {
    # All small dogs stayed the same. 
    # Means both length(sellAll) == 0 and length(buyAll) == 0
    actionsDataFrame <- buyOrSellDogsAndActions
  }
  if (length(sellAll)>0 && length(sellAll) != config$totalNumSmallDogs)
  {
    # Some (not all) dogs I own are no longer small dogs--the most common case.
    # Means both length(buyAll) and length(buyOrSell) are also not 0.
    actionsDataFrame <- rbind(sellAllDogsAndActions,buyAllDogsAndActions)
    actionsDataFrame <- rbind(actionsDataFrame,buyOrSellDogsAndActions)
  }
  if (length(sellAll) == config$totalNumSmallDogs)
  {
    # All dogs I own are no longer small dogs.
    # Means length(buyAll) is also 5 and length(buyOrSell) == 0
    actionsDataFrame <- rbind(sellAllDogsAndActions,buyAllDogsAndActions)
  }

  # TO DO: first column in this actions data frame has odd numbers. 
  # fix that for now with this hack.
  rownames(actionsDataFrame) <- c(1:nrow(actionsDataFrame))

  # return the uber actions data frame
  return(actionsDataFrame)
}

# "calculates" (actually just moves to currentDogs) the number of shares I will have of each current small dog, after I execute the actions (buy or sell)
calcCurrentNumShares <- function (currentDogs,currentActions)
{
  # Loop through the symbols in currentDogs. Put the corresponding num.shares.after.action from currentActions in num.shares.
  for (s in currentDogs$symbol)
  {
    flog.debug("s is: %s", s)
    flog.debug("currentActions[currentActions$symbol==s,\"num.shares.after.action\"] is: %s",currentActions[currentActions$symbol==s,"num.shares.after.action"])
    currentDogs[currentDogs$symbol==s,"num.shares"] <- currentActions[currentActions$symbol==s,"num.shares.after.action"]    
  }

  return(currentDogs)
}

# adds the current year summary stats to the summarySheet datafile
updateSummary <- function(summarySheet, currentTotalValue, previousDogs, lastYear, thisYear, config)
{

  ####################################################### 
  # portfolio total (start): the value of the portfolio at the start of last year
  ####################################################### 
  
  # price.per.share * num.shares for each item
  portfolioStartValues <- previousDogs$price.per.share * previousDogs$num.shares
  flog.debug("portfolioStartValues: ", portfolioStartValues, capture = TRUE)
  
  # add those products
  portfolioStartSum <- sum(portfolioStartValues,na.rm = TRUE)
  flog.debug("portfolioSum: %s", portfolioStartSum)
  
  ####################################################### 
  # portfolio average (start): 
  #######################################################
  
  # portfolio sum / 5
  portfolioAverage <- portfolioStartSum / config$totalNumSmallDogs
  
  ####################################################### 
  # previous year total (end): the value of the portfolio on the day I rebalance, BEFORE I rebalance)
  #######################################################
  
  # currentTotalValue, passed in from cmd line
  currentTotalValue <- as.numeric(currentTotalValue)
  
  ####################################################### 
  # previous year average (end), this year target
  #######################################################
  currentAverage <- currentTotalValue / config$totalNumSmallDogs

  ####################################################### 
  # previous year average gain / loss (percent)
  #######################################################
  percentGainOrLoss <- ((currentTotalValue - portfolioStartSum)/currentTotalValue)*100
  
  ####################################################### 
  # assemble into a data frame, add to summary sheet
  ####################################################### 
  
  # description column
  descriptionColumn <- c(paste0(lastYear," total (start)"),paste0(lastYear," average (start)"),paste0(lastYear," total (end)"),paste0(lastYear," average (end), ",thisYear, " target"),paste0(lastYear," gain/loss (%)"))

  # value column
  valueColumn <- c(round(portfolioStartSum,2),round(portfolioAverage,2),round(currentTotalValue,2),round(currentAverage,2),round(percentGainOrLoss,2))
  
  # combine into a data frame
  yearlySummary <- data.frame(descriptionColumn,valueColumn)
  
  # column names
  names(yearlySummary) <- c("Description","Value")
  flog.debug("yearlySummary: ",yearlySummary, capture = TRUE)
  
  # add to summarySheet
  flog.debug("summarySheet before rbind: ",summarySheet, capture = TRUE)
  summarySheet <- rbind(yearlySummary,summarySheet)
  flog.debug("summarySheet after rbind: ",summarySheet, capture = TRUE)
  
  return(summarySheet)
}

# Writes a data frame to a spreadsheet, with some formatting.
# TO DO: instead of passing in the vector of integers repesenting the columns to resize, pass in a vector of column header strings--that
# lets me add/remove columns without breaking.
writeFormattedWorkSheet <- function(bNewWorkSheet, sNewWorkSheetName, iOldWorkSheetIndex, bBoldHeaders, bAutosizeColumns, vAutosizeColumns, dataFrameToWrite, excelFileName)
  {
    workbook <- loadWorkbook(excelFileName)
    # TO DO: figure out what setForceFormulaRecalculation does and if I need it. For now it seems I do not.
    #workbook$setForceFormulaRecalculation(TRUE)
    if(bNewWorkSheet)
    {
      s <- createSheet(workbook, sheetName=sNewWorkSheetName)
    } else
    {
      s <- getSheets(workbook)[[iOldWorkSheetIndex]]
    }
    
    headerStyle <- CellStyle(workbook) + Font(workbook, isBold=bBoldHeaders) 
    addDataFrame(dataFrameToWrite, sheet=s, row.names = FALSE, showNA = FALSE, colnamesStyle = headerStyle)
    if (bAutosizeColumns) {
      for (columnIndex in vAutosizeColumns) {
        autoSizeColumn(s,columnIndex)
      }
    }
    saveWorkbook(workbook, file = excelFileName)

}

main <- function(currentTotalValue)
{
  #######################################################   
  # set up
  ####################################################### 
  
  # read config
  config <- config::get(file = ".\\yearly_rebalance_config.yml", use_parent = FALSE)

  # logging
  #   level
  flog.threshold(config$debugLevel)
  #   directory for logging AND excel output / input
  setwd(config$workingDirectory)
  #   log file name
  flog.appender(appender.file(config$logFileName))
  #TO DO: figure out why f (function) here does not work.
  #layout <- layout.format('~l [~t] [f:~f] ~m')
  #flog.layout(layout)
  flog.info("debug level: %s",config$debugLevel)

  thisYear <- strftime(Sys.Date(),"%Y")
  lastYear <- as.character(as.integer(thisYear) - 1)
  flog.debug("This year is: %s",thisYear)
  flog.debug("Last year is: %s",lastYear)

  # working file, summary sheet
  smallDogsWorkingFileName <- config$excelFileName
  summarySheetName <- config$summarySheetName
  flog.info("working directory: %s",getwd())
  flog.info("working file (excel): %s",smallDogsWorkingFileName)
  flog.debug("summary sheet name: %s",summarySheetName)

  #######################################################   
  # get last year's small dogs
  #######################################################   
  
  previousSmallDogs <- read.xlsx(smallDogsWorkingFileName,sheetName = lastYear)
  flog.debug("previousSmallDogs from excel file is: ", previousSmallDogs, capture = TRUE)
  
  #######################################################   
  # get this year's small dogs
  #######################################################   
  
  ##2019 hack: http://www.dogsofthedow.com/ did not post a 2019 dogs page. 
  ##I am manually taking small dogs from https://www.money-zine.com/investing/stocks/dogs-of-the-dow-2019/
  ##and populating currentSmallDogsCharVector

  
    ## URL to scrape (current year)
    #currentDogsURL <- paste0(config$dogsSite,thisYear,config$dogsPageNameSuffix)
    #flog.debug("currentDogsURL is: %s", currentDogsURL)
    #
    ## get the page. 
    ## fyi I read that useInternalNodes = TRUE is ignored for HTML docs, but readHTMLTable below fails without it.
    #currentDogs.html <- htmlTreeParse(currentDogsURL , useInternalNodes = TRUE)
    #
    ## parse, just to figure out which table. Leave commented for run time.
    ## tables <- readHTMLTable(currentDogs.html,as.data.frame = FALSE)
    #
    ## get table #12 as a dataframe
    ## (skip the row below the header row - it is meta info I do not want/need
    #tables <- readHTMLTable(currentDogs.html,skip.rows = 2,stringsAsFactors=FALSE, which = config$dogsTableIndex)
    #
    ## select only the small dogs.
    ## spaces in column names are problematic, so get rid of those first
    #names(tables) <-gsub(" ",".",names(tables))
    #currentSmallDogs <- subset(tables, Small.Dog == "Yes", Symbol)
    #
    ########################################################   
    ## get a quote for each current small dog
    ########################################################   
    #
    ## convert currentSmallDogs dataframe into a character vector 
    #currentSmallDogsCharVector <- currentSmallDogs[['Symbol']]
    
  
  ##2019 hack: http://www.dogsofthedow.com/ did not post a 2019 dogs page. 
  ##I am manually taking small dogs from https://www.money-zine.com/investing/stocks/dogs-of-the-dow-2019/
  ##and populating currentSmallDogsCharVector
  currentSmallDogsCharVector <- c("XOM","VZ","KO","PFE","CSCO")
  
  # get a quote (LAST only) for each small dog
  currentSmallDogsQuotesLast <- getLast(currentSmallDogsCharVector)
  
  # Create a clean data frame
  currentSmallDogsDataFrame <- data.frame(currentSmallDogsCharVector,currentSmallDogsQuotesLast)
  colnames(currentSmallDogsDataFrame) <- c("symbol","price.per.share")

  #######################################################   
  # figure out what to buy or sell
  #######################################################   
  
  actionsDataFrame <- processDogs(previousSmallDogs,currentSmallDogsDataFrame,currentTotalValue,config)
  
  #######################################################   
  # set up for next year: currentDogs will be previousDogs
  #######################################################   
  
  # add num.shares to the current dogs. num.shares is the number of shares I will have AFTER I
  # execute the actions (buy or sell)
  currentSmallDogsDataFrame <- calcCurrentNumShares(currentSmallDogsDataFrame,actionsDataFrame)
  
  ####################################################### 
  # update the summary table
  ####################################################### 
  
  # get the current summary
  summarySheet <- read.xlsx(smallDogsWorkingFileName, sheetName = summarySheetName)
  flog.debug("initially read summary: ",summarySheet, capture = TRUE)
  # delete/drop the NA. column TO DO: some better way to do this (avoid the NA. column in the first place)
  summarySheet$NA. <- NULL
  flog.debug("summary after dropping NA. column: ", summarySheet, capture = TRUE)
  
  # update with this year's data
  summarySheet <- updateSummary(summarySheet, currentTotalValue, previousSmallDogs, lastYear, thisYear, config)
  
  ####################################################### 
  # Record all of this in excel
  ####################################################### 

  # Update the existing summary sheet
  writeFormattedWorkSheet(FALSE,NULL,1,TRUE,TRUE,c(1,2),summarySheet,smallDogsWorkingFileName)

  # write the symbols and actions for the year in a new "actions" worksheet
  writeFormattedWorkSheet(TRUE,paste(thisYear,"actions", sep = "-"),NULL,TRUE,TRUE,c(1:6),actionsDataFrame,smallDogsWorkingFileName)
  
  # write the new (current) dogs in a new worksheet
  writeFormattedWorkSheet(TRUE,thisYear,NULL,TRUE,TRUE,c(1:4),currentSmallDogsDataFrame,smallDogsWorkingFileName)
  
  ####################################################### 
  # "all done" message
  #######################################################
  
  message(paste0("Done. See ",smallDogsWorkingFileName, " for buy/sell actions required and yearly summary.")) 
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
  }
}
