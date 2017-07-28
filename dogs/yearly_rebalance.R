##############################################################################################
# Figures out what to do for the yearly "rebalance" of my "small dogs of the dow" investments.
# (and gives me an excuse to play with R)
# 
# Usage: Rscript yearly_rebalance.R
##############################################################################################

# get required input
args <- commandArgs(TRUE)
if (length(args) != 1)
{
  print("Usage: ")
  print("       Rscript yearly_rebalance.R <TotalValueOfDogsToday>")
} else {

  currentTotalValue <- args[1]
  
# Load packages.
# TO DO: Not working right from bash / Rscript
# TO DO: bash / Rscript is loading every time, with status message. Why?
# TO DO: what does "require" do? see http://www.r-fiddle.org/#/ after running demo, look at their code
# TO DO: drop out of the whole script if the package is not installed. Perhaps like this: http://mazamascience.com/WorkingWithData/?p=912
pkgs <- c("XML","quantmod")
for(pkg in pkgs)
{
  if(!require(pkg, character.only = TRUE))
  {
    missingPackageMessage <- paste0("Package ", pkg, " is not installed. Install and try again.")
    warning(missingPackageMessage)
  }
}

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

# TBD: remove this
print("DONE")

# end arg check 'else'
}
