# TO DO some appropriate header description

# TO DO consider this rm thing 
# rm(list=ls(all=TRUE)) 

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
