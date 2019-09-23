library(XML)
library(RCurl)

urlPrefix <- "https://www.pro-football-reference.com/years/"
urlEnd<- "/draft.htm"
year <- 1993

draftData <- NULL

#run loop from 1994 to 2018
for (i in 1:29){
  year <- year + 1
  url <- paste(urlPrefix,as.character(year),urlEnd,sep = '') #Build URL with increased year
  query <- getURL(url)
  query <- readHTMLTable(query, stringsAsFactors = F) #pull data
  tempDraftData <- query$drafts
  tempDraftData$DraftYear <- year #add draft year
  draftData <- rbind(draftData,tempDraftData) #rbind to main dataset
}

draftData <- draftData[draftData$Rnd != "Rnd",] #remove title rows
draftData$Player <- str_trim(draftData$Player,"right") #remove spaces at end of names