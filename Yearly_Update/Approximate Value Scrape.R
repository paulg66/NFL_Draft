library(XML)
library(RCurl)
library(httr)
library(rvest)


url <- "https://stathead.com/football/psl_finder.cgi?request=1&match=single&order_by_asc=0&order_by=av&year_min=2020&year_max=2020&positions%5B%5D=qb&positions%5B%5D=rb&positions%5B%5D=wr&positions%5B%5D=te&positions%5B%5D=e&positions%5B%5D=t&positions%5B%5D=g&positions%5B%5D=c&positions%5B%5D=ol&positions%5B%5D=dt&positions%5B%5D=de&positions%5B%5D=dl&positions%5B%5D=ilb&positions%5B%5D=olb&positions%5B%5D=lb&positions%5B%5D=cb&positions%5B%5D=s&positions%5B%5D=db&positions%5B%5D=k&positions%5B%5D=p&age_min=0&age_max=99&season_start=1&season_end=-1&undrafted=N&draft_year_min=1936&draft_year_max=2020&draft_type=B&draft_slot_min=1&draft_slot_max=500&draft_pick_in_round=pick_overall&conference=any&draft_positions%5B%5D=qb&draft_positions%5B%5D=rb&draft_positions%5B%5D=wr&draft_positions%5B%5D=te&draft_positions%5B%5D=e&draft_positions%5B%5D=t&draft_positions%5B%5D=g&draft_positions%5B%5D=c&draft_positions%5B%5D=ol&draft_positions%5B%5D=dt&draft_positions%5B%5D=de&draft_positions%5B%5D=dl&draft_positions%5B%5D=ilb&draft_positions%5B%5D=olb&draft_positions%5B%5D=lb&draft_positions%5B%5D=cb&draft_positions%5B%5D=s&draft_positions%5B%5D=db&draft_positions%5B%5D=k&draft_positions%5B%5D=p"
tabs <- getURL(url)
tabs <- readHTMLTable(tabs, stringsAsFactors = F)
data <- tabs$results

url <- "https://stathead.com/football/psl_finder.cgi?request=1&draft_slot_min=1&undrafted=N&order_by=av&draft_year_max=2020&draft_pick_in_round=pick_overall&season_start=1&order_by_asc=0&conference=any&year_min=2020&draft_slot_max=500&match=single&draft_positions[]=qb&draft_positions[]=rb&draft_positions[]=wr&draft_positions[]=te&draft_positions[]=e&draft_positions[]=t&draft_positions[]=g&draft_positions[]=c&draft_positions[]=ol&draft_positions[]=dt&draft_positions[]=de&draft_positions[]=dl&draft_positions[]=ilb&draft_positions[]=olb&draft_positions[]=lb&draft_positions[]=cb&draft_positions[]=s&draft_positions[]=db&draft_positions[]=k&draft_positions[]=p&year_max=2020&draft_year_min=1936&season_end=-1&draft_type=B&age_min=0&age_max=99&positions[]=qb&positions[]=rb&positions[]=wr&positions[]=te&positions[]=e&positions[]=t&positions[]=g&positions[]=c&positions[]=ol&positions[]=dt&positions[]=de&positions[]=dl&positions[]=ilb&positions[]=olb&positions[]=lb&positions[]=cb&positions[]=s&positions[]=db&positions[]=k&positions[]=p&offset=20"
query <- getURL(url)
query <- readHTMLTable(query, stringsAsFactors = F)
data <- rbind(data,query$results)

url <- parse_url(url)
url$query$offset <- "40"
url <- build_url(url)
query <- getURL(url)
query <- readHTMLTable(query, stringsAsFactors = F)
data <- rbind(data,query$results)

pageCount = 40

for(i in 1:500){
  pageCount <- pageCount + 20
  url <- parse_url(url)
  url$query$offset <- as.character(pageCount)
  url <- build_url(url)
  query <- getURL(url)
  query <- readHTMLTable(query, stringsAsFactors = F)
  if(length(query$results) == 0){
    break
  }
  data <- rbind(data,query$results)
}

#Flip sort, get last two pages
urlAsc <- "https://stathead.com/football/psl_finder.cgi?request=1&draft_slot_min=1&undrafted=N&order_by=av&draft_year_max=2020&draft_pick_in_round=pick_overall&season_start=1&order_by_asc=1&conference=any&year_min=2020&draft_slot_max=500&match=single&draft_positions[]=qb&draft_positions[]=rb&draft_positions[]=wr&draft_positions[]=te&draft_positions[]=e&draft_positions[]=t&draft_positions[]=g&draft_positions[]=c&draft_positions[]=ol&draft_positions[]=dt&draft_positions[]=de&draft_positions[]=dl&draft_positions[]=ilb&draft_positions[]=olb&draft_positions[]=lb&draft_positions[]=cb&draft_positions[]=s&draft_positions[]=db&draft_positions[]=k&draft_positions[]=p&year_max=2020&draft_year_min=1936&season_end=-1&draft_type=B&age_min=0&age_max=99&positions[]=qb&positions[]=rb&positions[]=wr&positions[]=te&positions[]=e&positions[]=t&positions[]=g&positions[]=c&positions[]=ol&positions[]=dt&positions[]=de&positions[]=dl&positions[]=ilb&positions[]=olb&positions[]=lb&positions[]=cb&positions[]=s&positions[]=db&positions[]=k&positions[]=p&offset=100"
urlAsc <- parse_url(urlAsc)
urlAsc$query$offset <- as.character(i*20)
urlAsc <- build_url(urlAsc)
query <- getURL(urlAsc)
query <- readHTMLTable(query, stringsAsFactors = F)
data <- rbind(data,query$results)

urlAsc <- "https://stathead.com/football/psl_finder.cgi?request=1&draft_slot_min=1&undrafted=N&order_by=av&draft_year_max=2020&draft_pick_in_round=pick_overall&season_start=1&order_by_asc=1&conference=any&year_min=2020&draft_slot_max=500&match=single&draft_positions[]=qb&draft_positions[]=rb&draft_positions[]=wr&draft_positions[]=te&draft_positions[]=e&draft_positions[]=t&draft_positions[]=g&draft_positions[]=c&draft_positions[]=ol&draft_positions[]=dt&draft_positions[]=de&draft_positions[]=dl&draft_positions[]=ilb&draft_positions[]=olb&draft_positions[]=lb&draft_positions[]=cb&draft_positions[]=s&draft_positions[]=db&draft_positions[]=k&draft_positions[]=p&year_max=2020&draft_year_min=1936&season_end=-1&draft_type=B&age_min=0&age_max=99&positions[]=qb&positions[]=rb&positions[]=wr&positions[]=te&positions[]=e&positions[]=t&positions[]=g&positions[]=c&positions[]=ol&positions[]=dt&positions[]=de&positions[]=dl&positions[]=ilb&positions[]=olb&positions[]=lb&positions[]=cb&positions[]=s&positions[]=db&positions[]=k&positions[]=p&offset=100"
urlAsc <- parse_url(urlAsc)
urlAsc$query$offset <- as.character(i*20+20)
urlAsc <- build_url(urlAsc)
query <- getURL(urlAsc)
query <- readHTMLTable(query, stringsAsFactors = F)
data <- rbind(data,query$results)

data <- unique(data)
data <- data[!(is.na(data$Player) | data$Player==""), ]#Remove Blank Rows

#Join to PY data
data_PY <- read.csv("PlayerData_PY.csv")
nflData <- rbind(data,data_PY)
nflData <- nflData[nflData$Rk != "Rk",] #remove extra title rows
write.csv(nflData,"PlayerData.csv",row.names = FALSE)


