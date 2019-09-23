library(XML)
library(RCurl)
url <- "https://www.pro-football-reference.com/play-index/psl_finder.cgi?request=1&match=single&year_min=2018&year_max=2018&season_start=1&season_end=-1&pos%5B%5D=qb&pos%5B%5D=rb&pos%5B%5D=wr&pos%5B%5D=te&pos%5B%5D=e&pos%5B%5D=t&pos%5B%5D=g&pos%5B%5D=c&pos%5B%5D=ol&pos%5B%5D=dt&pos%5B%5D=de&pos%5B%5D=dl&pos%5B%5D=ilb&pos%5B%5D=olb&pos%5B%5D=lb&pos%5B%5D=cb&pos%5B%5D=s&pos%5B%5D=db&pos%5B%5D=k&pos%5B%5D=p&draft_year_min=1936&draft_year_max=2019&draft_slot_min=1&draft_slot_max=500&draft_pick_in_round=pick_overall&conference=any&draft_pos%5B%5D=qb&draft_pos%5B%5D=rb&draft_pos%5B%5D=wr&draft_pos%5B%5D=te&draft_pos%5B%5D=e&draft_pos%5B%5D=t&draft_pos%5B%5D=g&draft_pos%5B%5D=c&draft_pos%5B%5D=ol&draft_pos%5B%5D=dt&draft_pos%5B%5D=de&draft_pos%5B%5D=dl&draft_pos%5B%5D=ilb&draft_pos%5B%5D=olb&draft_pos%5B%5D=lb&draft_pos%5B%5D=cb&draft_pos%5B%5D=s&draft_pos%5B%5D=db&draft_pos%5B%5D=k&draft_pos%5B%5D=p&c5val=1.0&order_by=av"
tabs <- getURL(url)
tabs <- readHTMLTable(tabs, stringsAsFactors = F)
data <- tabs$results

url <- "https://www.pro-football-reference.com/play-index/psl_finder.cgi?request=1&match=single&year_min=2018&year_max=2018&season_start=1&season_end=1&age_min=0&age_max=99&league_id=&team_id=&is_active=&is_hof=&pos_is_qb=Y&pos_is_rb=Y&pos_is_wr=Y&pos_is_te=Y&pos_is_e=Y&pos_is_t=Y&pos_is_g=Y&pos_is_c=Y&pos_is_ol=Y&pos_is_dt=Y&pos_is_de=Y&pos_is_dl=Y&pos_is_ilb=Y&pos_is_olb=Y&pos_is_lb=Y&pos_is_cb=Y&pos_is_s=Y&pos_is_db=Y&pos_is_k=Y&pos_is_p=Y&c1stat=&c1comp=gt&c1val=&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&c5comp=&c5gtlt=lt&c6mult=1.0&c6comp=&order_by=av&draft=0&draft_year_min=1936&draft_year_max=2015&type=&draft_round_min=0&draft_round_max=99&draft_slot_min=1&draft_slot_max=500&draft_pick_in_round=0&draft_league_id=&draft_team_id=&college_id=all&conference=any&draft_pos_is_qb=Y&draft_pos_is_rb=Y&draft_pos_is_wr=Y&draft_pos_is_te=Y&draft_pos_is_e=Y&draft_pos_is_t=Y&draft_pos_is_g=Y&draft_pos_is_c=Y&draft_pos_is_ol=Y&draft_pos_is_dt=Y&draft_pos_is_de=Y&draft_pos_is_dl=Y&draft_pos_is_ilb=Y&draft_pos_is_olb=Y&draft_pos_is_lb=Y&draft_pos_is_cb=Y&draft_pos_is_s=Y&draft_pos_is_db=Y&draft_pos_is_k=Y&draft_pos_is_p=Y&offset=100"
query <- getURL(url)
query <- readHTMLTable(query, stringsAsFactors = F)
data <- rbind(data,query$results)

url <- parse_url(url)
url$query$offset <- "200"
url <- build_url(url)
query <- getURL(url)
query <- readHTMLTable(query, stringsAsFactors = F)
data <- rbind(data,query$results)

pageCount = 200

' run upto 400'
  pageCount <- pageCount + 100
  url <- parse_url(url)
  url$query$offset <- as.character(pageCount)
  url <- build_url(url)
  query <- getURL(url)
  query <- readHTMLTable(query, stringsAsFactors = F)
  data <- rbind(data,query$results)

  url <- "https://www.pro-football-reference.com/play-index/psl_finder.cgi?request=1&match=single&year_min=2018&year_max=2018&season_start=1&season_end=-1&pos%5B%5D=qb&pos%5B%5D=rb&pos%5B%5D=wr&pos%5B%5D=te&pos%5B%5D=e&pos%5B%5D=t&pos%5B%5D=g&pos%5B%5D=c&pos%5B%5D=ol&pos%5B%5D=dt&pos%5B%5D=de&pos%5B%5D=dl&pos%5B%5D=ilb&pos%5B%5D=olb&pos%5B%5D=lb&pos%5B%5D=cb&pos%5B%5D=s&pos%5B%5D=db&pos%5B%5D=k&pos%5B%5D=p&draft_year_min=1936&draft_year_max=2019&draft_slot_min=1&draft_slot_max=500&draft_pick_in_round=pick_overall&conference=any&draft_pos%5B%5D=qb&draft_pos%5B%5D=rb&draft_pos%5B%5D=wr&draft_pos%5B%5D=te&draft_pos%5B%5D=e&draft_pos%5B%5D=t&draft_pos%5B%5D=g&draft_pos%5B%5D=c&draft_pos%5B%5D=ol&draft_pos%5B%5D=dt&draft_pos%5B%5D=de&draft_pos%5B%5D=dl&draft_pos%5B%5D=ilb&draft_pos%5B%5D=olb&draft_pos%5B%5D=lb&draft_pos%5B%5D=cb&draft_pos%5B%5D=s&draft_pos%5B%5D=db&draft_pos%5B%5D=k&draft_pos%5B%5D=p&c5val=1.0&order_by=av&offset=500"
  pageCount <- pageCount + 100
  url <- parse_url(url)
  url$query$offset <- as.character(pageCount)
  url <- build_url(url)
  query <- getURL(url)
  query <- readHTMLTable(query, stringsAsFactors = F)
  data <- rbind(data,query$results)
