library(stringi)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(pgirmess)

#setwd("C:/Users/Paul/Documents/R Projects/NFL/NFL Draft")

#--------------Data Load-------------------------------
data <- read.csv("PlayerData.csv",stringsAsFactors = FALSE)
draftData <- read.csv("draftData.csv",stringsAsFactors = FALSE)
draftClassifer <- read.csv("Draft_Classifier.csv")
nflLogos <- read.csv("NFLLogos.csv",stringsAsFactors = FALSE)

#Load from nflverse
trades <- read.csv("https://raw.githubusercontent.com/nflverse/nfldata/master/data/trades.csv")
draftDataPFR <- read.csv("https://raw.githubusercontent.com/nflverse/nfldata/master/data/draft_picks.csv")
#rosters <- fast_scraper_roster(2000:2020) #Run once outside of script per year
rosters <- rostersBackup
rostersBackup <- rosters

#--------------Initial Cleaning-------------------------
nflData <- data
nflData[is.na(nflData$AV),"AV"] <- 0 #Update AV's logged as NA to 0
nflData <- nflData[nflData$Rk != "Rk",] #remove extra title rows
nflData$Player <- stri_replace_all_regex(nflData$Player,"[*]","")

#Remove II, III, Jr. from player names
nflData$Player <- gsub("III","",nflData$Player)
nflData$Player <- gsub("II","",nflData$Player)
nflData$Player <- gsub("Jr.","",nflData$Player)
nflData$Player <- gsub("Jr","",nflData$Player)
nflData$Player <- str_trim(nflData$Player,"right")

draftData$Player  <- gsub("III","",draftData$Player )
draftData$Player  <- gsub("II","",draftData$Player )
draftData$Player  <- gsub("Jr.","",draftData$Player )
draftData$Player  <- gsub("Jr","",draftData$Player )
draftData$Player  <- str_trim(draftData$Player ,"right")

draftData <- draftData[which(draftData$DraftYear >= 2001),]
draftData$Player <- stri_replace_all_regex(draftData$Player,"HOF","")
draftData$Player <- str_trim(draftData$Player,"right")

#Add Tom Brady
TB12 <- data.frame(6,199,"NWE","Tom Brady","QB",23,2021,3,14,20,182,174,313,7125,11106,82975,615,201,655,1095,26,3,65,0,0,0,0,"Michigan","College Stats",2000)
names(TB12) <- names(draftData)
draftData <- rbind(draftData,TB12)

#Fix Josh Allen Duplicate
nflData[which(nflData$Player == "Josh Allen" & nflData$Tm == "JAX"),c("Player")]<- "Josh Allen."
draftData[which(draftData$Player == "Josh Allen" & draftData$Tm == "JAX"),c("Player")] <- "Josh Allen."

#Create Unique PlayerID
nflData$PlayerID <- paste(nflData$Player,nflData$Draft, sep = "")
draftData$PlayerID <- paste(draftData$Player,draftData$Rnd, "-",draftData$Pick, sep="")

#Remove Duplicates
nflData <- nflData[!duplicated(nflData[,c("Year","PlayerID")]),]

#Create PFR ID Lookup Table
pfrLookup <- draftDataPFR[,c("season","round","pick","pfr_id")]
names(pfrLookup) <- c("DraftYear","Rnd","Pick","pfr_id")

#-------------Merge Draft & NFL Data----------------------------
#Prep Files for Merging
draftYrLookup <- draftData[c("PlayerID","DraftYear","Tm","Pos","Rnd","Pick","Player")] ##create lookup table for draft years of players
names(draftYrLookup) <- c("PlayerID","DraftYear","DraftTm","Pos","Rnd","Pick","PlayerName")
PlayerNameLookup <- draftYrLookup[,c("PlayerID","PlayerName")]

##Add Draft Year to Season Data
nflData<- merge(nflData,draftYrLookup, by = "PlayerID", all.y = TRUE)

#When player change teams mid-season, change tm to drafttm
nflData$Tm <- ifelse(nflData$Tm == '2TM',nflData$DraftTm,nflData$Tm)
nflData$Tm <- ifelse(nflData$Tm == '3TM',nflData$DraftTm,nflData$Tm)
nflData$Tm <- ifelse(nflData$Tm == '4TM',nflData$DraftTm,nflData$Tm)

#Find first season for each player
nflData$Year <- as.numeric(nflData$Year)
firstYr <- aggregate(Year~PlayerID,data = nflData, min)
names(firstYr) <- c("PlayerID","FirstYear")
nflData <- merge(nflData,firstYr,by = "PlayerID",all.x = TRUE)
nflData$DraftYear <- ifelse(is.na(nflData$DraftYear),nflData$FirstYear,nflData$DraftYear)
nflData <- nflData[nflData$DraftYear >= 2000,]

#convert colums to numeric
nflData$Year <- as.numeric(nflData$Year)
nflData$DraftYear <- as.numeric(nflData$DraftYear)
nflData$AV <- as.numeric(nflData$AV)
nflData$G <- as.numeric(nflData$G)
nflData$GS <- as.numeric(nflData$GS)

#Calculate Season after being drafted
nflData$Season <- nflData$Year - nflData$DraftYear + 1



#--------------Data Standardization & Ad-hoc Updates----
#Standardize Positions
nflData$Pos <- stri_replace_all_regex(nflData$Pos,"S","DB")
nflData$Pos <- stri_replace_all_regex(nflData$Pos,"CB","DB")
nflData$Pos <- stri_replace_all_regex(nflData$Pos,"DL","DT")
nflData$Pos <- stri_replace_all_regex(nflData$Pos,"NT","DT")
nflData$Pos <- stri_replace_all_regex(nflData$Pos,"DE","EDGE")
nflData$Pos <- stri_replace_all_regex(nflData$Pos,"OLB","EDGE")
nflData$Pos <- stri_replace_all_regex(nflData$Pos,"ILB","LB")
nflData$Pos <- stri_replace_all_regex(nflData$Pos,"OL","G")

#Update Teams for Eli/Rivers Trade
nflData[which(nflData$PlayerID == "Eli Manning1-1"),c("DraftTm")] <- "NYG"
nflData[which(nflData$PlayerID == "Eli Manning1-1"),c("Tm")] <- "NYG"
nflData[which(nflData$PlayerID == "Philip Rivers1-4"),c("DraftTm")] <- "LAC"
nflData[which(nflData$PlayerID == "Philip Rivers1-4"),c("Tm")] <- "LAC"

##Fix Relocated Teams
nflData$DraftTm <- stri_replace_all_regex(nflData$DraftTm ,"STL","LAR")
nflData$DraftTm <- stri_replace_all_regex(nflData$DraftTm ,"SDG","LAC")
nflData$DraftTm <- stri_replace_all_regex(nflData$DraftTm ,"OAK","LVR")
nflData$Tm <- stri_replace_all_regex(nflData$Tm ,"STL","LAR")
nflData$Tm <- stri_replace_all_regex(nflData$Tm ,"SDG","LAC")
nflData$Tm <- stri_replace_all_regex(nflData$Tm ,"OAK","LVR")

#--------------AV Calculations Start--------------------------
#Fix years for drafted players who never played
nflData$Season <- ifelse(is.na(nflData$Season),-1,nflData$Season)
nflData$FirstYear <- ifelse(is.na(nflData$FirstYear),nflData$DraftYear,nflData$FirstYear)
nflData$Year <- ifelse(is.na(nflData$Year),nflData$DraftYear,nflData$Year)
nflData$AV <- ifelse(is.na(nflData$AV),0,nflData$AV)
nflData$Tm <- ifelse(is.na(nflData$Tm),nflData$DraftTm,nflData$Tm)

#Add logo URL for output graphs
nflData <- merge(nflData,nflLogos[,c("team_code","url")],by.x='Tm', by.y='team_code',all.x = TRUE)

#Find average AV during rookie deal
rookieDealAV <- nflData %>% filter(Season < 5) %>% group_by(PlayerID,Player,DraftYear,DraftTm,Pos,Rnd,Pick) %>% 
  dplyr::summarize(RookieAV = sum(AV, na.rm = TRUE),G = sum(G,na.rm = TRUE), GS = sum(GS, na.rm = TRUE),
                   AvgAV = mean(AV, na.rm = TRUE),LastSeason = max(Year, na.rm = TRUE),Seasons = dplyr::n())
rookieDealAV <-merge(rookieDealAV,PlayerNameLookup, by = "PlayerID")
rookieDealAV$Player <- rookieDealAV$PlayerName
rookieDealAV <- rookieDealAV[complete.cases(rookieDealAV),]
rookieDealAV <- merge(draftClassifer,rookieDealAV, by = "Pick",all = TRUE)
rookieDealAV$Classifier <- factor(rookieDealAV$Classifier, levels = (c("Top 5", "6-15", "16-32", "33-48","49-64","3","4","5","6","7")))
rookieDealAV <- rookieDealAV[complete.cases(rookieDealAV),]

#Remove players who were cut before playing a game for their draft team
cutPlayers <- nflData[which(nflData$Tm != nflData$DraftTm & nflData$FirstYear == nflData$Year),]$PlayerID
rookieDealAV[rookieDealAV$PlayerID %in% cutPlayers,]$RookieAV <- 0

##Calculate Years since Drafted
thisYear <- as.integer(format(Sys.Date(), "%Y"))
rookieDealAV$YearsSinceDrafted <- thisYear - rookieDealAV$DraftYear 
rookieDealAV$YearsSinceDrafted[rookieDealAV$YearsSinceDrafted >= 4] <- -1

#Add PFR ID
rookieDealAV <- merge(rookieDealAV,pfrLookup,by = c("DraftYear","Rnd","Pick"))


#--------------Bonus AV and AV Extrapolation-------------------
#Add years with draft team after rookiedeal
BonusAV <- nflData %>% filter(Season > 4,Season <= 8,DraftTm == Tm) %>% group_by(PlayerID,DraftTm,Tm) %>% 
  dplyr::summarize(BonusAV = sum(AV, na.rm = TRUE),Bonus_Seasons = max(Season,na.rm = TRUE)-4,
                   Last_Year_Bonus = max(Year,na.rm = TRUE))
BonusAV <- BonusAV[,c("PlayerID","BonusAV","Bonus_Seasons","Last_Year_Bonus")]
rookieDealAV <- merge(rookieDealAV,BonusAV,all.x = TRUE)
rookieDealAV$BonusAV[is.na(rookieDealAV$BonusAV)] <- 0 #Replace NAs with 0
rookieDealAV$Bonus_Seasons[is.na(rookieDealAV$Bonus_Seasons)] <- 0 #Replace NAs with 0
rookieDealAV$Last_Year_Bonus[is.na(rookieDealAV$Last_Year_Bonus)] <- 0 #Replace NAs with 0

#Add Bonus AV, Exprolate Players in years 5-8
rookieDealAV$BonusAV <- ifelse(rookieDealAV$Bonus_Seasons > 0 & rookieDealAV$Bonus_Seasons < 4 & rookieDealAV$Last_Year_Bonus == (thisYear-1),
                               rookieDealAV$BonusAV + ((rookieDealAV$RookieAV + rookieDealAV$BonusAV)/(rookieDealAV$Bonus_Seasons+4))*(4-rookieDealAV$Bonus_Seasons),
                               rookieDealAV$BonusAV)
rookieDealAV$RookieAV <- rookieDealAV$RookieAV + rookieDealAV$BonusAV/2 #Weight extra seasons at 50%

##Extrapolate totals for players drafted <= 4 seasons ago and non-rookies
rookieDealAV$RookieAV <- ifelse(rookieDealAV$YearsSinceDrafted == 1 ,rookieDealAV$RookieAV*4,rookieDealAV$RookieAV)
rookieDealAV$RookieAV <- ifelse(rookieDealAV$YearsSinceDrafted == 2,rookieDealAV$RookieAV*2,rookieDealAV$RookieAV)
rookieDealAV$RookieAV <- ifelse(rookieDealAV$YearsSinceDrafted == 3 ,rookieDealAV$RookieAV*(4/3),rookieDealAV$RookieAV)

#Extrapolate bonus AV for players <= 4 years
rookieDealAV$RookieAV <- ifelse(rookieDealAV$Seasons == 4 & rookieDealAV$LastSeason == (thisYear-1),
                                rookieDealAV$RookieAV*(1.5),rookieDealAV$RookieAV) #Players going into year 5 with the same team
rookieDealAV$RookieAV <- ifelse(rookieDealAV$Seasons < 4 & rookieDealAV$LastSeason == (thisYear-1),
                                rookieDealAV$RookieAV + (rookieDealAV$RookieAV / 4) ,rookieDealAV$RookieAV) #On average, player bonus AV is 50% their rookie AV


#--------------Subset and Copy data frames-----
#Remove Full backs and long snappers
rookieDealAV <- rookieDealAV[rookieDealAV$Pos != "FB" & rookieDealAV$Pos !="LDB",]
rookieDealAV <- subset(rookieDealAV,rookieDealAV$DraftYear < thisYear)

##copy dataset
rookieDealAVNon <- rookieDealAV

#--------------xAV & dAv Calculations----
##Median AV per Pos & Classifier
PosSummaryNon <- rookieDealAVNon %>% group_by(Classifier,Pos) %>% dplyr::summarize(MedianAV = median(RookieAV))

##Find differences between xAV and AV 
rookieDealAVNon <- join(as.data.frame(rookieDealAVNon),as.data.frame(PosSummaryNon), by = c("Classifier","Pos"))
rookieDealAVNon$dAV <- rookieDealAVNon$RookieAV - rookieDealAVNon$MedianAV

##Transform Data
rookieDealAV$RookieAV[which(rookieDealAV$RookieAV < 0)] <- 0
rookieDealAV$RookieAV <- sqrt(rookieDealAV$RookieAV)

##Median AV per Pos & Rnd
PosSummaryNon <- rookieDealAV %>% group_by(Classifier,Pos) %>% dplyr::summarize(MedianAV = median(RookieAV))

##Find differences between xAV and AV 
rookieDealAV <- join(as.data.frame(rookieDealAV),as.data.frame(PosSummaryNon), by = c("Classifier","Pos"))
rookieDealAV$dAV <- rookieDealAV$RookieAV - rookieDealAV$MedianAV

##Count Classifier & Pos
PosSummary <- rookieDealAV %>% group_by(Classifier,Pos) %>% dplyr::summarize(MedianAV = median(RookieAV))
ClassifierSummary <- rookieDealAV %>% group_by(Classifier) %>% dplyr::summarize(MedianAV = median(RookieAV),n = n())

#Round Dataframes
rookieDealAVNon$RookieAV <- round(rookieDealAVNon$RookieAV,2)
rookieDealAVNon$MedianAV <- round(rookieDealAVNon$MedianAV,2)
rookieDealAVNon$dAV <- round(rookieDealAVNon$dAV,2)

#Sort Data Frame
rookieDealAVNon <- rookieDealAVNon[order(-rookieDealAVNon$dAV),]


#--------------Apply Draft Grades-----
##Draft Grades
ApplyGroups <- function(x) {
  cut(x, breaks=c(quantile(rookieDealAVNon$dAV, probs = c(0,0.05,0.25,0.40,0.60,0.75,0.95,1.00))), 
      labels=c("Bust","Poor Pick","Below Average Pick","Average Pick","Good Pick","Great Pick","Steal"), include.lowest=TRUE)
}
rookieDealAVNon$Grade <- sapply(rookieDealAVNon$dAV,ApplyGroups)


#-------------Add Player Headshots----
#Most Recent Image
rosters <- rosters[order(rosters$season,decreasing = TRUE),] 
rosters <- rosters[!duplicated(rosters$pfr_id),]

headshots <- rosters[,c("pfr_id","headshot_url")]
rookieDealAVNon <- join(rookieDealAVNon,headshots, by = "pfr_id", type = "left" )

#Ad-hoc Updates
rookieDealAVNon$headshot_url[which(rookieDealAVNon$Player == "JaMarcus Russell")] <- "https://www.pro-football-reference.com/req/20180910/images/headshots/RussJa00_2019.jpg"
rookieDealAVNon$headshot_url[which(rookieDealAVNon$Player == "Clinton Portis")]<- "https://www.pro-football-reference.com/req/20180910/images/headshots/PortCl00.jpg"
rookieDealAVNon$headshot_url[which(rookieDealAVNon$Player == "Brian Westbrook")] <- "https://www.pro-football-reference.com/req/20180910/images/headshots/WestBr00_2017.jpg"
rookieDealAVNon$headshot_url[which(rookieDealAVNon$PlayerID == "Charles Rogers1-2")] <-"https://www.pro-football-reference.com/req/20180910/images/headshots/RogeCh01_2019.jpg"
rookieDealAVNon$headshot_url[which(rookieDealAVNon$PlayerID == "Chris Williams1-14")] <- "http://static.nfl.com/static/content/public/static/img/fantasy/transparent/200x200/WIL188018.png"
rookieDealAVNon$headshot_url[which(rookieDealAVNon$PlayerID == "Freddie Mitchell1-25")] <- "https://www.pro-football-reference.com/players/M/MitcFr00.htm"
rookieDealAVNon$headshot_url[which(rookieDealAVNon$PlayerID == "Mike Adams2-56")] <- "https://www.pro-football-reference.com/req/20180910/images/headshots/AdamMi02_2019.jpg"
rookieDealAVNon$headshot_url[which(rookieDealAVNon$PlayerID == "Jonathan Baldwin1-26")] <- "https://www.pro-football-reference.com/req/20180910/images/headshots/BaldJo00_2019.jpg"

#Update NA headshot url's to generic NFL fantasy silhouette
rookieDealAVNon[is.na(rookieDealAVNon$headshot_url),"headshot_url"] <- "http://static.nfl.com/static/content/public/static/img/fantasy/transparent/200x200/WIL442916.png"

#-------------Write files to folder----
write.csv(rookieDealAV,"rookieDealAV.csv",row.names=FALSE,fileEncoding ="UTF-8")
write.csv(rookieDealAVNon,"rookieDealAVNon.csv",row.names=FALSE,fileEncoding ="UTF-8")
write.csv(nflData,"NFLData.csv",row.names=FALSE,fileEncoding="UTF-8")

#Extra stuff for testing-----
noId <- rookieDealAVNon[rookieDealAVNon$pfr_id == "",]
#noID[noId$Player %in% trades$pfr_name,]