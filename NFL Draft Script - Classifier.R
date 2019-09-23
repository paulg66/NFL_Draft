library(stringi)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(pgirmess)

setwd("C:/Users/Paul/Documents/R Projects/NFL/NFL Draft")

data <- read.csv("PlayerData.csv",stringsAsFactors = FALSE)
## Run draft script instead of reading from csv
##draftData <- read.csv("nfl_draft.csv",stringsAsFactors = FALSE)
draftClassifer <- read.csv("Draft_Classifier.csv")
nflLogos <- read.csv("NFLLogos.csv",stringsAsFactors = FALSE)

nflData <- data
nflData <- nflData[nflData$Rk != "Rk",] #remove extra title rows

nflData$Player <- stri_replace_all_regex(nflData$Player,"[*]","")
draftData$Player <- stri_replace_all_regex(draftData$Player,"HOF","")

draftData$Player <- str_trim(draftData$Player,"right")
nflData$Player <- str_trim(nflData$Player,"right")

#Create Unique PlayerID
nflData$PlayerID <- paste(nflData$Player,nflData$Draft,sep = "")
draftData$PlayerID <- paste(draftData$Player,draftData$Rnd, "-",draftData$Pick,sep="")

draftYrLookup <- draftData[c(31,30,3,5,1,2)] ##create lookup table for draft years of players
names(draftYrLookup) <- c("PlayerID","DraftYear","DraftTm","Pos","Rnd","Pick")

##Add Draft Year to Season Data
nflData<- merge(nflData,draftYrLookup, by = "PlayerID", all.y = TRUE)

#Find first season for each player
nflData$Year <- as.numeric(nflData$Year)
firstYr <- aggregate(Year~PlayerID,data = nflData, min)
names(firstYr) <- c("PlayerID","FirstYear")
nflData <- merge(nflData,firstYr,by = "PlayerID",all.x = TRUE)
nflData$DraftYear <- ifelse(is.na(nflData$DraftYear),nflData$FirstYear,nflData$DraftYear)
nflData <- nflData[nflData$DraftYear > 2000,]

#convert colums to numeric
nflData$Year <- as.numeric(nflData$Year)
nflData$DraftYear <- as.numeric(nflData$DraftYear)
nflData$AV <- as.numeric(nflData$AV)
nflData$G <- as.numeric(nflData$G)
nflData$GS <- as.numeric(nflData$GS)

#Calculate Season after being drafted
nflData$Season <- nflData$Year - nflData$DraftYear + 1

#Standardize Positions
nflData$Pos <- stri_replace_all_regex(nflData$Pos,"S","DB")
nflData$Pos <- stri_replace_all_regex(nflData$Pos,"CB","DB")
nflData$Pos <- stri_replace_all_regex(nflData$Pos,"DL","DT")
nflData$Pos <- stri_replace_all_regex(nflData$Pos,"NT","DT")
nflData$Pos <- stri_replace_all_regex(nflData$Pos,"DE","EDGE")
nflData$Pos <- stri_replace_all_regex(nflData$Pos,"OLB","EDGE")
nflData$Pos <- stri_replace_all_regex(nflData$Pos,"ILB","LB")
nflData$Pos <- stri_replace_all_regex(nflData$Pos,"OL","G")

#Fix years for drafted players who never played
nflData$Season <- ifelse(is.na(nflData$Season),-1,nflData$Season)
nflData$FirstYear <- ifelse(is.na(nflData$FirstYear),nflData$DraftYear,nflData$FirstYear)
nflData$Year <- ifelse(is.na(nflData$Year),nflData$DraftYear,nflData$Year)
nflData$AV <- ifelse(is.na(nflData$AV),0,nflData$AV)

#Find average AV during rookie deal
rookieDealAV <- nflData %>% filter(Season < 5) %>% group_by(PlayerID,Player,DraftYear,DraftTm,Pos,Rnd,Pick) %>% summarize(RookieAV = sum(AV, na.rm = TRUE),G = sum(G,na.rm = TRUE), GS = sum(GS, na.rm = TRUE),AvgAV = mean(AV, na.rm = TRUE),LastSeason = max(Year, na.rm = TRUE),Seasons = n())
rookieDealAV <- rookieDealAV[complete.cases(rookieDealAV),]
rookieDealAV <- merge(draftClassifer,rookieDealAV, by = "Pick",all = TRUE)
rookieDealAV$Classifier <- factor(rookieDealAV$Classifier, levels = (c("Top 5", "6-15", "16-32", "33-48","49-64","3","4","5","6","7")))
rookieDealAV <- rookieDealAV[complete.cases(rookieDealAV),]

##Fix LA Teams
rookieDealAV$DraftTm <- stri_replace_all_regex(rookieDealAV$DraftTm ,"STL","LAR")
rookieDealAV$DraftTm <- stri_replace_all_regex(rookieDealAV$DraftTm ,"SDG","LAC")

##Calculate Years since Drafted
thisYear <- as.integer(format(Sys.Date(), "%Y"))
rookieDealAV$YearsSinceDrafted <- thisYear - rookieDealAV$DraftYear 
rookieDealAV$YearsSinceDrafted[rookieDealAV$YearsSinceDrafted >= 4] <- -1

##Extrapolate totals for players drafted < 4 seasons ago and non-rookies
rookieDealAV$RookieAV <- ifelse(rookieDealAV$YearsSinceDrafted == rookieDealAV$Seasons & rookieDealAV$DraftYear <= (thisYear - 2),rookieDealAV$RookieAV*(4-rookieDealAV$YearsSinceDrafted),rookieDealAV$RookieAV)
rookieDealAV$RookieAV <- ifelse(rookieDealAV$YearsSinceDrafted == rookieDealAV$Seasons &  rookieDealAV$Seasons == 3 ,rookieDealAV$RookieAV*(1.25),rookieDealAV$RookieAV)

#Extrapolate totals for rookies by 4 years
rookieDealAV$RookieAV <- ifelse(rookieDealAV$YearsSinceDrafted == rookieDealAV$Seasons & rookieDealAV$DraftYear == (thisYear - 1),rookieDealAV$RookieAV*4,rookieDealAV$RookieAV)

#Remove Full backs and long snappers
rookieDealAV <- rookieDealAV[rookieDealAV$Pos != "FB" & rookieDealAV$Pos !="LDB",]
rookieDealAV <- subset(rookieDealAV,rookieDealAV$DraftYear < thisYear)

##copy dataset
rookieDealAVNon <- rookieDealAV

##Median AV per Pos & Rnd
PosSummaryNon <- rookieDealAVNon %>% group_by(Classifier,Pos) %>% summarize(MedianAV = median(RookieAV))

##Find differences between xAV and AV 
rookieDealAVNon <- join(as.data.frame(rookieDealAVNon),as.data.frame(PosSummaryNon), by = c("Classifier","Pos"))
rookieDealAVNon$dAV <- rookieDealAVNon$RookieAV - rookieDealAVNon$MedianAV

##Transform Data
rookieDealAV$RookieAV <- sqrt(rookieDealAV$RookieAV)

##Median AV per Pos & Rnd
PosSummaryNon <- rookieDealAV %>% group_by(Classifier,Pos) %>% summarize(MedianAV = median(RookieAV))

##Find differences between xAV and AV 
rookieDealAV <- join(as.data.frame(rookieDealAV),as.data.frame(PosSummaryNon), by = c("Classifier","Pos"))
rookieDealAV$dAV <- rookieDealAV$RookieAV - rookieDealAV$MedianAV

##Count Classifier & Pos
PosSummary <- rookieDealAV %>% group_by(Classifier,Pos) %>% summarize(MedianAV = median(RookieAV))
ClassifierSummary <- rookieDealAV %>% group_by(Classifier) %>% summarize(MedianAV = median(RookieAV),n = n())

#Graph
ggplot(rookieDealAV, aes(rookieDealAV$Classifier, rookieDealAV$RookieAV,fill = Classifier)) + geom_boxplot() + xlab("Round") + ylab("AV") + ggtitle("Round vs AV")

posGraph <- function(pos){
  posDF <- subset(rookieDealAV, Pos == pos)
  print(ggplot(posDF, aes(posDF$Rnd, posDF$RookieAV,fill = Rnd)) + geom_boxplot() + xlab("Round/Picks") + ylab("Sqrt(Approximate Value)") + ggtitle(paste("Round vs Approximate Value - Position:", pos)))
  pairwise.t.test(posDF$RookieAV,posDF$Rnd, p.adjust.method = "bonferroni")
  ##kruskalmc(RookieAV~Rnd , data = posDF)
}

ggplot(rookieDealAV,aes(x = reorder(Pos,RookieAV, FUN = median), y = RookieAV, fill = Pos)) + geom_boxplot() + xlab("Position") + ylab("Square Root of Approximate Value") + ggtitle("Position vs Approximate Value")

##ggplot(subset(rookieDealAV, Classifier == "Top 5"),aes(x = reorder(Pos,RookieAV, FUN = median), y = RookieAV, fill = Pos)) + geom_boxplot() + xlab("Position") + ylab("Approximate Value ") + ggtitle("Position vs AV")

##Calculate Differences between round average and position average for that round
PosDiff <- join(as.data.frame(PosSummary),as.data.frame(ClassifierSummary[,1:2]),by = "Classifier")
names(PosDiff) <- c("Classifier","Pos","PosAV","ClassifierAV")
PosDiff$Diff <- PosDiff$PosAV - PosDiff$ClassifierAV
PosDiff <- PosDiff[order(PosDiff$Diff),]

#PosTotals <- subset(PosDiff, Rnd <= 3) %>% group_by(Pos) %>% summarize(TotalDiff = sum(Diff))
#PosTotals <- PosTotals[order(PosTotals$TotalDiff),]

##Draft Grades
ApplyGroups <- function(x) {
  cut(x, breaks=c(quantile(rookieDealAVNon$dAV, probs = c(0,0.05,0.25,0.40,0.60,0.75,0.95,1.00))), 
      labels=c("Bust","Poor Pick","Below Average Pick","Average Pick","Good Pick","Great Pick","Steal"), include.lowest=TRUE)
}
rookieDealAVNon$Grade <- sapply(rookieDealAVNon$dAV,ApplyGroups)

##Colors for Bar Graph
colfunc<-colorRampPalette(c("red","yellow","green"))
colorsBar <- data.frame(AV = seq(1:15),color = colfunc(15))

##Colors for League Rankings
colorsRank <- data.frame(Rank = seq(1:32),color = 'red')
colorsRank$color <- ifelse(colorsRank$Rank > 10 & colorsRank$Rank <= 20,'yellow','red')
colorsRank$color <- ifelse(colorsRank$Rank <= 10,'green',colorsRank$color)
