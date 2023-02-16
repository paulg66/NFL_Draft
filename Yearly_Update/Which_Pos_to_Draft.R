library(dplyr)
library(scales)

options(scipen=999) #Remove scientific notation on output

#Calculate Expected AV for a starter
starterData <- nflData %>% filter(GS >=10) %>% group_by(Pos) %>% 
  dplyr::summarise(GS = sum(GS),AV=sum(AV))
starterData$PerGameAV <- starterData$AV/starterData$GS

#Calculate Expected AV for a backup
backupData <- nflData %>% filter(GS <=4, G >= 12) %>% group_by(Pos) %>% 
  dplyr::summarise(G = sum(G),AV=sum(AV))
backupData$BackupPerGame <- backupData$AV / backupData$G
starterData <- merge(starterData,backupData[,c(1,4)])

#Assume a starter plays an average of 12 games per year
  #12 games covers for normal amount of injuries and teams not starting players in years 1 or 2 right away
#x6 since years 5-8 have half the weight

starterData$xAVStarter8 <- starterData$PerGameAV*12*6 
starterData$xAVStarter4 <- starterData$PerGameAV*12*4 #Starter for 4 seasons
starterData$xAVStarter2 <- starterData$PerGameAV*12*2 + starterData$BackupPerGame*12*2 #2 seasons as starter, 2 seasons backup
starterData$xAVBackup <- starterData$BackupPerGame*12*3 #Backup for 3 seasons

#Calculate the probability of drafting a starter in each classifier
#QBs
AVB <- sqrt(as.numeric(starterData[which(starterData$Pos == "QB"),"xAVBackup"]))
AV2 <- sqrt(as.numeric(starterData[which(starterData$Pos == "QB"),"xAVStarter2"]))
AV4 <- sqrt(as.numeric(starterData[which(starterData$Pos == "QB"),"xAVStarter4"]))
AV8 <- sqrt(as.numeric(starterData[which(starterData$Pos == "QB"),"xAVStarter8"]))
df_player_prob <- rookieDealAV[rookieDealAV$Pos == "QB",] %>% group_by(Classifier) %>% 
  dplyr::summarize(Avg = median(RookieAV),SD = sd(RookieAV)) #get all of them then delete dupes
df_player_prob <- data.frame(df_player_prob)
df_player_prob <- df_player_prob %>% arrange(match(Classifier,c("Top 5","6-15","16-32","33-48","49-64","3","4","5","6","7")))
df_player_prob$Probability8 <- apply(df_player_prob[,2:3], 1, function(x) pnorm(AV8,x[1],x[2],lower.tail = FALSE))
df_player_prob$Probability4 <- apply(df_player_prob[,2:3], 1, function(x) pnorm(AV4,x[1],x[2],lower.tail = FALSE))
df_player_prob$Probability2 <- apply(df_player_prob[,2:3], 1, function(x) pnorm(AV2,x[1],x[2],lower.tail = FALSE))
df_player_prob$ProbabilityB <- apply(df_player_prob[,2:3], 1, function(x) pnorm(AVB,x[1],x[2],lower.tail = FALSE))
df_player_prob$ProbabilityN <- apply(df_player_prob[,2:3], 1, function(x) pnorm(0,x[1],x[2],lower.tail = FALSE))
FinalProbDF <- df_player_prob[,c("Classifier","Probability8","Probability4","Probability2","ProbabilityB","ProbabilityN")]
FinalProbDF$Pos <- "QB"

#Loop through remaining position
positions <- starterData$Pos
positions <- positions[positions != "QB"]
positions <- positions[positions != "FB"]

for (i in 1:length(positions)){
  Pos = positions[i]
  AVB <- sqrt(as.numeric(starterData[which(starterData$Pos == Pos),"xAVBackup"]))
  AV2 <- sqrt(as.numeric(starterData[which(starterData$Pos == Pos),"xAVStarter2"]))
  AV4 <- sqrt(as.numeric(starterData[which(starterData$Pos == Pos),"xAVStarter4"]))
  AV8 <- sqrt(as.numeric(starterData[which(starterData$Pos == Pos),"xAVStarter8"]))
  df_player_prob <- rookieDealAV[rookieDealAV$Pos == Pos,] %>% group_by(Classifier) %>% 
    dplyr::summarize(Avg = median(RookieAV),SD = sd(RookieAV)) #get all of them then delete dupes
  df_player_prob <- data.frame(df_player_prob)
  df_player_prob <- df_player_prob %>% arrange(match(Classifier,c("Top 5","6-15","16-32","33-48","49-64","3","4","5","6","7")))
  df_player_prob$Probability8 <- apply(df_player_prob[,2:3], 1, function(x) pnorm(AV8,x[1],x[2],lower.tail = FALSE))
  df_player_prob$Probability4 <- apply(df_player_prob[,2:3], 1, function(x) pnorm(AV4,x[1],x[2],lower.tail = FALSE))
  df_player_prob$Probability2 <- apply(df_player_prob[,2:3], 1, function(x) pnorm(AV2,x[1],x[2],lower.tail = FALSE))
  df_player_prob$ProbabilityB <- apply(df_player_prob[,2:3], 1, function(x) pnorm(AVB,x[1],x[2],lower.tail = FALSE))
  df_player_prob$ProbabilityN <- apply(df_player_prob[,2:3], 1, function(x) pnorm(0,x[1],x[2],lower.tail = FALSE))
  df_player_prob$Pos <- Pos
  FinalProbDF <- rbind(FinalProbDF,df_player_prob[,c("Pos","Classifier","Probability8","Probability4","Probability2","ProbabilityB","ProbabilityN")])
  
}#end of for loop

#Ignore error after loop

#Check Average AV in first 2 seasons
#First2Yrs <- nflData %>% filter(Season_Num <=2 & Season_Num >=1 & AV >= 0) %>% group_by(Classifier,Pos) %>% dplyr::summarise(AV_First2Yrs = median(AV),n = n())
#Year3_4 <- nflData %>% filter(Season <=4 & Season >=3 & AV >= 0) %>% group_by(Classifier,Pos) %>% dplyr::summarise(AV_Year3_4 = median(AV))
#FinalProbDF <- merge(FinalProbDF,First2Yrs)
#FinalProbDF <- merge(FinalProbDF,Year3_4)
FinalProbDF <- FinalProbDF %>% arrange(match(Classifier,c("Top 5","6-15","16-32","33-48","49-64","3","4","5","6","7")))
FinalProbDF <- FinalProbDF[order(FinalProbDF$Pos),]

#Format Columns

FinalProbDF$Probability8 <- round(FinalProbDF$Probability8,2)
FinalProbDF$Probability4 <- round(FinalProbDF$Probability4,2)
FinalProbDF$Probability2 <- round(FinalProbDF$Probability2,2)
FinalProbDF$ProbabilityB <- round(FinalProbDF$ProbabilityB,2)
FinalProbDF$ProbabilityN <- round(FinalProbDF$ProbabilityN,2)
#FinalProbDF$AV_First2Yrs <- round(FinalProbDF$AV_First2Yrs,2)
#FinalProbDF$AV_Year3_4 <- round(FinalProbDF$AV_Year3_4,2)
#  df_player_prob$Probability4 <- label_percent()(df_player_prob$Probability4)

#Player Prob Ranking
Player_Prob_Ranks <- FinalProbDF %>%
  group_by(Classifier) %>%
  summarize(Avg8 = mean(Probability8,na.rm = TRUE),
            Avg4 = mean(Probability4,na.rm = TRUE),
            Avg2 = mean(Probability2,na.rm = TRUE),
            AvgB = mean(ProbabilityB,na.rm = TRUE),
            AvgN = mean(ProbabilityN,na.rm = TRUE)
  )


