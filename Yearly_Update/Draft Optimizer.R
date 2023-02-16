library(ggplot2)
library(dplyr)
library(nflreadr)
library(scales)
library(reactable)
library(reactablefmtr)
library(stringr)
library(sparkline)
library(formattable)

#Run Which_Pos_to_Draft FIRST

#Variables
salary_cap <- 220000000

#Load Data
nflData <- read.csv("Datasets/NFLData.csv",stringsAsFactors = FALSE)
rookieDealAV <- read.csv("Datasets/rookieDealAV.csv",stringsAsFactors = FALSE)
rookieDealAVNon <- read.csv("Datasets/rookieDealAVNon.csv",stringsAsFactors = FALSE)
draftClassifer <- read.csv("Datasets/Draft_Classifier.csv",stringsAsFactors = FALSE)
contract_expectations <- read.csv("Datasets/contract_expectations.csv",stringsAsFactors = FALSE)
rookie_cap_hits <- read.csv("Datasets/rookie_cap_hits.csv",stringsAsFactors = FALSE)
nflLogos <- read.csv("Datasets/NFLLogos.csv",stringsAsFactors = FALSE)

#FinalProbDF, #Player_Prob_Ranks

nflData <- merge(draftClassifer,nflData, by = "Pick",all = TRUE)
nflData$Classifier <- factor(nflData$Classifier, levels = (c("Top 5", "6-15", "16-32", "33-48","49-64","3","4","5","6","7")))


RndSummary <- rookieDealAVNon %>% group_by(Rnd) %>% dplyr::summarize(MedianAV = median(RookieAV),n = n())
ClassifierSummary <- rookieDealAVNon %>% group_by(Classifier) %>% dplyr::summarize(MedianAV_Classifier = median(RookieAV),n__Classifier = n())
PosSummary <- rookieDealAVNon %>% group_by(Pos,Classifier) %>% dplyr::summarize(MedianAV = median(RookieAV),n = n(),sd = sd(RookieAV))
PosSummary <- merge(PosSummary,ClassifierSummary,by = 'Classifier')
PosSummary <- PosSummary %>%  group_by(Pos) %>% mutate(rank_pos = rank(-(MedianAV-MedianAV_Classifier), ties.method = "min"))
PosSummary <- PosSummary %>%  group_by(Classifier) %>% mutate(rank_classifier = rank(-(MedianAV), ties.method = "min"))

#Free Agent Cost
#Median AV over first 4 seasons, see yearly cap hit on the open market for a player of that calibre
FA_Cost <- nflData %>% filter(Season <= 4) %>% group_by(PlayerID,Pos,Classifier) %>% dplyr::summarize(RookieAV = sum(AV))
FA_Cost <- FA_Cost %>% group_by(Classifier,Pos) %>% dplyr::summarize(sd = sd(RookieAV),RookieAV = median(RookieAV))
FA_Cost <- merge(FA_Cost,contract_expectations)
FA_Cost$FA_Cap_Hit <- FA_Cost$RookieAV*salary_cap / FA_Cost$AV_per_cap_percent / 4
FA_Cost$FA_Cap_Hit <- dollar_format()(FA_Cost$FA_Cap_Hit)

PosSummary <- merge(PosSummary,FA_Cost[,c("Pos","Classifier","AV_per_cap_percent","FA_Cap_Hit")])

#Position
Pos1 = 'T'
Pos1_Prob <- FinalProbDF[FinalProbDF$Pos == Pos1,]
Pos1_Summary <- PosSummary[PosSummary$Pos == Pos1,]


#Classifier
ClassiferSelect = '16-32'
Classifer_Prob <- FinalProbDF[FinalProbDF$Classifier == ClassiferSelect,]
Classifer_Rank <- Player_Prob_Ranks[Player_Prob_Ranks$Classifier == ClassiferSelect,]
Classifier_Select_Summary <- PosSummary[PosSummary$Classifier == ClassiferSelect,]

#Tables
starter_table <- merge(FinalProbDF,Player_Prob_Ranks,by = c('Classifier')) %>%
  arrange(match(Classifier,c("Top 5","6-15","16-32","33-48","49-64","3","4","5","6","7")))
starter_table$Avg8 <- starter_table$Probability8 - starter_table$Avg8
starter_table$Avg4 <- starter_table$Probability4 - starter_table$Avg4
starter_table$Avg2 <- starter_table$Probability2 - starter_table$Avg2
starter_table$AvgB <- starter_table$ProbabilityB - starter_table$AvgB
starter_table$AvgN <- starter_table$ProbabilityN - starter_table$AvgN

starter_table <- starter_table[,c("Classifier","Pos","Probability8","Avg8","Probability4","Avg4"
                                  ,"Probability2","Avg2","ProbabilityB","AvgB","ProbabilityN","AvgN")]

starter_table <- starter_table %>% arrange(match(Classifier,c("Top 5","6-15","16-32","33-48","49-64","3","4","5","6","7")))

#colors_palette <- c("#e24115","#fe9d00","#fdce02","#77c000","#019a48","#236df7")
colors_palette_avg <- c("#e24115","#fe9d00","#fdce02","#f0f2f5","#77c000","#019a48")
colors_palette_prob <- c("#e24115","#fe9d00","#fdce02","#77c000","#019a48")

colfunc_bar<-colorRampPalette(c("#e24115","#fe9d00","#fdce02","#77c000","#019a48"))
colorsBar_prob <- data.frame(prob = seq(from = 0, to = 1,by = 0.01),color = colfunc_bar(101))
colorsBar_prob$prob <- as.character(colorsBar_prob$prob)

starter_table <- merge(starter_table,colorsBar_prob,by.x = c("Probability8"),by.y= c("prob"))
starter_table <- merge(starter_table,colorsBar_prob,by.x = c("Probability4"),by.y= c("prob"))
starter_table <- merge(starter_table,colorsBar_prob,by.x = c("Probability2"),by.y= c("prob"))
starter_table <-starter_table %>% dplyr::rename(color8 = color.x, color4 = color.y, color2 = color)

starter_table <- merge(starter_table,colorsBar_prob,by.x = c("ProbabilityB"),by.y= c("prob"))
starter_table <- merge(starter_table,colorsBar_prob,by.x = c("ProbabilityN"),by.y= c("prob"))
starter_table <-starter_table %>% dplyr::rename(colorB = color.x, colorN = color.y)

starter_table <- starter_table[,c("Classifier","Pos","Probability8","Avg8","Probability4","Avg4"
                                  ,"Probability2","Avg2","ProbabilityB","AvgB","ProbabilityN","AvgN",
                                  "color8","color4","color2","colorB","colorN")]

starter_table <- starter_table %>% arrange(match(Classifier,c("Top 5","6-15","16-32","33-48","49-64","3","4","5","6","7")))
write.csv(starter_table,'starter_table.csv',row.names=FALSE,fileEncoding ="UTF-8")

#Stop here if building table for shiny app
starter_table <- starter_table[starter_table$Pos == Pos1,]

starter_table <- starter_table %>% arrange(match(Classifier,c("Top 5","6-15","16-32","33-48","49-64","3","4","5","6","7")))

reactable(
  starter_table,
  theme = espn(centered = TRUE),
  compact = TRUE,
  columnGroups = list(
    colGroup(name = "8-Yr Starter", columns = c("Probability8","Avg8")),
    colGroup(name = "4-Yr Starter", columns = c("Probability4","Avg4")),
    colGroup(name = "2-Yr Starter", columns = c("Probability2","Avg2")),
    colGroup(name = "Career Backup", columns = c("ProbabilityB","AvgB")),
    colGroup(name = "Non-Roster Player", columns = c("ProbabilityN","AvgN"))
  ),#colGroups
  columns = list(
    # 8 - Year Starter
    Probability8 = colDef(name = 'Probability',
                          align = 'left',
                          cell = data_bars(
                            starter_table,
                            max_value = 1,
                            min_value = 0,
                            fill_color_ref = "color8",
                            number_fmt = scales::percent_format(accuracy = 0.1),
                            force_outside = c(0,0.4),
                            text_position = "above",
                            round_edges = TRUE
                          ),
                          style = list(borderLeft = "1px dashed rgba(0, 0, 0, 0.3)")                          
    ),#prob8
    Avg8 = colDef(name = "vs Avg",
                  align = "center",
                  maxWidth = 65,
                  cell = color_tiles(
                    starter_table,
                    number_fmt = scales::percent_format(accuracy = 0.1),
                    colors = colors_palette_avg,
                    box_shadow = TRUE,
                    span = c("Avg8","Avg4","Avg2","AvgB","AvgN"))
                  ),#Avg8
    
    # 4 - Year Starter
    Probability4 = colDef(name = 'Probability',
                          align = 'left',
                          cell = data_bars(
                            starter_table,
                            max_value = 1,
                            min_value = 0,
                            fill_color_ref = "color4",
                            number_fmt = scales::percent_format(accuracy = 0.1),
                            force_outside = c(0,0.4),
                            text_position = "above",
                            round_edges = TRUE
                          ),
                          style = list(borderLeft = "1px dashed rgba(0, 0, 0, 0.3)")                          
    ),#prob4
    Avg4 = colDef(name = "vs Avg",
                  align = "center",
                  maxWidth = 65,
                  cell = color_tiles(
                    starter_table,
                    number_fmt = scales::percent_format(accuracy = 0.1),
                    colors = colors_palette_avg,
                    box_shadow = TRUE,
                    span = c("Avg8","Avg4","Avg2","AvgB","AvgN"))
    ),#Avg4
    
    # 2 - Year Starter
    Probability2 = colDef(name = 'Probability',
                          align = 'left',
                          cell = data_bars(
                            starter_table,
                            max_value = 1,
                            min_value = 0,
                            fill_color_ref = "color2",
                            number_fmt = scales::percent_format(accuracy = 0.1),
                            force_outside = c(0,0.4),
                            text_position = "above",
                            round_edges = TRUE
                          ),
                          style = list(borderLeft = "1px dashed rgba(0, 0, 0, 0.3)")                          
    ),#prob2
    Avg2 = colDef(name = "vs Avg",
                  align = "center",
                  maxWidth = 65,
                  cell = color_tiles(
                    starter_table,
                    number_fmt = scales::percent_format(accuracy = 0.1),
                    colors = colors_palette_avg,
                    box_shadow = TRUE,
                    span = c("Avg8","Avg4","Avg2","AvgB","AvgN"))
    ),#Avg2
    
    # Backup
    ProbabilityB = colDef(name = 'Probability',
                          align = 'left',
                          cell = data_bars(
                            starter_table,
                            max_value = 1,
                            min_value = 0,
                            fill_color_ref = "colorB",
                            number_fmt = scales::percent_format(accuracy = 0.1),
                            force_outside = c(0,0.4),
                            text_position = "above",
                            round_edges = TRUE
                          ),
                          style = list(borderLeft = "1px dashed rgba(0, 0, 0, 0.3)")                          
    ),#probB
    AvgB = colDef(name = "vs Avg",
                  align = "center",
                  maxWidth = 65,
                  cell = color_tiles(
                    starter_table,
                    number_fmt = scales::percent_format(accuracy = 0.1),
                    colors = colors_palette_avg,
                    box_shadow = TRUE,
                    span = c("Avg8","Avg4","Avg2","AvgB","AvgN"))
    ),#AvgB
    
    # Non-Roster
    ProbabilityN = colDef(name = 'Probability',
                          align = 'left',
                          cell = data_bars(
                            starter_table,
                            max_value = 1,
                            min_value = 0,
                            fill_color_ref = "colorN",
                            number_fmt = scales::percent_format(accuracy = 0.1),
                            force_outside = c(0,0.4),
                            text_position = "above",
                            round_edges = TRUE
                          ),
                          style = list(borderLeft = "1px dashed rgba(0, 0, 0, 0.3)")                          
    ),#prob4
    AvgN = colDef(name = "vs Avg",
                  align = "center",
                  maxWidth = 65,
                  cell = color_tiles(
                    starter_table,
                    number_fmt = scales::percent_format(accuracy = 0.1),
                    colors = colors_palette_avg,
                    box_shadow = TRUE,
                    span = c("Avg8","Avg4","Avg2","AvgB","AvgN"))
    ),#AvgN
    color8 = colDef(show = FALSE),
    color4 = colDef(show = FALSE),
    color2 = colDef(show = FALSE),
    colorB = colDef(show = FALSE),
    colorN = colDef(show = FALSE)
  )#column list
)#reacttable

#Player Archetypes Table
player_archetypes <- read.csv("Datasets/player_archetypes.csv",stringsAsFactors = FALSE)
player_archetypes <- merge(player_archetypes,nflData[,c('PlayerID','DraftTm','Player','Season','AV')])
player_archetypes <- merge(player_archetypes,rookieDealAVNon[,c('PlayerID','headshot_url')])

player_archetypes$headshot_url <- paste('<img src="',player_archetypes$headshot_url,'">',sep = '')
player_archetypes$headshot_url <- str_replace(player_archetypes$headshot_url,"w_250","w_75")
player_archetypes$headshot_url <- str_replace(player_archetypes$headshot_url,"h_200","h_60")
player_archetypes$headshot_url <- str_replace(player_archetypes$headshot_url,"w=250","w=75")
player_archetypes$headshot_url <- str_replace(player_archetypes$headshot_url,"h=200","h=60")

player_archetypes_sum <- player_archetypes %>% group_by(PlayerID,DraftTm,Pos,Player_Type,Player,headshot_url) %>%
  dplyr::summarize(Total_AV = sum(AV),Seasons = spk_chr(AV, type = 'bar')) %>% arrange(desc(Total_AV))
player_archetypes_sum <- player_archetypes_sum[,c("headshot_url","Player_Type","Player","DraftTm","Pos",'Total_AV',"Seasons")]
names(player_archetypes_sum) <- c(" ","Player Type","Name","Draft Team","Pos","Total AV","AV by Season")
player_archetypes_sum  %>% formattable() %>%  formattable::as.htmlwidget() %>% spk_add_deps()

write.csv(player_archetypes_sum,'player_archetypes_sum.csv',row.names=FALSE,fileEncoding ="UTF-8")
