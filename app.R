# Shiny R Application - NFL Draft
# By: Paul Gallagher

#----------------------------Pre Load----------------------------------

library(shiny)
library(ggplot2)
library(ggimage)
library(reshape)
library(DT)
library(shinydashboard)
library(scales)
library(pgirmess)
library(dplyr)
library(markdown)
library(lamisc)
library(reactable)
library(reactablefmtr)
library(sparkline)
library(formattable)
library(httr)

#Load Data
nflData <- read.csv("NFLData.csv",stringsAsFactors = FALSE)
rookieDealAV <- read.csv("rookieDealAV.csv",stringsAsFactors = FALSE)
rookieDealAVNon <- read.csv("rookieDealAVNon.csv",stringsAsFactors = FALSE)
nflLogos <- read.csv("NFLLogos.csv",stringsAsFactors = FALSE)
player_archetypes_sum <- read.csv("player_archetypes_sum.csv",stringsAsFactors = FALSE,,row.names = 'X.')
starter_table <- read.csv("starter_table.csv",stringsAsFactors = FALSE)

RndSummary <- rookieDealAV %>% group_by(Rnd) %>% dplyr::summarize(MedianAV = median(RookieAV),n = n())
ClassifierSummary <- rookieDealAV %>% group_by(Classifier) %>% dplyr::summarize(MedianAV = median(RookieAV),n = n())

##Colors for Bar Graph
colfunc<-colorRampPalette(c("red","yellow","green"))
colorsBar <- data.frame(AV = seq(1:20),color = colfunc(20))
colorsBar <- rbind(colorsBar,data.frame(AV =seq(along.with = 21:40,from = 21),color = "#00FF00"))

##Colors for League Rankings
colorsRank <- data.frame(Rank = seq(1:32),color = 'red')
colorsRank$color <- ifelse(colorsRank$Rank > 10 & colorsRank$Rank <= 20,'yellow','red')
colorsRank$color <- ifelse(colorsRank$Rank <= 10,'green',colorsRank$color)

# UI-----
ui <- navbarPage("NFL Draft",
                 #-------------------------------------Dashboards--------------------                 
                 tabPanel("Teams",dashboardPage(skin = "blue",
                                                
                                                #Page Title         
                                                dashboardHeader(title = "NFL Teams"),
                                                
                                                #DropDown for Team
                                                dashboardSidebar(selectInput("teamDash","Team",c("ALL",unique(rookieDealAV$DraftTm)[sort.list(unique(rookieDealAV$DraftTm))]),selected = "ARI"),htmlOutput("logo")),
                                                dashboardBody(fluidRow(
                                                  column(4,selectInput("posTeam","Position",list("ALL","QB","RB","WR","TE","T","G","C","DT","EDGE","LB","DB","K"))),
                                                  column(4,selectInput("draftYrTeam","Draft Year",c("ALL",unique(rookieDealAV[which(rookieDealAV$DraftYear != 2000),"DraftYear"])[sort.list(unique(rookieDealAV[which(rookieDealAV$DraftYear != 2000),"DraftYear"]))])))),
                                                  fluidRow(valueBoxOutput("NFLRanking"),valueBoxOutput("teamdAV"),valueBoxOutput("teamAV")),
                                                  fluidRow(
                                                    box(title = "Best Draft Pick",status = "success",solidHeader = TRUE, fluidRow(column(5,htmlOutput("headshotBest"),h3(textOutput("DashBestName")),h4(htmlOutput("DashBestGrade")),h4(textOutput("DashBestYr")),h4(textOutput("DashBestRndPick")),br(),h4(textOutput("DashBestAV")),h4(textOutput("DashBestxAV")),h4(htmlOutput("DashBestdAV"))),column(7,plotOutput({"BestGraph"})))),
                                                    box(title = "Worst Draft Pick",status = "danger", solidHeader = TRUE,fluidRow(column(5,htmlOutput("headshotWorst"),h3(textOutput("DashWorstName")),h4(htmlOutput("DashWorstGrade")),h4(textOutput("DashWorstYr")),h4(textOutput("DashWorstRndPick")),br(),h4(textOutput("DashWorstAV")),h4(textOutput("DashWorstxAV")),h4(htmlOutput("DashWorstdAV"))),column(7,plotOutput({"WorstGraph"}))))
                                                  ),#fluidRow draft picks
                                                  "Viewing on a mobile device? Click the 3 solid white lines near the top of the page to change teams",br(),br(),"AV = Approximate Value",br(),"Players Drafted From 2001-2022",br(),"Data Source: www.pro-football-reference.com",br(),br(),uiOutput('RedditLink'),br(),uiOutput('GitHubLink'),uiOutput("FeedbackLink"))#body
                 )),#Teams   

                 #--------------------------Draft Optimizer------------------------------
                 tabPanel('Draft Optimizer',
                          tabsetPanel(type = "tabs",
                                      tabPanel("By Position",
                                               titlePanel("Draft Pick by Position Optimizer"),
                                               fluidRow(
                                                 column(3,
                                                        selectInput("posOptimizer","Position",list("QB","RB","WR","TE","T","G","C","DT","EDGE","LB","DB"))
                                                 )
                                               ),
                                               #fluidrow
                                               markdown(
                                                 '       **Probability:** Is the probability of drafting a player who is **at least** as good as the player type, or better. <br />
                                                         **Vs Avg:** The difference the selected position and all other positions, for that player type and draft classifier.
                                                 '
                                               ),#markdown
                                               reactableOutput("PosTableOptimizer"),
                                               markdown(
                                                 '__________________________________________________________________________________________________________________________ <br />
                                                 ### Player Type Examples'
                                               ),#markdown
                                               column(6,htmlOutput("PlayerArchetypes"))  
                                      )#tabPanel
                          )#tabset Panel
                 ),#Draft Optimizer
                 
                 #-----------------------Tables------------------------------              
                 tabPanel("Tables",
                          tabsetPanel(type = "tabs",
                                      tabPanel("Players",
                                               titlePanel("NFL Players"),
                                               fluidRow(
                                                 column(3,
                                                        selectInput("team","Team",c("ALL",unique(rookieDealAV$DraftTm)[sort.list(unique(rookieDealAV$DraftTm))]))
                                                 ),
                                                 column(3,
                                                        selectInput("posPlayers","Position",list("ALL","QB","RB","WR","TE","T","G","C","DT","EDGE","LB","DB","K"))
                                                 ),
                                                 column(3,
                                                        selectInput("rndPlayers","Round",list("ALL",1,2,3,4,5,6,7))
                                                 ),
                                                 column(3,
                                                        selectInput("draftYr","Draft Year",c("ALL",unique(rookieDealAV[which(rookieDealAV$DraftYear != 2000),"DraftYear"])
                                                                                             [sort.list(unique(rookieDealAV[which(rookieDealAV$DraftYear != 2000),"DraftYear"]))]))
                                                 )
                                               ),#fluid Row 
                                               DT::dataTableOutput("PlayerTable"),
                                               "AV - Approximate Value - First 4 years + next 4 years if the player is still with their draft team, years 4-8 weighted at 50%",br(), "xAV - Expected Approximate Value", br(),"dAV – Difference between expected and actual approximate value"
                                      ),#Players
                                      tabPanel("Draft Classes",verbatimTextOutput("value"),
                                               titlePanel("Draft Classes"),
                                               checkboxGroupInput("radio_draft_class", label = h4("Grouping Options:"), 
                                                                  choices = list("Team" = "Team", "Position" = "Pos","Draft Year" = "Yr"),
                                                                  selected = c("Team","Yr"), inline = TRUE),
                                               DT::dataTableOutput("Draft_Class_Table"),
                                               "dAV – Difference between expected and actual approximate value",br(),"Draft Ranking - For position & team groups - ranking is within a draft year; otherwise all-time"
                                      )#Draft Years
                          )#Tabset Panel               
                 ),#Tables  
                 
                 #--------------------------Statistical Tests-------------------------------
                 tabPanel("Statistical Tests",navbarPage("",
                                                         tabPanel("ANOVA - By Position",             
                                                                  
                                                                  # Application title
                                                                  titlePanel("NFL Draft - By Position"),
                                                                  
                                                                  # Dropdown for Pos
                                                                  sidebarLayout(
                                                                    sidebarPanel(
                                                                      selectInput("pos",
                                                                                  label = "Select Position",
                                                                                  choices = list("ALL","QB","RB","WR","TE","T","G","C","DT","EDGE","LB","DB","K")
                                                                      ),verbatimTextOutput("PosTable"),h6("Kruskal-Wallis Test - p-Value: 0.05")
                                                                    ),
                                                                    
                                                                    # Show boxplot
                                                                    mainPanel(plotOutput("PosGraph"),plotOutput("PosAvgGraph")
                                                                    )
                                                                  )
                                                         ),##tab Panel by Pos
                                                         tabPanel("ANOVA - By Round",
                                                                  # Application title
                                                                  titlePanel("NFL Draft - By Round"),
                                                                  # Dropdown for Round
                                                                  sidebarLayout(
                                                                    sidebarPanel(
                                                                      selectInput("rnd",
                                                                                  label = "Select Round",
                                                                                  choices = list("ALL",1,2,3,4,5,6,7)
                                                                      ),verbatimTextOutput("RndTable"),h6("Kruskal-Wallis Test - p-Value: 0.05")
                                                                    ),
                                                                    # Show boxplot
                                                                    mainPanel(plotOutput("RndGraph"))
                                                                  )
                                                         ),##Tab Player Probability
                                                         tabPanel("Player Probabilities",
                                                                  # Application title
                                                                  titlePanel("The Probabilty of Drafting a Comparable Player"),
                                                                  h5("This section calculates the probability of drafting a player with comparable production in each section of the draft. 
                                                                     Drop down only includes players with a minimum AV of 30"),
                                                    
                                                                  selectInput('ProbSelect', 'Select a Player', unique(rookieDealAVNon[which(rookieDealAVNon$RookieAV >= 25),"PlayerID"])
                                                                                                               [sort.list(unique(rookieDealAVNon[which(rookieDealAVNon$RookieAV >= 25),"PlayerID"]))],selected = "David Johnson3-86", selectize=TRUE),
                                                                  verbatimTextOutput("PlayerProbTable")
                                                         ))##tabpanel By round
                          #Write_up--------------------------------------------------------------
                 ),tabPanel("Methodology",fluidPage(includeMarkdown("https://www.dropbox.com/s/vdd752d67pxqaeq/Shiny%20Writeup.md?dl=1"),hr(),uiOutput('AVLink'),br()))#Navbar, statistical tests
                 ,tabPanel("Settings",sliderInput("draftYrGlobal","Select Draft Years - Filters all Tables & Charts",min = 2000, max = max(rookieDealAV$DraftYear),value = c(2000, max(rookieDealAV$DraftYear)),sep ='',step = 1))
)#UI

# SERVER START-------------------------------------------------------------------------
server <- function(input, output,session) {
    colors <- data.frame("Grade" = c("Bust","Poor Pick","Below Average Pick","Average Pick","Good Pick","Great Pick","Steal","Strategic Pick"),"ColorCode" = c("#4f0101","#b80000","#ff5500","#808080","#04c729","#017016", "#3495eb","#000000"))
    
    #Update Draft Year Based on Slider
    observe({
      years <- c("ALL",seq(input$draftYrGlobal[1],input$draftYrGlobal[2],1))
      if(2000 %in% years){
        years <- years[-2]
      }
      updateSelectInput(session,"draftYrTeam",choices = years,selected = "ALL")
      updateSelectInput(session,"draftYr",choices = years,selected = "ALL")
    })#Update Drop Down
    
# Statistical Tests - Server ----------------------------------------------
    ##By Position Graph
    output$PosGraph <- renderPlot({
      ggSubset <- rookieDealAV[which(rookieDealAV$DraftYear >= input$draftYrGlobal[1] & 
                                           rookieDealAV$DraftYear <= input$draftYrGlobal[2]),]
      ggSubset$Classifier <- factor(ggSubset$Classifier,c("Top 5","6-15","16-32","33-48","49-64","3","4","5","6","7"))
        if (input$pos == "ALL"){
            
            print(ggplot(ggSubset, aes(Classifier, RookieAV,fill = as.factor(Classifier))) + geom_boxplot() + xlab("Section of the Draft") + ylab("Sqrt(Approximate Value)") + ggtitle("Classifier vs Approximate Value")) 
        }else{
            posDF <- subset(ggSubset, Pos == input$pos)
            print(ggplot(posDF, aes(Classifier, RookieAV,fill = as.factor(Classifier))) + geom_boxplot() + xlab("Section of the Draft") + ylab("Sqrt(Approximate Value)") + ggtitle(paste("Classifier vs Approximate Value - Position:", input$pos))) 
        }

    })
    
    ##By Position Vs Average
    output$PosAvgGraph <- renderPlot({
      PosSummary <- rookieDealAV[which(rookieDealAV$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAV$DraftYear <= input$draftYrGlobal[2]),] %>% 
                                         group_by(Classifier,Pos) %>% dplyr::summarize(MedianAV = median(RookieAV))
        if (input$pos == "ALL"|input$pos == "K"){
            
        }else{
            ggSubset <- subset(PosSummary, Pos == input$pos)
            ggSubset$RndAvg <- ClassifierSummary[ClassifierSummary$Classifier %in% ggSubset$Classifier,]$MedianAV
            ggSubset <- melt(as.data.frame(ggSubset),names(ggSubset)[1:2])
            ggSubset$Classifier <- factor(ggSubset$Classifier,c("Top 5","6-15","16-32","33-48","49-64","3","4","5","6","7"))
            ggplot(ggSubset, aes(x = Classifier, y = value, group = variable, colour = variable)) + geom_point(size = 2) + geom_line() + scale_colour_manual(name = "Pick Type", labels = c(input$pos,"All Pos Avg"),values = c("blue","red")) + ggtitle(paste("All Position Average Vs. Average for: ", input$pos)) + xlab("Section of the Draft") + ylab("Sqrt(Approximate Value)")
        }
    })
    
    
    ##By Round Graph
    output$RndGraph <- renderPlot({
      rookieDealAV <- rookieDealAV[which(rookieDealAV$DraftYear >= input$draftYrGlobal[1] & 
                                           rookieDealAV$DraftYear <= input$draftYrGlobal[2]),]
        if (input$rnd == "ALL"){
            ggplot(subset(rookieDealAV),aes(x = reorder(Pos,RookieAV, FUN = median), y = RookieAV, fill = Pos)) + geom_boxplot() + xlab("Position") + ylab("Sqrt(Approximate Value)") + ggtitle("Position vs AV")
        }else{
            posDF <- subset(rookieDealAV, Pos == input$rnd)
            ggplot(subset(rookieDealAV, Rnd == input$rnd),aes(x = reorder(Pos,RookieAV, FUN = median), y = RookieAV, fill = Pos)) + geom_boxplot() + xlab("Position") + ylab("Sqrt(Approximate Value)") + ggtitle("Position vs AV")
        }
        
    })
    
    ##By Position - ANOVA
    output$PosTable <- renderPrint({
      rookieDealAV <- rookieDealAV[which(rookieDealAV$DraftYear >= input$draftYrGlobal[1] & 
                                           rookieDealAV$DraftYear <= input$draftYrGlobal[2]),]
        if (input$pos == "ALL"){
            posANOVA <- kruskalmc(RookieAV~Classifier , data = rookieDealAV)
            posANOVA$dif.com
            posANOVA <- data.frame(posANOVA$dif.com)
            posANOVA[order(posANOVA$difference,posANOVA$obs.dif,decreasing = TRUE),]
        }else{
            posDF <- subset(rookieDealAV, Pos == input$pos)
            posANOVA <- kruskalmc(RookieAV~Classifier , data = posDF)
            posANOVA <- data.frame(posANOVA$dif.com)
            posANOVA[order(posANOVA$difference,posANOVA$obs.dif,decreasing = TRUE),]
        }
    })
    
    ##By Round - ANOVA
    output$RndTable <- renderPrint({
      rookieDealAV <- rookieDealAV[which(rookieDealAV$DraftYear >= input$draftYrGlobal[1] & 
                                           rookieDealAV$DraftYear <= input$draftYrGlobal[2]),]
        if (input$rnd == "ALL"){
            rndANOVA <- kruskalmc(RookieAV~Pos , data = rookieDealAV)
            rndANOVA <- data.frame(rndANOVA$dif.com)
            rndANOVA[order(rndANOVA$difference,rndANOVA$obs.dif,decreasing = TRUE),]
        }else{
            posDF <- subset(rookieDealAV, Rnd == input$rnd)
            rndANOVA <- kruskalmc(RookieAV~Pos , data = posDF)
            rndANOVA <- data.frame(rndANOVA$dif.com)
            rndANOVA[order(rndANOVA$difference,rndANOVA$obs.dif,decreasing = TRUE),]
        }
    })
    
    #Player Probabilities
    output$PlayerProbTable <- renderPrint({
      pos <- rookieDealAV[which(rookieDealAV$PlayerID == input$ProbSelect),"Pos"]
      AV <- rookieDealAV[which(rookieDealAV$PlayerID == input$ProbSelect),"RookieAV"]
      df_player_prob <- rookieDealAV %>% filter(Pos == pos) %>% group_by(Classifier) %>% 
        dplyr::summarize(Avg = median(RookieAV),SD = sd(RookieAV)) #get all of them then delete dupes
      df_player_prob <- data.frame(df_player_prob)
      df_player_prob <- df_player_prob %>% arrange(match(Classifier,c("Top 5","6-15","16-32","33-48","49-64","3","4","5","6","7")))
      df_player_prob$Probability <- apply(df_player_prob[,2:3], 1, function(x) pnorm(AV,x[1],x[2],lower.tail = FALSE))
      df_player_prob$Probability <- label_percent()(df_player_prob$Probability)
      names(df_player_prob) <- c("Section_of_Draft","Avg","SD","Probability")
      df_player_prob[,c("Section_of_Draft","Probability")]
    })

# Tables - Server ---------------------------------------------------------
#* Players Table - Server --------------------------------------------------
    #Players Table
    output$PlayerTable <-  DT::renderDataTable(DT::datatable({
        playerData <- rookieDealAVNon[,c('Player','DraftYear','DraftTm','Pos','Rnd','Pick','Classifier','RookieAV','MedianAV','dAV','Grade')]
        levels(playerData$Grade) <- c("Bust","Poor Pick","Below Average Pick","Average Pick","Good Pick","Great Pick","Steal","Strategic Pick")
        playerData <- playerData[which(playerData$DraftYear >= input$draftYrGlobal[1] &
                                         playerData$DraftYear <= input$draftYrGlobal[2]),]
        if (input$team != "ALL"){
            playerData <- subset(playerData, playerData$DraftTm == input$team)
        }
        if (input$posPlayers != "ALL"){
            playerData <- subset(playerData, playerData$Pos == input$posPlayers)
        }
        if (input$rndPlayers != "ALL"){
            playerData <- subset(playerData, playerData$Rnd == input$rndPlayers)
        }
        if (input$draftYr != "ALL"){
            playerData <- subset(playerData, playerData$DraftYear == input$draftYr)
        }
        playerData
    },rownames = FALSE,options = list(columnDefs = 
                                        list(list(className = 'dt-center', 
                                                  targets = 1:9))),colnames = c("Player","Draft Year","Draft Team", "Pos", "Round","Pick","Classifier","Actual AV","xAV","dAV","Grade"),caption = "AV: Approximate Value | xAV: Expected Approximate Value | dAV: Actual AV - xAV")
    %>% formatStyle("Grade",color = styleEqual(c("Bust","Poor Pick","Below Average Pick","Average Pick","Good Pick","Great Pick","Steal","Strategic Pick"),c("#4f0101","#b80000","#ff5500","#808080","#04c729","#017016", "#3495eb","#000000")),fontWeight = 'bold')
        )  #Player Table


#* Team / Draft Class Table -----------------------------------------------
    
    output$Draft_Class_Table <- DT::renderDataTable(DT::datatable({
    rookieDealAVNon <- rookieDealAVNon[which(rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                               rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
    ##Draft Grades
      ApplyGroups <- function(x) {
        cut(x, breaks=c(quantile(draftClass$dAV, probs = c(0,0.05,0.25,0.40,0.60,0.75,0.95,1.00))), 
            labels=c("Terrible Draft","Poor Draft","Below Average Draft","Average Draft","Good Draft","Great Draft","Amazing Draft"), include.lowest=TRUE)
      }

    #colnames(rookieDealAVNon) <-  c("Pick","Classifier","PlayerID","Player","Draft Year","DraftTm","Pos","Rnd","RookieAV","G","GS","AvgAV","LastSeason","Seasons","PlayerName","YearsSinceDrafted","MedianAV","dAV","Grade") 
    #Remove 2000 since Tom Brady is only played in the dataset from that draft class
      #No Grouping  
    if(length(input$radio_draft_class) == 0 || all("Yr" ==  input$radio_draft_class)){
      draftClass <- rookieDealAVNon[which(rookieDealAVNon$DraftYear != 2000),] %>% group_by(DraftYear) %>% dplyr::summarize(dAV = round(sum(dAV),0))
      draftClass <- rank_in_group2(data = draftClass, arrange_var = dAV)
      colDraftClass<- c("Draft Year","Total dAV","Draft Ranking","Grade")
      draftClass$Grade <- sapply(draftClass$dAV,ApplyGroups)  
      names(draftClass)<- colDraftClass
    }  
      #By Pos,Team & Yr
      else if (all(c("Team","Pos","Yr") == input$radio_draft_class)){
        draftClass <- rookieDealAVNon[which(rookieDealAVNon$DraftYear != 2000),] %>% group_by(DraftTm,Pos, DraftYear) %>% dplyr::summarize(dAV = round(sum(dAV),0))
        draftClass <- rank_in_group2(data = draftClass,group_var = Pos, arrange_var = dAV)
        colDraftClass <- c("Draft Team","Position","Draft Year","Total dAV","Draft Ranking","Grade")
        draftClass$Grade <- sapply(draftClass$dAV,ApplyGroups)  
        names(draftClass)<- colDraftClass
      }
      #By Pos & Yr
      else if (all(c("Pos","Yr") == input$radio_draft_class)){
        draftClass <- rookieDealAVNon[which(rookieDealAVNon$DraftYear != 2000),] %>% group_by(Pos, DraftYear) %>% dplyr::summarize(dAV = round(sum(dAV),0))
        draftClass <- rank_in_group2(data = draftClass,group_var = DraftYear, arrange_var = dAV)
        colDraftClass <- c("Position","Draft Year","Total dAV","Draft Ranking","Grade")
        draftClass$Grade <- sapply(draftClass$dAV,ApplyGroups)  
        names(draftClass)<- colDraftClass
      }
      #By Pos,Team
      else if (all(c("Team","Pos") == input$radio_draft_class)){
        draftClass <- rookieDealAVNon[which(rookieDealAVNon$DraftYear != 2000),] %>% group_by(DraftTm,Pos) %>% dplyr::summarize(dAV = round(sum(dAV),0))
        draftClass <- rank_in_group2(data = draftClass,group_var = Pos, arrange_var = dAV)
        colDraftClass <- c("Draft Team","Position","Total dAV","Draft Ranking","Grade")
        draftClass$Grade <- ""
        names(draftClass)<- colDraftClass
      }    
      #By Position
      else if (all("Pos" ==  input$radio_draft_class)){
        draftClass <- rookieDealAVNon[which(rookieDealAVNon$DraftYear != 2000),] %>% group_by(Pos) %>% dplyr::summarize(dAV = round(sum(dAV),0))
        draftClass <- rank_in_group2(data = draftClass, arrange_var = dAV)
        draftClass$Grade <- ""
        colDraftClass <- c("Position","Total dAV","Draft Ranking","Grade")
        names(draftClass)<- colDraftClass
      }      
      #By Team
      else if (all("Team" == input$radio_draft_class)){
        draftClass <- rookieDealAVNon[which(rookieDealAVNon$DraftYear != 2000),] %>% group_by(DraftTm) %>% dplyr::summarize(dAV = round(sum(dAV),0))
        draftClass <- rank_in_group2(data = draftClass, arrange_var = dAV)
        draftClass$Grade <- ""
        colDraftClass <- c("Draft Team","Total dAV","Rank","Grade")
        names(draftClass)<- colDraftClass
      }
      #By Team & Yr
      else if (all(c("Team","Yr") == input$radio_draft_class)){
        draftClass <- rookieDealAVNon[which(rookieDealAVNon$DraftYear != 2000),] %>% group_by(DraftTm, DraftYear) %>% dplyr::summarize(dAV = round(sum(dAV),0))
        draftClass <- rank_in_group2(data = draftClass,group_var = DraftYear, arrange_var = dAV)
        colDraftClass <- c("Draft Team","Draft Year","Total dAV","Draft Ranking","Grade")
        draftClass$Grade <- sapply(draftClass$dAV,ApplyGroups)  
        names(draftClass)<- colDraftClass
      }
      #draftClass$Grade <- sapply(draftClass$dAV,ApplyGroups)  
      #names(draftClass)<- colDraftClass
      draftClass
      
    },rownames = FALSE,filter = "top",options = list(columnDefs = 
                                                       list(list(className = 'dt-center', 
                                                                 targets = "_all"))))
    %>% formatStyle("Grade",color = styleEqual(c("Terrible Draft","Poor Draft","Below Average Draft","Average Draft","Good Draft","Great Draft","Amazing Draft"),c("#4f0101","#b80000","#ff5500","#808080","#04c729","#017016", "#3495eb")),fontWeight = 'bold')
    )#Draft Class Table
    
# Dashboards - Server -----------------------------------------------------

# *Players Dashboard - Server ----------------------------------------------
    #Team Logos
    URLTeam <- reactive({
        a <- nflLogos[which(nflLogos$team_code == input$teamDash),4]
        return(a)
    })
    output$logo <- renderText({ c('<img src="',URLTeam(),'">')})
    
    ##Subset by team,

# **Best Pick ---------------------------------------------------------------
    #PlayerName        
    NameBest <- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash & 
                                   rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                   rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        a <- a[which.max(a$dAV),]
        return(c(a$Player,", ",a$Pos))
    })
    
    output$DashBestName <- renderText({NameBest()})
    
    #Player Photo
    URLBestPlayer <- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash & 
                                         rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        a <- a[which.max(a$dAV),]
        #rvPlayerName <- gsub("[.]","",a$Player)
        #playerName <- gsub(" ","-",rvPlayerName)
        #return(playerName)
        headshotURL <- a$headshot_url
        if(httr::http_error(headshotURL)){
          headshotURL <- "https://static.www.nfl.com/image/private/f_auto,q_auto,w_250,h_200/league/j9utxwp9846osapesksk.png"}
        return(headshotURL)
    })
    
        ##Create URL
    #url1 <- "https://tsnimages.tsn.ca/ImageProvider/PlayerHeadshot?seoId="
    url2 <- "&w=250&h=200"
    
    output$headshotBest <- renderText({
      if(grepl("espn",URLBestPlayer())){
        c('<img src="',paste(URLBestPlayer(),url2,sep = ""),'">')
      }else{
        c('<img src="',URLBestPlayer(),'">')
      }
        
    })
    
    #DraftYear       
    bestYr <- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash & 
                                         rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        a <- a[which.max(a$dAV),]
        return(c("Draft Year: ",a$DraftYear))
    })
    
    output$DashBestYr <- renderText({bestYr()})
    
    #Rnd & Pick       
    bestRndPick <- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash & 
                                         rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        a <- a[which.max(a$dAV),]
        return(c("Round: ",a$Rnd,", Pick: ",a$Pick))
    })
    
    output$DashBestRndPick <- renderText({bestRndPick()})
    
    ##AV
    bestAV<- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash & 
                                         rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        a <- a[which.max(a$dAV),]
        return(c("Actual AV: ",a$RookieAV))
    })
    
    output$DashBestAV <- renderText({bestAV()})
    
    ##xAV
    bestxAV<- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash & 
                                         rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        a <- a[which.max(a$dAV),]
        return(c("Expected AV: ",a$MedianAV))
    })
    
    output$DashBestxAV <- renderText({bestxAV()})
    
    ##dAV
    bestdAV<- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash & 
                                         rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        a <- a[which.max(a$dAV),]
        colourGrade <- colors[which(colors$Grade == a$Grade),2]
        return(paste("Difference from expected AV: ","<font color=",colourGrade,"><b>", a$dAV, "</b></font>"))
    })
    
    output$DashBestdAV <- renderText({bestdAV()})
    
    #Draft Grade
    bestGrade <- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash & 
                                         rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        a <- a[which.max(a$dAV),]
        colourGrade <- colors[which(colors$Grade == a$Grade),2]
        return(paste("Draft Grade: ","<font color=",colourGrade,"><b>", as.character(a$Grade), "</b></font>"))

    })
    
    output$DashBestGrade <- renderText({bestGrade()})

# **Worst Pick --------------------------------------------------------------
    #PlayerName        
    NameWorst <- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] &
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash & 
                                         rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        a <- a[which.min(a$dAV),]
        return(c(a$Player,", ",a$Pos))
    })
    
    output$DashWorstName <- renderText({NameWorst()})
    
    #Player Photo
    URLWorstPlayer <- reactive({
      if(input$teamDash == "ALL"){
        a <- rookieDealAVNon[which(rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] &
                                     rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
      }else{        
        a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash & 
                                     rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                     rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
      }
      if(input$posTeam != "ALL"){
        a <- a[which(a$Pos == input$posTeam),]
      }
      if(input$draftYrTeam != "ALL"){
        a <- a[which(a$DraftYear == input$draftYrTeam),]
      }
      a <- a[which.min(a$dAV),]
      #rvPlayerName <- gsub("[.]","",a$Player)
      #playerName <- gsub(" ","-",rvPlayerName)
      #return(playerName)
      headshotURL <- a$headshot_url
      if(httr::http_error(headshotURL)){
        headshotURL <- "https://static.www.nfl.com/image/private/f_auto,q_auto,w_250,h_200/league/j9utxwp9846osapesksk.png"}
      return(headshotURL)
    })
    
    ##Create URL
    #url1 <- "https://tsnimages.tsn.ca/ImageProvider/PlayerHeadshot?seoId="
    url2 <- "&w=250&h=200"
    
    output$headshotWorst <- renderText({
      if(grepl("espn",URLWorstPlayer())){
        c('<img src="',paste(URLWorstPlayer(),url2,sep = ""),'">')
      }else{
        c('<img src="',URLWorstPlayer(),'">')
      }
      
    })
    
    #DraftYear       
    WorstYr <- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] &
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash & 
                                         rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        a <- a[which.min(a$dAV),]
        return(c("Draft Year: ",a$DraftYear))
    })
    
    output$DashWorstYr <- renderText({WorstYr()})
    
    #Rnd & Pick       
    WorstRndPick <- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] &
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash & 
                                         rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        a <- a[which.min(a$dAV),]
        return(c("Round: ",a$Rnd,", Pick: ",a$Pick))
    })
    
    output$DashWorstRndPick <- renderText({WorstRndPick()})
    
    ##AV
    WorstAV<- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] &
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash & 
                                         rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        a <- a[which.min(a$dAV),]
        return(c("Actual AV: ",a$RookieAV))
    })
    
    output$DashWorstAV <- renderText({WorstAV()})
    
    ##xAV
    WorstxAV<- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] &
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash & 
                                         rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        a <- a[which.min(a$dAV),]
        return(c("Expected AV: ",a$MedianAV))
    })
    
    output$DashWorstxAV <- renderText({WorstxAV()})
    
    ##dAV
    WorstdAV<- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] &
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash & 
                                         rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        a <- a[which.min(a$dAV),]
        colourGrade <- colors[which(colors$Grade == a$Grade),2]
        return(paste("Difference from expected AV: ","<font color=",colourGrade,"><b>", a$dAV, "</b></font>"))
    })
    
    output$DashWorstdAV <- renderText({WorstdAV()})
    
    #Draft Grade
    WorstGrade <- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] &
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash & 
                                         rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        a <- a[which.min(a$dAV),]
        colourGrade <- colors[which(colors$Grade == a$Grade),2]
        return(paste("Draft Grade: ","<font color=",colourGrade,"><b>", as.character(a$Grade), "</b></font>"))
    })
    
    output$DashWorstGrade <- renderText({WorstGrade()})
    
    ##BestGraph
    UpperLimit <- 20
    BestGraph <- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash & 
                                         rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        a <- a[which.max(a$dAV),]
        a <- unique(a$PlayerID)
        playerSubset <- nflData[which(nflData$PlayerID == a),]
        playerSubset <- playerSubset[order(playerSubset$Season),]
        playerSubset <- playerSubset[c(1:8),]
        playerSubset <-playerSubset[complete.cases(playerSubset),]
        playerSubsetScale <- playerSubset
        playerSubset$AV <- ifelse(playerSubset$AV <= 0,1,playerSubset$AV)
        #playerSubset$AV <- ifelse(playerSubset$AV > 20, 20,playerSubset$AV)
        gg <- ggplot(playerSubset,aes(x = as.character(Season),y = AV,fill = as.factor(AV)))+ geom_bar(stat = "identity",width = 0.60) + scale_fill_manual(values = as.vector(factor(subset(colorsBar,AV %in% playerSubset$AV)$color))) + scale_y_continuous(limits = c(0,ifelse(max(playerSubset$AV) > 20,max(playerSubset$AV),UpperLimit))) + xlab("Year") + ylab("Approximate Value" )+ ggtitle("Actual Performance - Max 8 Seasons")+ theme(legend.position="none") + geom_image(aes(image = url), size = 0.075)
        return (gg)
    })
    output$BestGraph <- renderPlot({BestGraph()})
    
    ##WorstGraph
    WorstGraph <- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] &
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash & 
                                         rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] & 
                                         rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        }
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        a <- a[which.min(a$dAV),]
        a <- unique(a$PlayerID)
        playerSubset <- nflData[which(nflData$PlayerID == a),]
        playerSubset <- playerSubset[order(playerSubset$Season),]
        playerSubset <- playerSubset[c(1:8),]
        playerSubset <-playerSubset[complete.cases(playerSubset),]
        playerSubset$AV <- ifelse(playerSubset$AV <= 0,1,playerSubset$AV)
        #playerSubset$AV <- ifelse(playerSubset$AV > 15, 15,playerSubset$AV)
        gg <-  ggplot(playerSubset,aes(x = as.character(Season),y = AV,fill = as.factor(AV)))+ geom_bar(stat = "identity",width = 0.60) + scale_fill_manual(values = as.vector(factor(subset(colorsBar,AV %in% playerSubset$AV)$color))) + scale_y_continuous(limits = c(0,ifelse(max(playerSubset$AV) > 20,max(playerSubset$AV),UpperLimit))) + xlab("Year") + ylab("Approximate Value" )+ ggtitle("Actual Performance - Max 8 Seasons")+ theme(legend.position="none")+ geom_image(aes(image = url), size = 0.075)
        return (gg)
    })
    output$WorstGraph <- renderPlot({WorstGraph()})
    
    #Team Value Boxes
    draftRanking <- reactive({
        a <- rookieDealAVNon[which(rookieDealAVNon$DraftYear >= input$draftYrGlobal[1] &
                                     rookieDealAVNon$DraftYear <= input$draftYrGlobal[2]),]
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        draftRanking <- aggregate(dAV~DraftTm, data = a, FUN = sum)
        draftRanking <- merge(draftRanking,aggregate(RookieAV~DraftTm, data = a , FUN = sum),by = "DraftTm")
        rownames(draftRanking) <- NULL
        draftRanking <- draftRanking[order(draftRanking$dAV,decreasing = T),]
        return(draftRanking)
    })

    output$NFLRanking <- renderValueBox({valueBox(ordinal(as.numeric(which(draftRanking()$DraftTm == input$teamDash))),tags$p(paste(input$teamDash," League Ranking"), style = "font-size: 150%;"),icon = icon("list-ol"),color = ifelse(input$teamDash != "ALL",as.character(colorsRank[which(colorsRank$Rank == which(draftRanking()$DraftTm == input$teamDash)),2]),'blue'))})
    output$teamdAV <- renderValueBox({valueBox(draftRanking()[which(draftRanking()$DraftTm == input$teamDash),2],tags$p(paste(input$teamDash,": Total Difference from Expected AV"),style = "font-size: 150%;"),icon = icon("trophy"),color = ifelse(input$teamDash != "ALL",as.character(colorsRank[which(colorsRank$Rank == which(draftRanking()$DraftTm == input$teamDash)),2]),'blue'))})
    output$teamAV <- renderValueBox({valueBox(draftRanking()[which(draftRanking()$DraftTm == input$teamDash),3],tags$p(paste(input$teamDash,": Actual AV"),style = "font-size: 150%;"),icon = icon("football-ball"),color = ifelse(input$teamDash != "ALL",as.character(colorsRank[which(colorsRank$Rank == which(draftRanking()$DraftTm == input$teamDash)),2]),'blue'))})
    

# HyperLinks - Server -----------------------------------------------------
    url <- a("Click ", href="https://www.pro-football-reference.com/blog/index37a8.html")
    output$AVLink <- renderUI({
        tagList("AV Explanation:", url)
    })
    
    urlGit <- a("Application Code ", href="https://github.com/paulg66/NFL_Draft")
    output$GitHubLink <- renderUI({
        tagList(urlGit)
    })
    
    urlReddit <- a("u/paulg66 ", href = "https://www.reddit.com/user/paulg66")
    output$RedditLink <- renderUI({
      tagList("Created and Maintained by Paul Gallagher - ",urlReddit)
    })    
    
    urlFeedback <- a("Feedback Form",href = "https://forms.gle/9LSJ2ze78fWMcgnK6")
    output$FeedbackLink <- renderUI({
      tagList(urlFeedback)
    })
    
# Draft Optimizer ---------------------------------------------------------
    #By Position ----------------------------------------------------------
    
    #Reactable  -----------------------------------------------------------
    colors_palette_avg <- c("#e24115","#fe9d00","#fdce02","#f0f2f5","#77c000","#019a48")
    output$PosTableOptimizer <- renderReactable({
        posProbTable <- subset(starter_table, starter_table$Pos == input$posOptimizer)
      reactable(
        posProbTable,
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
                                  posProbTable,
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
                          posProbTable,
                          number_fmt = scales::percent_format(accuracy = 0.1),
                          colors = colors_palette_avg,
                          box_shadow = TRUE,
                          span = c("Avg8","Avg4","Avg2","AvgB","AvgN"))
          ),#Avg8
          
          # 4 - Year Starter
          Probability4 = colDef(name = 'Probability',
                                align = 'left',
                                cell = data_bars(
                                  posProbTable,
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
                                  posProbTable,
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
                                  posProbTable,
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
                                  posProbTable,
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
    })#Pos Table Optimizer
    
    #formattable - PlayerArchetypes
    names(player_archetypes_sum) <- c("Player Type","Name","Draft Team","Pos","Total_AV","AV by Season")
    output$PlayerArchetypes <- renderUI({
      player_archetypes_pos <- subset(player_archetypes_sum, player_archetypes_sum$Pos == input$posOptimizer)
      player_archetypes_pos  %>% formattable() %>%  formattable::as.htmlwidget() %>% spk_add_deps()
    })#formattable - PlayerArchetypes
    
}

# Run the application 
shinyApp(ui = ui, server = server)
