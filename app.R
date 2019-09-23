# Shiny R Application - NFL Draft
# By: Paul Gallagher


library(shiny)
library(ggplot2)
library(reshape)
library(DT)
library(shinydashboard)
library(scales)
library(pgirmess)
library(dplyr)


#Load Data from Dropbox
nflData <- read.csv("https://www.dropbox.com/s/p4lfzkaib1fktu2/NFLData.csv?dl=1",stringsAsFactors = FALSE)
rookieDealAV <- read.csv("https://www.dropbox.com/s/datmk8ea7y3p26s/rookieDealAV.csv?dl=1",stringsAsFactors = FALSE)
rookieDealAVNon <- read.csv("https://www.dropbox.com/s/uuio8ddbeermk0o/rookieDealAVNon.csv?dl=1",stringsAsFactors = FALSE)
nflLogos <- read.csv("https://www.dropbox.com/s/x4exm1ee0jrr50j/NFLLogos.csv?dl=1",stringsAsFactors = FALSE)

PosSummary <- rookieDealAV %>% group_by(Rnd,Pos) %>% summarize(MedianAV = median(RookieAV))
ClassifierSummary <- rookieDealAV %>% group_by(Rnd) %>% summarize(MedianAV = median(RookieAV),n = n())

##Colors for Bar Graph
colfunc<-colorRampPalette(c("red","yellow","green"))
colorsBar <- data.frame(AV = seq(1:15),color = colfunc(15))

##Colors for League Rankings
colorsRank <- data.frame(Rank = seq(1:32),color = 'red')
colorsRank$color <- ifelse(colorsRank$Rank > 10 & colorsRank$Rank <= 20,'yellow','red')
colorsRank$color <- ifelse(colorsRank$Rank <= 10,'green',colorsRank$color)

# Define UI for application that draws a histogram
ui <- navbarPage("NFL Draft",
                 
                 tabPanel("Teams",dashboardPage(
                     
                     #Page Title         
                     dashboardHeader(title = "NFL Teams"),
                     
                     #DropDown for Team
                     dashboardSidebar(selectInput("teamDash","Team",c("ALL",unique(rookieDealAV$DraftTm)[sort.list(unique(rookieDealAV$DraftTm))]),selected = "ARI"),htmlOutput("logo")),
                     dashboardBody(fluidRow(
                         column(4,selectInput("posTeam","Position",list("ALL","QB","RB","WR","TE","T","G","C","DT","EDGE","LB","DB","K"))),
                         column(4,selectInput("draftYrTeam","Draft Year",c("ALL",unique(rookieDealAV$DraftYear)[sort.list(unique(rookieDealAV$DraftYear))])))),
                         fluidRow(valueBoxOutput("NFLRanking"),valueBoxOutput("teamdAV"),valueBoxOutput("teamAV")),
                         fluidRow(
                             box(title = "Best Draft Pick",status = "success",solidHeader = TRUE, fluidRow(column(5,htmlOutput("headshotBest"),h3(textOutput("DashBestName")),h4(htmlOutput("DashBestGrade")),h4(textOutput("DashBestYr")),h4(textOutput("DashBestRndPick")),br(),h4(textOutput("DashBestAV")),h4(textOutput("DashBestxAV")),h4(htmlOutput("DashBestdAV"))),column(7,plotOutput({"BestGraph"})))),
                             box(title = "Worst Draft Pick",status = "danger", solidHeader = TRUE,fluidRow(column(5,htmlOutput("headshotWorst"),h3(textOutput("DashWorstName")),h4(htmlOutput("DashWorstGrade")),h4(textOutput("DashWorstYr")),h4(textOutput("DashWorstRndPick")),br(),h4(textOutput("DashWorstAV")),h4(textOutput("DashWorstxAV")),h4(htmlOutput("DashWorstdAV"))),column(7,plotOutput({"WorstGraph"}))))
                             
                         ),#fluidRow draft picks
                         "AV = Approximate Value",br(),"Players Drafted From 2001-2018",br(),"Data Source: www.pro-football-reference.com",br(),br(),"Created and Maintained by Paul Gallagher (@PaulGallagher12)",br(),uiOutput('GitHubLink'))#body
                 )),#Teams   
                 
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
                    selectInput("draftYr","Draft Year",c("ALL",unique(rookieDealAV$DraftYear)[sort.list(unique(rookieDealAV$DraftYear))]))
                              )
                ),#fluid Row 
        DT::dataTableOutput("PlayerTable")
                          
    ),#Players  

    tabPanel("Statistical Tests",navbarPage("",
    tabPanel("ANOVA - By Position",             

    # Application title
    titlePanel("NFL Draft - By Position"),

    # Dropdown for Pos
    sidebarLayout(
        sidebarPanel(
            selectInput("pos",
                        label = "Selection Position",
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
                        label = "Selection Round",
                        choices = list("ALL",1,2,3,4,5,6,7)
            ),verbatimTextOutput("RndTable"),h6("Kruskal-Wallis Test - p-Value: 0.05")
        ),
        # Show boxplot
        mainPanel(plotOutput("RndGraph"))
    )
    ))##tabpanel By round

    ),tabPanel("Methodology",fluidPage(includeMarkdown("https://www.dropbox.com/s/eerqntvsbn887ge/Shiny%20Writeup.Rmd?dl=1"),hr(),uiOutput('AVLink'),br()))#Navbar, statistical tests
)

# SERVER START______________________________________________________________________________________________________________
server <- function(input, output) {

    colors <- data.frame("Grade" = c("Bust","Poor Pick","Below Average Pick","Average Pick","Good Pick","Great Pick","Steal"),"ColorCode" = c("#4f0101","#b80000","#ff5500","#808080","#04c729","#017016", "#3495eb"))
    
    ##By Position Graph
    output$PosGraph <- renderPlot({
        if (input$pos == "ALL"){
            print(ggplot(rookieDealAV, aes(rookieDealAV$Rnd, rookieDealAV$RookieAV,fill = as.factor(Rnd))) + geom_boxplot() + xlab("Round") + ylab("Sqrt(Approximate Value)") + ggtitle("Round vs Approximate Value")) 
        }else{
            posDF <- subset(rookieDealAV, Pos == input$pos)
            print(ggplot(posDF, aes(posDF$Rnd, posDF$RookieAV,fill = as.factor(Rnd))) + geom_boxplot() + xlab("Round") + ylab("Sqrt(Approximate Value)") + ggtitle(paste("Round vs Approximate Value - Position:", input$pos))) 
        }

    })
    
    ##By Position Vs Average
    output$PosAvgGraph <- renderPlot({
        if (input$pos == "ALL"|input$pos == "K"){
            
        }else{
            ggSubset <- subset(PosSummary, Pos == input$pos)
            ggSubset$RndAvg <- ClassifierSummary$MedianAV
            ggSubset <- melt(as.data.frame(ggSubset),names(ggSubset)[1:2])
            ggplot(ggSubset, aes(x = Rnd, y = value, group = variable, colour = variable)) + geom_point(size = 2) + geom_line() + scale_colour_manual(name = "Pick Type", labels = c(input$pos,"All Pos Avg"),values = c("blue","red")) + ggtitle(paste("All Position Average Vs. Average for: ", input$pos)) + xlab("Round") + ylab("Sqrt(Approximate Value)")
        }
    })
    
    
    ##By Round Graph
    output$RndGraph <- renderPlot({
        if (input$rnd == "ALL"){
            ggplot(subset(rookieDealAV),aes(x = reorder(Pos,RookieAV, FUN = median), y = RookieAV, fill = Pos)) + geom_boxplot() + xlab("Position") + ylab("Sqrt(Approximate Value)") + ggtitle("Position vs AV")
        }else{
            posDF <- subset(rookieDealAV, Pos == input$rnd)
            ggplot(subset(rookieDealAV, Rnd == input$rnd),aes(x = reorder(Pos,RookieAV, FUN = median), y = RookieAV, fill = Pos)) + geom_boxplot() + xlab("Position") + ylab("Sqrt(Approximate Value)") + ggtitle("Position vs AV")
        }
        
    })
    
    ##By Position - ANOVA
    output$PosTable <- renderPrint({
        if (input$pos == "ALL"){
            posANOVA <- kruskalmc(RookieAV~Rnd , data = rookieDealAV)
            posANOVA$dif.com
        }else{
            posDF <- subset(rookieDealAV, Pos == input$pos)
            posANOVA <- kruskalmc(RookieAV~Rnd , data = posDF)
            posANOVA$dif.com
        }
    })
    
    ##By Round - ANOVA
    output$RndTable <- renderPrint({
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
    
    #Players Table
    output$PlayerTable <-  DT::renderDataTable(DT::datatable({
        playerData <- rookieDealAVNon[,-1]
        playerData <- playerData[,c(4:8,1,2,9,16,17,18)] 
        levels(playerData$Grade) <- c("Bust","Poor Pick","Below Average Pick","Average Pick","Good Pick","Great Pick","Steal")
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
    },rownames = FALSE,colnames = c("Player","Draft Year","Draft Team", "Pos", "Round","Pick","Classifier","Rookie Contract AV","xAV","dAV","Grade"),caption = "AV = Approximate Value  |  Rookie Contract = 4 years")
    %>% formatStyle("Grade",color = styleEqual(c("Bust","Poor Pick","Below Average Pick","Average Pick","Good Pick","Great Pick","Steal"),c("#4f0101","#b80000","#ff5500","#808080","#04c729","#017016", "#3495eb")),fontWeight = 'bold')
        )  #Player Table
    
    #Dashboard
    #Team Logos
    URLTeam <- reactive({
        a <- nflLogos[which(nflLogos$team_code == input$teamDash),4]
        return(a)
    })
    output$logo <- renderText({ c('<img src="',URLTeam(),'">')})
    
    ##Subset by team,
        #Best Pick
    
    #PlayerName        
    NameBest <- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash),]
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
            a <- rookieDealAVNon
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash),]
        }
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        a <- a[which.max(a$dAV),]
        rvPlayerName <- gsub("[.]","",a$Player)
        playerName <- gsub(" ","-",rvPlayerName)
        return(playerName)
    })
    
        ##Create URL
    url1 <- "https://tsnimages.tsn.ca/ImageProvider/PlayerHeadshot?seoId="
    url2 <- "&width=200&height=200"
    
    output$headshotBest <- renderText({
        c('<img src="',paste(url1,URLBestPlayer(),url2,sep = ""),'">')
    })
    
    #DraftYear       
    bestYr <- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash),]
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
            a <- rookieDealAVNon
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash),]
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
            a <- rookieDealAVNon
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash),]
        }
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        a <- a[which.max(a$dAV),]
        return(c("AV, 1st Four Yrs: ",a$RookieAV))
    })
    
    output$DashBestAV <- renderText({bestAV()})
    
    ##xAV
    bestxAV<- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash),]
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
            a <- rookieDealAVNon
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash),]
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
            a <- rookieDealAVNon
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash),]
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
    
    #Worst Pick_____________________________________
    
    #PlayerName        
    NameWorst <- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash),]
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
            a <- rookieDealAVNon
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash),]
        }
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        a <- a[which.min(a$dAV),]
        rvPlayerName <- a$Player
        playerName <- gsub(" ","-",rvPlayerName)
        return(playerName)
    })
    
    
    ##Create URL
    url1 <- "https://tsnimages.tsn.ca/ImageProvider/PlayerHeadshot?seoId="
    url2 <- "&width=200&height=200"
    
    output$headshotWorst <- renderText({
        c('<img src="',paste(url1,URLWorstPlayer(),url2,sep = ""),'">')
    })
    
    #DraftYear       
    WorstYr <- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash),]
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
            a <- rookieDealAVNon
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash),]
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
            a <- rookieDealAVNon
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash),]
        }
        if(input$posTeam != "ALL"){
            a <- a[which(a$Pos == input$posTeam),]
        }
        if(input$draftYrTeam != "ALL"){
            a <- a[which(a$DraftYear == input$draftYrTeam),]
        }
        a <- a[which.min(a$dAV),]
        return(c("AV, 1st Four Yrs: ",a$RookieAV))
    })
    
    output$DashWorstAV <- renderText({WorstAV()})
    
    ##xAV
    WorstxAV<- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash),]
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
            a <- rookieDealAVNon
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash),]
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
            a <- rookieDealAVNon
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash),]
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
    BestGraph <- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash),]
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
        playerSubset <- playerSubset[order(playerSubset$Year),]
        playerSubset <- playerSubset[c(1:4),]
        playerSubset <-playerSubset[complete.cases(playerSubset),]
        playerSubsetScale <- playerSubset
        playerSubset$AV <- ifelse(playerSubset$AV == 0,1,playerSubset$AV)
        playerSubset$AV <- ifelse(playerSubset$AV > 15, 15,playerSubset$AV)
        gg <- ggplot(playerSubset,aes(x = as.character(playerSubset$Year),y = playerSubsetScale$AV,fill = as.factor(playerSubset$AV)))+ geom_bar(stat = "identity",width = 0.60) + scale_fill_manual(values = as.vector(factor(subset(colorsBar,AV %in% playerSubset$AV)$color))) + scale_y_continuous(limits = c(0,ifelse(max(playerSubsetScale$AV) > 15,max(playerSubsetScale$AV),15))) + xlab("Year") + ylab("Approximate Value" )+ ggtitle("Rookie Contract Performance - First 4 Seasons")+ theme(legend.position="none")
        return (gg)
    })
    output$BestGraph <- renderPlot({BestGraph()})
    
    ##WorstGraph
    WorstGraph <- reactive({
        if(input$teamDash == "ALL"){
            a <- rookieDealAVNon
        }else{        
            a <- rookieDealAVNon[which(rookieDealAVNon$DraftTm == input$teamDash),]
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
        playerSubset <- playerSubset[order(playerSubset$Year),]
        playerSubset <- playerSubset[c(1:4),]
        playerSubset <-playerSubset[complete.cases(playerSubset),]
        playerSubset$AV <- ifelse(playerSubset$AV == 0,1,playerSubset$AV)
        playerSubset$AV <- ifelse(playerSubset$AV > 15, 15,playerSubset$AV)
        gg <-  ggplot(playerSubset,aes(x = as.character(playerSubset$Year),y = playerSubset$AV,fill = as.factor(playerSubset$AV)))+ geom_bar(stat = "identity",width = 0.60) + scale_fill_manual(values = as.vector(factor(subset(colorsBar,AV %in% playerSubset$AV)$color))) + scale_y_continuous(limits = c(0,15)) + xlab("Year") + ylab("Approximate Value" )+ ggtitle("Rookie Contract Performance - First 4 Seasons")+ theme(legend.position="none")
        return (gg)
    })
    output$WorstGraph <- renderPlot({WorstGraph()})
    
    #Team Value Boxes
    draftRanking <- reactive({
        a <- rookieDealAVNon
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
    output$teamAV <- renderValueBox({valueBox(draftRanking()[which(draftRanking()$DraftTm == input$teamDash),3],tags$p(paste(input$teamDash,": Total Rookie Contract AV"),style = "font-size: 150%;"),icon = icon("football-ball"),color = ifelse(input$teamDash != "ALL",as.character(colorsRank[which(colorsRank$Rank == which(draftRanking()$DraftTm == input$teamDash)),2]),'blue'))})
    
    #Hyperlinks
    url <- a("Click ", href="https://www.pro-football-reference.com/blog/index37a8.html")
    output$AVLink <- renderUI({
        tagList("AV Explanation:", url)
    })
    
    urlGit <- a("Application Code ", href="https://github.com/paulg66/NFL_Draft")
    output$GitHubLink <- renderUI({
        tagList(urlGit)
    })
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)
