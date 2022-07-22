#Load in the required libraries
library(tidyverse)
library("readxl")
library(shiny)
library(reactable)

#New library for similarity index
library(factoextra)

#We read in our data before our UI so that we can use the data to show UI's choices in
#lists, drop-down, etc
dispdata <- read_xlsx(".//data//newTry.xlsx") %>%
  na.omit()

#This is the UI - the page that the user sees
UI = fluidPage(
  #Adding a title to the page
  titlePanel("Implementing Live Metrics Analysis into ShinyApp"),
  
  #Our GUI will be housed in a sidebar layout that will make it easy for the user to access it
  sidebarLayout(
    sidebarPanel(
      #In all options, we give the choices a variable name. This variable name
      #will allow us to access the contents in that variable when we go to our server.
      
      #This option allows us to select a player from a list of players.
      #In our choices, we also give the option "All", allowing us to see ALL players
      selectizeInput("player", "Select a Player", choices = c("All", unique(dispdata$Player)),
                     options = list(maxItems = 1, placeholder = 'Type Player Name')),
      
      #slider input is a cool way to pick ages - slide the number bar between the minimum (16) and maximum (40) limit
      sliderInput("ager", "Age:",
                  min = 16, max = 40,
                  value = c(16,40)),
      
      #Same as selecting players, here we select leagues based on FBRef terminology
      selectizeInput("league", "Select a League", choices = c("All","fr Ligue 1",
                                                              "de Bundesliga",
                                                              "eng Premier League",
                                                              "es La Liga",
                                                              "it Serie A"),
                     options = list(maxItems = 1, placeholder = 'Type League Name')),
      
      #Same as selecting players, here we select nations
      selectizeInput("nation", "Select a Nation", choices = c("All", unique(dispdata$Nation)),
                     options = list(maxItems = 1, placeholder = 'Type League Name')),
      
      #Our button that - when clicked - shows the table
      actionButton("show", label = "Show"),
      
    ),
    #This is the main panel where our reactive table will go
    mainPanel(
      
      #We add some HTML so it fits the page - don't worry you don't need to
      #know this 
      HTML("<div style ='overflow:auto; height:500px; width:1100px' >"),
      #This is our actual table output
      reactableOutput("playerData"),
      HTML("</div>")
    )
  )
)


#This is the server - the part that handles parsing and uploading the table
#so that we can see it

#Here, we add a new parameter - session. This session allows us to control the table
#based on what choices we pick in our UI.
SERVER <- function(input, output, session) {
  
  #Before, we would return a simple datable. Here we return an eventReactive function
  #which means the code inside this function is re-done every time the Show button (that is where input$show comes int)
  #is clicked
  playerTable <- eventReactive(input$show,{
    
    #Read in your data from the data folder and we will do some basic cleaning of data
    alldata <- read_xlsx(".//data//newTry.xlsx") %>%
      #filter(Min >= 1000) %>% #You can choose to filter your table - for example by minutes
      na.omit()
    
    #First, we're going to create a general dataframe with only numbers and ALL columns.
    #This will be our general similarity ranking.
    PCAData <- alldata %>% select(-Rk) %>%
      #We're deselecting these columns because we want our dataframes with ONLY numbers.
      #These columns are redundant and/or have letters
      select(-c("Player","Nation","Pos","Squad","Comp","Age","Born","90s", "G-PK")) %>%
      na.omit() %>%
      #scale() standardizes (puts them on the same scale) the numbers for us
      scale()
    
    #Next, we're going to construct more detailed similarity scores for a section of play.
    #We're going to choose the metrics we want for Passing, Attacking, and Defending
    Passing = c("Pressing","PressDefThird",
                "PressMidThird","PressAttThird",
                "PassAtt","PassLive",	"PassDead",	
                "PassPress",	"PassSwitch",	"PassCross", "PassCK",
                "PassGround",	"PassLow",	"PassHigh",	"PassLeft",
                "PassRight",	"PassHead",	"PassTI",	"PassOther", "PassOff",
                "PassOut",	"PassTotDist",	"PassPrgDist",
                "ShrtAtt",	"MedAtt",	"LongAtt", 
                "PenaltyAreaDirectness", "PassDirectness",
                "AttackCarries", "CarryDirectness",
                "ProgDistPerCarry", "DribblesPerTouch")
    
    Defending = c("Tkl",	"TklDefThird",	"TklMidThird",	"TklAttThird",
                  "Blocks",	"PassBlock",	"Int",	"Clr", "Err")
    
    Attacking = c("TouchesAttThird",	"TouchesAttPen", "AttDribble",
                  "Carries",	"TotDistCarries",	"PrgDistCarries",	
                  "ProgCarries",	"FinalThirdCarries"	,"CPA",
                  "SCADrib",	"SCAShots", "GCADrib", "GCAShots",
                  "CrnIn",	"CrnOut",	"CrnStr",
                  "Sh/90",	"SoT/90",	"DistanceShot",
                  "PTF3",	"PPA",	"CrsPA", "PassTB","KP",	
                  "SCAPassLive",	"GCAPassLive",
                  "npxG", "npxG/Sh")

    #We're then going to select those columns and store them in their respective dataframes
    PassData <- alldata %>% select(-Rk) %>%
      #We're deselecting these columns because we want our dataframes with ONLY numbers.
      #These columns are redundant and/or have letters
      select(-c("Player","Nation","Pos","Squad","Comp","Age","Born","90s")) %>%
      na.omit() %>%
      select(Passing) %>%
      scale()
    
    DefData <- alldata %>% select(-Rk) %>%
      select(-c("Player","Nation","Pos","Squad","Comp","Age","Born","90s")) %>%
      na.omit() %>%
      select(Defending) %>%
      scale()
    
    AttData <- alldata %>% select(-Rk) %>%
      select(-c("Player","Nation","Pos","Squad","Comp","Age","Born","90s")) %>%
      na.omit() %>%
      select(Attacking) %>%
      scale()
    
    #We're going to Principal Component Analysis to reduce all those columns to a set few
    #that are mathematically important and that can still represent the distributions of data
    pca <- prcomp(PCAData)
    pcaPass <- prcomp(PassData)
    pcaDef <- prcomp(DefData)
    pcaAtt <- prcomp(AttData)
    
    pcGeneral <- as.data.frame(pca$x[,1:8]) # extract first eight PCs
    pcPass <- as.data.frame(pcaPass$x[,1:8]) # extract first eight PCs
    pcDef <- as.data.frame(pcaDef$x[,1:8])
    pcAtt <- as.data.frame(pcaAtt$x[,1:8])
    
    #We're going to add important labels back to our PCA-sized dataframes
    pcGeneral <- pcGeneral %>% mutate(Player = alldata$Player) %>%
      mutate(Squad = alldata$Squad) %>%
      mutate(Position = alldata$Pos) %>%
      mutate(Age = alldata$Age)%>%
      mutate(Comp = alldata$Comp) %>%
      mutate(Nation = alldata$Nation) %>%
      mutate(npxG = alldata$npxG)
    
    pcPass <- pcPass %>% mutate(Player = alldata$Player) %>%
      mutate(Squad = alldata$Squad) %>%
      mutate(Position = alldata$Pos) %>%
      mutate(Age = alldata$Age)%>%
      mutate(Comp = alldata$Comp)%>%
      mutate(Nation = alldata$Nation)
    
    pcDef <- pcDef %>% mutate(Player = alldata$Player) %>%
      mutate(Squad = alldata$Squad) %>%
      mutate(Position = alldata$Pos) %>%
      mutate(Age = alldata$Age)%>%
      mutate(Comp = alldata$Comp)%>%
      mutate(Nation = alldata$Nation)
    
    pcAtt <- pcAtt %>% mutate(Player = alldata$Player) %>%
      mutate(Squad = alldata$Squad) %>%
      mutate(Position = alldata$Pos) %>%
      mutate(Age = alldata$Age) %>%
      mutate(Comp = alldata$Comp)%>%
      mutate(Nation = alldata$Nation)
    
    #Now we're going to get the data for the player that was selected by user
    findGeneral <- pcGeneral %>% filter(Player == input$player)
    findPass <- pcPass %>% filter(Player == input$player)
    findDef <- pcDef %>% filter(Player == input$player)
    findAtt <- pcAtt %>% filter(Player == input$player)
    
    
    #We use the variable league - accessed via input$ - and do a simple if-else.
    if(input$league == "All"){
      #If ALL is selected, we define a variable that contains all the leagues
      leagueFinder = c("fr Ligue 1",
                       "de Bundesliga",
                       "eng Premier League",
                       "es La Liga",
                       "it Serie A")
    }
    else{
      #if not, we select the specific league the user selected.
      leagueFinder = input$league
    }
    
    #We repeat this for nation
    if(input$nation == "All"){
      #Notice that when we pick ALL, we store the entire column of data under Nation
      #in the variable. This is because we want ALL the nations under that column.
      #We use unique to get all unique nations 
      nationFinder = alldata$Nation
    }
    else{
      nationFinder = input$nation
    }
    
    #And for player
    if(input$player == "All"){
      #Notice that when we pick ALL, we store the entire column of data under Player
      #in the variable. This is because we want ALL the players under that column.
      #We use unique to get all unique player names 
      playerFinder = unique(alldata$Player)
    }
    else{
      playerFinder = input$player
    }
    
    #Making our dataframes that we will utilize to display to user
    #Use the UI choices to filter our players and then find the similarity score
    #of all players from our selected player
    similar <- pcGeneral %>% 
      filter(Age >= input$ager[1]) %>%
      filter(Age <= input$ager[2]) %>%
      filter(Comp %in% leagueFinder) %>%
      filter(Nation %in% nationFinder) %>%
      #Calculate the euclidean distance which is our similarity score
      mutate(euc = sqrt((PC1 -  findGeneral$PC1)^2+(PC2 - findGeneral$PC2)^2+
                          (PC3 -  findGeneral$PC3)^2+(PC4 - findGeneral$PC4)^2+
                          (PC5 -  findGeneral$PC5)^2+(PC6 -  findGeneral$PC6)^2+
                          (PC7 -  findGeneral$PC7)^2+(PC8 -  findGeneral$PC8)^2))
    
    #Repeating this on the passing score
    pcPass = pcPass %>% 
      filter(Age >= input$ager[1]) %>%
      filter(Age <= input$ager[2]) %>%
      filter(Comp %in% leagueFinder) %>%
      filter(Nation %in% nationFinder) %>%
      mutate(eucPass = sqrt((PC1 -  findPass$PC1)^2+(PC2 - findPass$PC2)^2+
                              (PC3 -  findPass$PC3)^2+(PC4 - findPass$PC4)^2+
                              (PC5 -  findPass$PC5)^2+(PC6 -  findPass$PC6)^2+
                              (PC7 -  findPass$PC7)^2+(PC8 -  findPass$PC8)^2))
    #Merging so we get the similarity scores of every player except our player
    pcPass <- pcPass[ !(pcPass$Player %in% c(input$player)), ]
    
    #Repeating this on the defensive score
    pcDef = pcDef %>% 
      filter(Age >= input$ager[1]) %>%
      filter(Age <= input$ager[2]) %>%
      filter(Comp %in% leagueFinder) %>%
      filter(Nation %in% nationFinder) %>%
      mutate(eucDef = sqrt((PC1 -  findDef$PC1)^2+(PC2 - findDef$PC2)^2+
                             (PC3 -  findDef$PC3)^2+(PC4 - findDef$PC4)^2+
                             (PC5 -  findDef$PC5)^2+(PC6 -  findDef$PC6)^2+
                             (PC7 -  findDef$PC7)^2+(PC8 -  findDef$PC8)^2))
    pcDef <- pcDef[ !(pcDef$Player %in% c(input$player)), ]
    
    
    #Repeating this on the attacking score
    pcAtt = pcAtt %>% 
      filter(Age >= input$ager[1]) %>%
      filter(Age <= input$ager[2]) %>%
      filter(Comp %in% leagueFinder) %>%
      filter(Nation %in% nationFinder) %>%
      mutate(eucAtt = sqrt((PC1 -  findAtt$PC1)^2+(PC2 - findAtt$PC2)^2+
                             (PC3 -  findAtt$PC3)^2+(PC4 - findAtt$PC4)^2+
                             (PC5 -  findAtt$PC5)^2+(PC6 -  findAtt$PC6)^2+
                             (PC7 -  findAtt$PC7)^2+(PC8 -  findAtt$PC8)^2))
    pcAtt <- pcAtt[ !(pcAtt$Player %in% c(input$player)), ]
    
    #We finally select our columns we want to show generally, then select every player
    #except player selected by user
    #and make new columns that contain the general, attacking, passing, and defending scores
    view <- similar %>% 
      select(c("Player", "Squad", "Position", "Age", "euc", "npxG"))
    view <- view[ !(view$Player %in% c(input$player)), ]
    view <- view %>%
      mutate(Rank = rank(euc)) %>%
      mutate(PassScore = pcPass$eucPass) %>%
      mutate(PassScore = rank(PassScore)) %>%
      mutate(DefScore = pcDef$eucDef) %>%
      mutate(DefScore = rank(DefScore)) %>%
      mutate(AttScore = pcAtt$eucAtt) %>%
      mutate(AttScore = rank(AttScore)) %>%
      select(-euc) %>%
      na.omit() %>%
      arrange(Rank)
    
    #This function allows us to color our columns of our similarity scores. The first color
    #is for low values while the last color is for high values with the value in the middle
    #standing for the intermediate color
    red_pal <- function(x) rgb(colorRamp(c("#30a2da", "#e5ae38", "#fc4f30"))(x), maxColorValue = 255)
    
    #This is where we create our reactive table
    reactable(
      #Input our table
      view, 
      
      #We want to dynamically filter our table
      filterable = TRUE,
      
      #On one page, we only want 20 rows to be seen
      defaultPageSize = 20,
      
      #We want to add coloring to our columns so we define those functions for those columns
      columns = list(
        #for rank, we apply the coloring scale
        Rank = colDef(
          style = function(value) {
            normalized <- (value - min(view$Rank)) / (max(view$Rank) - min(view$Rank))
            color <- red_pal(normalized)
            list(background = color)
          }
        ),
        #Do the same for Pass Scores
        PassScore = colDef(
          style = function(value) {
            normalized <- (value - min(view$PassScore)) / (max(view$PassScore) - min(view$PassScore))
            color <- red_pal(normalized)
            list(background = color)
          }
        ),
        #and Defensive Scores
        DefScore = colDef(
          style = function(value) {
            normalized <- (value - min(view$DefScore)) / (max(view$DefScore) - min(view$DefScore))
            color <- red_pal(normalized)
            list(background = color)
          }
        ),
        #and attacking scores
        AttScore = colDef(
          style = function(value) {
            normalized <- (value - min(view$AttScore)) / (max(view$AttScore) - min(view$AttScore))
            color <- red_pal(normalized)
            list(background = color)
          }
        )
      ),
      
      #Establishing a minimal theme for the table. Feel free
      #to input your own colors and have fun!
      theme = reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5f9",
        cellPadding = "8px 12px",
        style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
        searchInputStyle = list(width = "100%")
      )
    )
  })
  
  #We 'render' our table - process and make it ready for being displayed
  #to the user
  output$playerData <- renderReactable({
    #Here we pass our eventReactive function into the rendering and we're done!
    playerTable()
  })
}

# Return a Shiny app object
shinyApp(ui = UI, server = SERVER)