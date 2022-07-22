#Load in the required libraries
library(tidyverse)
library("readxl")
library(shiny)
library(reactable)

#We read in our data before our UI so that we can use the data to show UI's choices in
#lists, drop-down, etc
dispdata <- read_xlsx(".//data//newTry.xlsx") %>%
  na.omit()

#This is the UI - the page that the user sees
UI = fluidPage(
  #Adding a title to the page
  titlePanel("Adding Interactive GUI with Reactive Tables in R"),
  
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
    
    #Here we filter our original data with the age that the user selected,
    #the league, nation, and player
    
    #Then we finally select the columns that we want and that's it!
    view <- alldata %>% 
      filter(Age >= input$ager[1]) %>%
      filter(Age <= input$ager[2]) %>%
      filter(Comp %in% leagueFinder) %>%
      filter(Nation %in% nationFinder) %>%
      filter(Player %in% playerFinder)%>%
      #If you want different columns, feel free to add a column name from 
      #the Excel spreadsheet. Simply add a comma and put the column name
      #in parentheses as shown here:
      
      select(c("Player", "Squad", "Pos", "Age",
               "TouchesAttThird","TouchesAttPen","LiveTouches","AttDribble",
               "DribblesPerTouch","Carries"))
    
    #This is where we create our reactive table
    reactable(
      #Input our table
      view, 
      
      #We want to dynamically filter our table
      filterable = TRUE,
      
      #On one page, we only want 20 rows to be seen
      defaultPageSize = 20,
      
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