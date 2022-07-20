#Load in the required libraries
library(tidyverse)
library("readxl")
library(shiny)
library(reactable)

#This is the UI - the page that the user sees
UI = fluidPage(
  #Adding a title to the page
  titlePanel("Building Dynamic and Reactive Tables in R"),
  
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


#This is the server - the part that handles parsing and uploading the table
#so that we can see it
SERVER <- function(input, output) {
  
  #Read in your data from the data folder and we will do some basic cleaning of data
    alldata <- read_xlsx(".//data//newTry.xlsx") %>%
      #filter(Min >= 1000) %>% #You can choose to filter your table - for example by minutes
      na.omit()
    
    #We don't want to see all the columns - it's too much!
    #Here we filter by column names
    
    view <- alldata %>% 
      
      #If you want different columns, feel free to add a column name from 
      #the Excel spreadsheet. Simply add a comma and put the column name
      #in parentheses as shown here:
      
      select(c("Player", "Squad", "Pos", "Age",
               "TouchesAttThird","TouchesAttPen","LiveTouches","AttDribble",
               "DribblesPerTouch","Carries"))
    
    #This is where we create our reactive table
    output_table <- reactable(
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
  
    #We 'render' our table - process and make it ready for being displayed
    #to the user
  output$playerData <- renderReactable({
    output_table
  })
}

# Return a Shiny app object
shinyApp(ui = UI, server = SERVER)