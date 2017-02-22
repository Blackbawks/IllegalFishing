
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Using Machine learning to combat IUU fishing"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      HTML("<h2> Pick a ship from this list.. </h2>"),
      selectInput("shipname", "Ship's name",
                  c("Jimmy Buffet" = "Jimmy Buffet",
                    "Neil Young" = "Neil Young",
                    "Bruce Lee" = "Bruce Lee",
                    "Christian Bale" = "Christian Bale",
                    "Jolly Roger" = "Jolly Roger")),
      
      HTML("<h3> or enter ship details manually </h3>"),
      HTML("<hr style='border-top: 1px solid #000;' />"),
      HTML("<h1> Ship details </h1>"),
      selectInput("shipowner", "which company owns this ship?",
                  c("ScummyFishCo" = "ScummyFishCo",
                    "FishARRRRies" = "FishARRRies",
                    "KungFuFish" = "KungFuFish",
                    "FishRGud" = "FishRGud",
                    "SparkleFish" = "SparkleFish")),
      selectInput("country", "which flag is this ship flying?",
                  c("Noordilund" = "Noordilund",
                    "Sidonia" = "Sidonia",
                    "Avalon" = "Avalon",
                    "Slagovnia" = "Slagovnia",
                    "Tortuga" = "Tortuga")),
      sliderInput('shiplength', 'How long is the ship in meters?', min = 60, max = 300, value= 60),
      numericInput("yearBuilt", "What year was the ship built?", min = 1960, max = 2016, value = 1960),
      p("We will hard code this information in, but in a real application, this would be drawn from a database"),
      selectInput("shiproute", "Where does this ship normally send product?",
                  c('LaLaLand' = 'LaLaLand',
                    'Alpha Centauri' = 'Alpha Centauri',
                    'BetaZed' = 'BetaZed',
                    'The Shire' = 'The Shire',
                    'Kings Landing' = 'Kings Landing')),
      
      
      
      HTML("<h1> Fishing details </h1>"),
      selectInput("species", "What species of fish have they recorded catching?",
                  c('Raricus Fishica' = 'Raricus Fishica',
                    'Commonae Eatedie' = 'Commonae Eatedie',
                    'Billidae Nyiecus' = 'Billidae Nyiecus',
                    'Donaldus Trumpfishii' = 'Donaldus Trumpfishii',
                    'Fishica Maximus' = 'Fishica Maximus')),
      selectInput("shipto", "Where is the shipment heading?",
                  c('LaLaLand' = 'LaLaLand',
                    'Alpha Centauri' = 'Alpha Centauri',
                    'BetaZed' = 'BetaZed',
                    'The Shire' = 'The Shire',
                    'Kings Landing' = 'Kings Landing')),
      radioButtons("AISon", "Was AIS active since departing the last port?",
                   c('Yes' = 1, 'No' = 0), selected = NULL, inline = TRUE)
      
      
    ),
    mainPanel(
      HTML("<h3> View decision support data and warnings </h3>"),
      actionButton('viewdata', "Click for data", icon = icon("eye-open", lib = "glyphicon")),
      conditionalPanel(
        
        condition = "input.viewdata != 0",
        h2("Data about your case"),
        h3("The ship"),
        p(htmlOutput("owner")),
        p(htmlOutput("country")),
        p(htmlOutput("shiptype")),
        p(htmlOutput("shipsize")),
        p(htmlOutput("yearbuilt")),
        p(htmlOutput("normship")),
        
        HTML("<hr style='border-top: 1px solid #000;' />"),
        
        h3("Species fished"),
        
        p(htmlOutput("species")),
        p(htmlOutput("cites")),
        p(htmlOutput("ShippingTo")),
        HTML("<hr style='border-top: 1px solid #000;' />"),
        
        h3("Risk factors"),
        p(htmlOutput("countryflag")),
        p(htmlOutput("ownerflags")),
        p(htmlOutput("AISon")),
        p(htmlOutput("ShipSwitch")),
        
        h3("Run Risk Analysis"),
        htmlOutput("analysisbutton"),
        htmlOutput("RiskProbability")
        
      ) 
    )
  )
))

