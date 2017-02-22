
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rsconnect)
library(dplyr)
library(tidyr)
library(reshape)
library(caret)
library(randomForest)


GetOFlags <- function(O.expand, owner, yr){
  Z <- O.expand[which(O.expand$owners == owner),]
  val <- Z$flagcount[which(Z$flagged == max(Z$flagged[which(Z$flagged <=yr)]))]
  if(length(val) == 0){
    return(0)
  }else{
    return(val)
  }
}


GetCflags <- function(C.flagged,country,yr){
  val <- C.flagged$severity[which(C.flagged$countries == country)]
  if(C.flagged$yearflagged[which(C.flagged$countries == country)] < yr){
    return(val)
  }else{
    return(0)
  }
  
}


Model <- readRDS("Caret_RF.RDS")


shinyServer(function(input, output, session) {

  Case.ship.names <- c("Jimmy Buffet", "Neil Young", "Bruce Lee", "Christian Bale", "Jolly Roger")
  Case.ship.years <- c(2002, 1994, 2014, 1975, 1961)
  Case.ship.types <- c(1,2,3,4,5)
  Case.ship.owners <- c("SparkleFish","FishRGud","KungFuFish","ScummyFishCo","FishARRRies")
  Case.ship.countries <- c("Sidonia","Avalon","Noordilund","Slagovnia","Tortuga")
  Case.ship.sizes <- c(73, 104, 83, 192, 270 )
  Case.ship.ShipTo <- c('LaLaLand','BetaZed','The Shire','Alpha Centauri','Kings Landing')
  
  CaseShips <- data.frame(Case.ship.names,Case.ship.years,Case.ship.types,Case.ship.owners,Case.ship.countries,Case.ship.sizes,Case.ship.ShipTo)
  
  Fish.Species <- data.frame(species = c('Raricus Fishica','Commonae Eatedie',
                                         'Billidae Nyiecus','Donaldus Trumpfishii',
                                         'Fishica Maximus'),CitesLevel = c('2','3','2','2','3'))
  
  
  
  O.flagged <- data.frame(owners = Case.ship.owners, numflags = c(1,5,1,14,20))
  SFflags <- c(1991)
  SFflagcount <- c(1)
  FGflags <- c(1991,1994,1999,2002,2004)
  FGflagcount <- c(1:length(FGflags))
  KFflags <- c(1982)
  KFflagcount <- c(1:length(KFflags))
  SCflags <- c(1962,1971,1972,1975,1980,1982,1983,1984,1991,1992,1995,1999,2000,2007)
  SCflagcount <- c(1:length(SCflags))
  FAflags <- c(1961,1963,1964,1965,1968,1970,1972,1975,1977,1980,1984,1989,1992,1994,1999,2000,2004,2005,2007,2010)
  FAflagcount <- c(1:length(FAflags))
  O.expand <-untable(O.flagged[,c(1,2)], num=O.flagged[,2])
  O.expand$flagged <- c(SFflags,FGflags,KFflags,SCflags,FAflags)
  O.expand$flagcount <- c(SFflagcount,FGflagcount,KFflagcount,SCflagcount,FAflagcount)
  
  
  C.flagged <- data.frame(countries = Case.ship.countries, yearflagged = c(2005,1994,99999,1972,1962),severity=c(1,1,0,2,2))
  
  
  
  
  observeEvent(input$shipname, {

    x <- input$shipname
    
    owner <- CaseShips$Case.ship.owners[which(CaseShips$Case.ship.names == x)]
    country <- CaseShips$Case.ship.countries[which(CaseShips$Case.ship.names == x)]
    shiplength <- CaseShips$Case.ship.sizes[which(CaseShips$Case.ship.names == x)]
    yearBuilt <- CaseShips$Case.ship.years[which(CaseShips$Case.ship.names == x)]
    
    
    updateSelectInput(session, "shipowner", selected = owner)
    updateSelectInput(session, "country", selected = country)
    updateSliderInput(session, "shiplength", value = shiplength)
    updateNumericInput(session, "yearBuilt", value = yearBuilt)
    
  })
  
  
  observeEvent(input$viewdata, {
    owner <- input$shipowner
    country <- input$country
    shiplength <- input$shiplength
    yearBuilt <- input$yearBuilt
    
    x <- input$shipname
    sp <- input$species
    normship <- input$shiproute
    st <- input$shipto
    yr <- 2017
    ais <- input$AISon
    
    
    if(shiplength >= 60 & shiplength <= 100){
      shiptype <- 1
    }else if(shiplength >= 101 & shiplength <= 130){
      shiptype <- 2
    }else if(shiplength >= 131 & shiplength <= 170){
      shiptype <- 3
    }else if(shiplength >= 171 & shiplength <= 220){
      shiptype <- 4
    }else if(shiplength >= 221 & shiplength <= 300){
      shiptype <- 5
    }
    
    output$owner <- renderUI({ 
      HTML('<p> Owner: <strong>',as.character(owner),"</strong></p>")
      })
    output$country <- renderUI({ 
      HTML('<p> Country: <strong>',as.character(country),"</strong></p>")
    })
    
    output$shiptype <- renderUI({ 
      HTML('<p> Ship class: <strong>',as.character(shiptype),"</strong></p>")
    })
    output$shipsize <- renderUI({ 
      HTML('<p> Ship length: <strong>',as.character(shiplength),"</strong></p>")
    })
    output$yearbuilt <- renderUI({ 
      HTML('<p> Ship constructed in: <strong>',as.character(yearBuilt),"</strong></p>")
    })
    output$normship <- renderUI({ 
      HTML('<p> Normally ships to: <strong>',as.character(normship),"</strong></p>")
    })
    
    
    output$species <- renderUI({
      HTML('<p> Species caught: <strong>',as.character(sp),"</strong></p>")
    })

    cit <- Fish.Species$CitesLevel[which(Fish.Species == sp)]

    output$cites <- renderUI({
     HTML('<p> Cites listing: <strong>',as.character(cit),"</strong></p>")
    })
    output$ShippingTo <- renderUI({
     HTML('<p> Shipping to: <strong>',as.character(st),"</strong></p>")
    })



    CountFlag <- GetCflags(C.flagged,country,yr)
    if(CountFlag == 0){
      Cc <- '#000'
    }else if(CountFlag == 1){
      Cc <- '#f7c100'
    }else if(CountFlag == 2){
      Cc <- '#f44242'
    }


    output$countryflag <- renderUI({
      HTML("<p> What is the country flag severity (0 = none, 1 = minor, 2 = severe)?: <strong style='color:",Cc,"'>",as.character(CountFlag),"</strong></p>")
    })


    OwnFlags <- GetOFlags(O.expand, owner, yr)
    if(OwnFlags > 3 & OwnFlags < 8){
      C <- '#f7c100'
    }else if(OwnFlags > 8){
      C <- '#f44242'
    }else if(OwnFlags <= 3){
      C <- '#000'
    }


    output$ownerflags <- renderUI({
      HTML("<p> Number of owner flags: <strong style='color:",C,"'>",as.character(OwnFlags),"</strong></p>")
    })

    if(ais == 0){
      as <- "Inactive"
      Ca <- '#f44242'
    }else{
      as <- "Active"
      Ca <- '#000'
    }


    output$AISon <- renderUI({
      HTML("<p> AIS active?: <strong style='color:",Ca,"'>",as.character(as),"</strong></p>")
    })


    if(st != normship){
      Csh <- '#f44242'
      SSh <- 'Yes'
    }else{
      Csh <- '#000'
      SSh <- 'No'
    }


    output$ShipSwitch <- renderUI({
      HTML("<p> Shipping to a different place? <strong style='color:",Csh,"'>",as.character(SSh),"</strong></p>")
    })
    
    
    output$analysisbutton <- renderUI({
      actionButton('runanalysis', "Calculate Risk", icon = icon("globe", lib = "glyphicon"))
    })
    
    
  })
  
  observeEvent(input$runanalysis, {
    
    
    
    owners <- input$shipowner
    country <- input$country
    shipSize <- as.double(input$shiplength)
    
    if(shipSize >= 60 & shipSize <= 100){
      shipType <- as.factor(1)
    }else if(shipSize >= 101 & shipSize <= 130){
      shipType <- as.factor(2)
    }else if(shipSize >= 131 & shipSize <= 170){
      shipType <- as.factor(3)
    }else if(shipSize >= 171 & shipSize <= 220){
      shipType <- as.factor(4)
    }else if(shipSize >= 221 & shipSize <= 300){
      shipType <- as.factor(5)
    }
    
    yearBuilt <- as.double(input$yearBuilt)
    species <- input$species
    
    cites <- as.factor(Fish.Species$CitesLevel[which(Fish.Species == species)])
    yr <- as.double(2017)
    
    
    countryFlag <- as.double(GetCflags(C.flagged,country,yr))
    Num.O.flags <- as.double(GetOFlags(O.expand,owners, yr))

  
    AIS <- as.factor(input$AISon)
    
    normship <- input$shiproute
    ShipTo <- input$shipto
    if(ShipTo != normship){
      ShipSwitch <- as.factor('1')
    }else{
      ShipSwitch <- as.factor('0')
    }
    
    D <- data.frame(owners,country,shipType,yearBuilt,shipSize,species,cites,Years = yr,countryFlag,Num.O.flags,AIS,ShipTo,ShipSwitch)
    
    RProb <- predict(Model, D, type='prob')[2]
    output$RiskProbability <- renderUI({
      HTML("<h2>Probability of illegal fishing is <strong>", as.character(RProb), "</strong></h2>")
      
    })
    
    #Tr$owners <- as.factor(Tr$owners)
    #Tr$country <- as.factor(Tr$country)
    #Tr$shipType <- as.factor(Tr$shipType)
    #Tr$yearBuilt <- as.double(Tr$yearBuilt)
    #Tr$shipSize <- as.double(Tr$shipSize)
    #Tr$species <- as.factor(Tr$species)
    #Tr$cites <- as.factor(Tr$cites)
    #Tr$Years <- as.double(Tr$Years)
    #Tr$countryFlag <- as.double(Tr$countryFlag)
    #Tr$Num.O.flags <- as.double(Tr$Num.O.flags)
    #Tr$AIS <- as.factor(Tr$AIS)
    #Tr$ShipTo <- as.factor(Tr$ShipTo)
    #Tr$ShipSwitch <- as.factor(Tr$ShipSwitch)
    
    
  })
  
})
