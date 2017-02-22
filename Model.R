#############################################################################
## Now we'll model all this data and create predictions on the new stuff!  ##

library(randomForest)
library(caret)
library(dplyr)

setwd("D:/Dropbox/BlackBawks/BlogData/IUUFishing/")

Train <- tbl_df(read.csv("Training.csv"))

Tr <- select(Train, -shipName, -ShipTo)
### Set the datatypes ahead of time - this is not always necessary, but I like to do it to help avoid
### problems down the road

Tr$owners <- as.factor(Tr$owners)
Tr$country <- as.factor(Tr$country)
Tr$shipType <- as.factor(Tr$shipType)
Tr$yearBuilt <- as.double(Tr$yearBuilt)
Tr$shipSize <- as.double(Tr$shipSize)
Tr$species <- as.factor(Tr$species)
Tr$cites <- as.factor(Tr$cites)
Tr$Years <- as.double(Tr$Years)
Tr$countryFlag <- as.double(Tr$countryFlag)
Tr$Num.O.flags <- as.double(Tr$Num.O.flags)
Tr$AIS <- as.factor(Tr$AIS)
#Tr$ShipTo <- as.factor(Tr$ShipTo)
Tr$ShipSwitch <- as.factor(Tr$ShipSwitch)
Tr$Illegal <- as.factor(Tr$Illegal)

### Here we're going to run a model using random forests in the caret package 
### This will take a while on a slower computer, so I've saved the output. 

set.seed(9)
validation_index <- createDataPartition(Tr$Illegal, p=0.80, list=FALSE)
validation <- Tr[-validation_index,]
training <- Tr[validation_index,]
# train a model and summarize model
set.seed(9)
metric <- "Accuracy"
mtry <- floor(sqrt(ncol(Tr)))
tunegrid <- expand.grid(.mtry=mtry)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
rf_default <- train(Illegal~., data=training, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)


saveRDS(rf_default,file="Caret_RF.RDS")

Fish.Species <- data.frame(species = c('Raricus Fishica','Commonae Eatedie',
                                       'Billidae Nyiecus','Donaldus Trumpfishii',
                                       'Fishica Maximus'),CitesLevel = c('2','3','2','2','3'))


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

Case.ship.names <- c("Jimmy Buffet", "Neil Young", "Bruce Lee", "Christian Bale", "Jolly Roger")
Case.ship.years <- c(2002, 1994, 2014, 1975, 1961)
Case.ship.types <- c(1,2,3,4,5)
Case.ship.owners <- c("SparkleFish","FishRGud","KungFuFish","ScummyFishCo","FishARRRies")
Case.ship.countries <- c("Sidonia","Avalon","Noordilund","Slagovnia","Tortuga")
Case.ship.sizes <- c(73, 104, 83, 192, 270 )
Case.ship.ShipTo <- c('LaLaLand','BetaZed','The Shire','Alpha Centauri','Kings Landing')

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


RfModel <- readRDS("Caret_RF.RDS")

owners <- as.factor("FishRGud")
country <- "Avalon"
shipSize <- as.double(76)

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

yearBuilt <- as.double(1998)
species <- 'Billidae Nyiecus'

cites <- as.factor(Fish.Species$CitesLevel[which(Fish.Species == species)])
Years <- as.double(2017)


countryFlag <- as.double(GetCflags(C.flagged,country,Years))
Num.O.flags <- as.double(GetOFlags(O.expand, Years))


AIS <- as.factor(0)

normship <- 'LaLaLand'
ShipTo <- 'BetaZed'
if(ShipTo != normship){
  ShipSwitch <- as.factor('1')
}else{
  ShipSwitch <- as.factor('0')
}

D <- data.frame(owners,country,shipType,yearBuilt,shipSize,species,cites,Years,countryFlag,Num.O.flags,AIS,ShipTo,ShipSwitch)


predict(rf_default,D)










### First is to set size of vessels wrt type and country
### We will manually set the size of the five case vessels - the ones here are for the training set


### Buid the data for our known cases that we will use as examples
Case.ship.names <- c("Jimmy Buffet", "Neil Young", "Bruce Lee", "Christian Bale", "Jolly Roger")
Case.ship.years <- c(2002, 1994, 2014, 1975, 1961)
Case.ship.types <- c(1,2,3,4,5)
Case.ship.owners <- c("SparkleFish","FishRGud","KungFuFish","ScummyFishCo","FishARRRies")
Case.ship.countries <- c("Sidonia","Avalon","Noordilund","Slagovnia","Tortuga")
Case.ship.sizes <- c(70, 100, 80, 190, 200 )
