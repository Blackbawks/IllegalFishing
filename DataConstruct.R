#### Build the dataset ####

library(dplyr)
library(tidyr)
library(reshape)
library(foreach)

setwd("D:/Dropbox/BlackBawks/BlogData/IUUFishing/")

source("DataFunctions.R")


Case.ship.names <- c("Jimmy Buffet", "Neil Young", "Bruce Lee", "Christian Bale", "Jolly Roger")
Case.ship.years <- c(2002, 1994, 2014, 1975, 1961)
Case.ship.types <- c(1,2,3,4,5)
Case.ship.owners <- c("SparkleFish","FishRGud","KungFuFish","ScummyFishCo","FishARRRies")
Case.ship.countries <- c("Sidonia","Avalon","Noordilund","Slagovnia","Tortuga")
Case.ship.sizes <- c(73, 104, 83, 192, 270 )
Case.ship.ShipTo <- c('LaLaLand','BetaZed','The Shire','Alpha Centauri','Kings Landing')
#Shipdata <- tbl_df(CreateShips())

#### Let's save this so we only have to do it one time

#write.csv(Shipdata, "Ships.csv",row.names=F)

#### And read it back in now

Shipdata <- tbl_df(read.csv("Ships.csv"))



## Have to set up other parameters as well... 
# 1)  Has the vessel engaged in IUU fishing?  
#     (This is the response as we are interested in determining the risk for individual vessels)
# 2)  Is the species being fished on the CITES list?
# 3)  Does the country have a flag against it? 
# 4)  Has the owner been caught for illegal fishing previously? 
# 5)  Was AIS/WMS active? 
# 6)  Has the ship's regular trade route changed suddenly? 



#### Let's create a column for the type of species fished, as well as a mini CITES database
#### The lower the number, the more restricted the trade
#### 1 will never be reported because they are illegal - however, we note that BillidaeNyiecus looks
#### very much like another species which IS listed as 1.  


Fish.Species <- data.frame(species = c('Raricus Fishica','Commonae Eatedie',
                                       'Billidae Nyiecus','Donaldus Trumpfishii',
                                       'Fishica Maximus'),CitesLevel = c('2','3','2','2','3'))

#### Now we set up the rules for these fish which we'll put in the training dataset
### Rule 1)  RaricusFishica can only be fished by class 4 and 5 vessels
### Rule 2)  CommonaeEatedie are taken by all classes
### Rule 4)  BillidaeNyiecus looks a lot like another species which is illegal to catch, and is only ever caught in
#            Class 1 vessels 
### Rule 5)  DonaldusTrumpfishii is somewhat rare, can only be targeted by class 2 and 3 vessels
### Rule 6)  FishicaMaximus is very common and taken only by class 5 vessels


Shipdata$species <- sapply(Shipdata$shipType, function(x){
  if(x == 5){
    Fish <- sample(Fish.Species$species, 1, replace=TRUE, prob=c(0.6,0.1,0,0,0.3))
  }else if(x == 4){
    Fish <- sample(Fish.Species$species, 1, replace=TRUE, prob=c(0.5,0.5,0,0,0))
  }else if(x == 3){
    Fish <- sample(Fish.Species$species, 1, replace=TRUE, prob=c(0,0.5,0,0.5,0))
  }else if(x == 2){
    Fish <- sample(Fish.Species$species, 1, replace=TRUE, prob=c(0,0.5,0,0.5,0))
  }else if(x == 1){
    Fish <- sample(Fish.Species$species, 1, replace=TRUE, prob=c(0,0.3,0.7,0,0))
  }
  
})

Shipdata$cites <- sapply(Shipdata$species, function(x){return(Fish.Species$CitesLevel[which(Fish.Species$species == x)])})




### Calculates the number of years active which we use to expand the table to add the year on the dataset
PresentYear <- 2017
Shipdata$totyears <- (PresentYear - Shipdata$yearBuilt) - 1
## We subtract 1 here to make sure we aren't added 2017 as the latest year
Shipdata <- data.frame(Shipdata)

#Expand the table to add the time series (year) data
Shipexpand <-untable(Shipdata[,c(1,2,3,4,5,6,7,8)], num=Shipdata[,9])





### Expands the data to create time series

Train1 <- foreach(k = unique(Shipexpand$shipName), .combine='rbind') %do% {
  cat(k,'\n')
  A <- Shipexpand[which(Shipexpand$shipName == k),]
  A$Years <- sapply(1:nrow(A), function(x){A$yearBuilt[x] + x}) 
  return(A)
}





### Let's set up the countries, if they have flags, and when they were received... we program three flags:
### 0 mean no flag, 1 means minor flag (one time infraction, or other circumstances), 2 is a major flag (blatent illegal activity)
### A value of 99999 is set to indicate a country has never been flagged

C.flagged <- data.frame(countries = Case.ship.countries, yearflagged = c(2005,1994,99999,1972,1962),severity=c(1,1,0,2,2))

### Get the severity flags for the countries
Train2 <- foreach(k = unique(Train1$shipName),.combine='rbind') %do% {
  cat(k,'\n')
  Dat <- Train1[which(Train1$shipName == k),]  
  Dat$countryFlag <- Flagged(C.flagged,Dat)
  return(Dat)
  
}

### Next we check the flags to ensure the ratios are consistent with our assumptions about the owners/operators

test <- tbl_df(Train2)
Train2$flag <- as.character(Train2$flag)
  
G <- test %>% group_by(owners,flag) %>% summarise(n=n())

########################

#   FishARRRies     0  1973
#   FishARRRies     1   685
#   FishARRRies     2 20521
#      FishRGud     0  5952
#      FishRGud     1  6942
#      FishRGud     2  3801
#    KungFuFish     0  6829
#    KungFuFish     1  2474
#    KungFuFish     2   981
#  ScummyFishCo     0  2557
#  ScummyFishCo     1   831
#  ScummyFishCo     2 17656
#   SparkleFish     0  7340
#   SparkleFish     1  4750
#   SparkleFish     2  1235

###########################


#######################################################

### Similar to above, we now set up flags for the operators/owners


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

### Place the flags in for the owners
Train3 <- foreach(k = unique(Train2$shipName),.combine='rbind') %do% {
  cat(k,'\n')
  Dat <- Train2[which(Train2$shipName == k),]  
  Dat$Num.O.flags <- OFlagged(O.expand,Dat)
  
  return(Dat)
  
}



### Next we simulate if a ship had on it's WMS / AIS system during the fish
### In reality, this would be hard to tell - the WMS/AIS system needs to be linked to the database
### We'll do this as a function of owner, country, and fish species
### We'll set this as the probability that the AIS is ON (or 1)


ownsprobs <- data.frame(owners = c("KungFuFish","SparkleFish","FishRGud","ScummyFishCo","FishARRRies"),Probs = c(5,4,3,2,1))
counprobs <- data.frame(countries = c("Noordilund","Sidonia","Avalon","Slagovnia","Tortuga"),Probs = c(5,4,3,2,1))
fishprobs <- data.frame(species = c('Raricus Fishica','Commonae Eatedie','Billidae Nyiecus','Donaldus Trumpfishii',
                                       'Fishica Maximus'),Probs = c(1,5,2,3,4))
## 15 is the maximum value, so we'll subtract the additive probability from 18 (to make some wiggle room for the possibility that
## a situation where 15 comes up does not always get assigned an AIS ON (1))

Train3$AIS <- sapply(1:nrow(Train3),function(x){
  owner <- Train3$owners[x]
  country <- Train3$country[x]
  fish <- Train3$species[x]
  ProbAIS <- ownsprobs$Probs[which(ownsprobs$owners == owner)]+
    counprobs$Probs[which(counprobs$countries == country)]+
    fishprobs$Probs[which(fishprobs$species == fish)]
  ProbNoAIS <- 18 - ProbAIS
  
  AIS <- sample(c(0,1),1,prob=c(ProbNoAIS,ProbAIS))
  
})



##### Last step is to fix shipping routes
#### Five possible places to go...

ShipTo <- c('LaLaLand','Alpha Centauri','BetaZed','The Shire','Kings Landing')

seed <- 1
Train4 <- foreach(k = unique(Train3$shipName), .combine='rbind') %do% {
  Dat <- data.frame(Train3[which(Train3$shipName == k),])
  EndPoint <- sample(ShipTo,1)
  Dat$ShipTo <- rep(EndPoint,nrow(Dat))
  seed <- seed+1
  return(Dat)
}

### Now we make it so a few of the high risk operators/owners and fish species end up sometimes changing shipto locations

Train4$ShipSwitch <- rep(0,nrow(Train4))
Train5 <- foreach(k = unique(Train4$shipName), .combine='rbind',.errorhandling='stop') %do% {
  cat(k,'\n')
  Dat <- Train4[which(Train4$shipName == k),]
  if(unique(Dat$owners) == "ScummyFishCo" | unique(Dat$owners) == "FishARRRies" ){
  Samp <- sample(c(1:10),1,replace=T)
    if(Samp > 5){
      if(unique(Dat$species)=='Raricus Fishica' | unique(Dat$species)=='Billidae Nyiecus'){
        rows <- sample(c(1:nrow(Dat)),2,replace=T)
        Dat$ShipTo[rows[1]] <- sample(ShipTo,1,replace=T)
        Dat$ShipTo[rows[2]] <- sample(ShipTo,1,replace=T)
        ## We do this for reference for creating our dataset
        Dat$ShipSwitch[rows[1]] <- 1
        Dat$ShipSwitch[rows[2]] <- 1

          
      }
    }
  }
  return(Dat)
}



#########################################################################
### Now the target parameters are set.. time to get the 1s and 0s in place for illegal activity...
### Here are the assumptions we want to use to create guidelines on how to put 1s and 0s in place

## Vessels of class 1, 2 and 3 are smaller (60 - 139m in length), class 4,5 are large (140 - 200m) 

## Assumption 1) The largest (class 5) and smallest (class 1) vessels are slightly more likely to engage in illegal fishing. 
##               This will help to create a bimodal distribution of the ship sizes engaging in illegal activities to 
##               demonstrate the non-parametric nature of the algorithm.
## Assumption 2) "responsible" countries with strong Illegal fishing laws are less likely to engage in illegal fishing.
##               In our dataset, Sidonia and Noordilund are countries with strong regulations, Avalon is in the middle
##               and Slagovnia and Tortuga have either little or no regulation
## Assumption 3) Companies with sustainable practices will almost never engage in illegal fisheries.
##               In our example, SparkleFish, and KungFuFish are the most sustainable, FishRGud are moderate, while 
##               ScummyFishCo and FishARRRies are the least sustainable
## Assumption 4) Older fishing vessels are more likely to engage in illegal fisheries as they are more likely to be used
##               by organizations wanting to cut costs and not prioritize safety features to save money (these are 
##               organizations likely to be more corrupt)
## Assumption 5) RaricusFishica is likely to be illegally caught the most.. but BillidaeNyiecus looks like another species
##               therefore we score it higher as there could be illegal fishing associated with it. 
## Assumption 6) Cites level 2 are more likely to be associated with illegal fishing
## Assumption 7) More flags by the owner increases likelihood a vessel is fishing illegally
## Assumption 8) If a country has been flagged, illegal fishing is more likely
## Assumption 9) If a ship has switched its trade route, it is more likley to be fishing illegally
## Assumption 10) If ship does not have on AIS, it is more likely to be fishing illegally


### Using a similar additive technique as above, we'll create probabilities of illegal fishing based on our assumptions

### Note - we're being arbitrary with our decisions here on how to scale probabilities to run a good model
### This is just a sample and is not necessarily reflective of exactly what it might look like in real life

## Assumption 1
shipprobs <- data.frame(class = c(1,2,3,4,5), scores= c(10,2,2,4,10))
## Assumption 2
counprobs <- data.frame(countries = c("Noordilund","Sidonia","Avalon","Slagovnia","Tortuga"),scores = c(0.25,0.3,1,6,15))
## Assumption 3
ownsprobs <- data.frame(owners = c("KungFuFish","SparkleFish","FishRGud","ScummyFishCo","FishARRRies"),scores = c(0.01,0.01,0.5,14,15))
## Assumption 4 will be scaled to a score of 5
scalar <- length(2017:1960)/5
constructionprobs <- data.frame(yearbuilt = c(2017:1960), score = c((1:length(2017:1960))/scalar))
## Assumption 5
fishprobs <- data.frame(species = c('Raricus Fishica','Commonae Eatedie','Billidae Nyiecus','Donaldus Trumpfishii','Fishica Maximus'),
                        scores = c(4,0.5,3,1,0.5))
## Assumption 6
citesprobs <- data.frame(cites = c(2,3), scores= c(2,1))
## Assumption 7 scaled to 5
scalar <- length(0:20)/30
ownerflagprobs <- data.frame(numflags = c(0:20), score = c((1:length(0:20))/scalar))
## Assumption 8
countryflagprobs <- data.frame(flag = c(0,1,2), score=c(0,1,10) )
## Assumption 9
switchprobs <- data.frame(switch = c(0,1), score = c(0,5))
## Assumption 10
AISprobs <- data.frame(AIS = c(0,1), score= c(5,1))



  
IllegalFishing <- sapply(1:nrow(Train5),function(x){
  shipclass <- Train5$shipType[x]
  country <- Train5$country[x]
  owner <- Train5$owners[x]
  yearBuilt <- Train5$yearBuilt[x]
  fish <- Train5$species[x]
  cites <- Train5$cites[x]
  ownerflagged <-Train5$Num.O.flags[x]
  countryflagged <-Train5$countryFlag[x]
  shipToSwitch <-Train5$ShipSwitch[x]
  AIS <-Train5$AIS[x]
  
  ProbIllegal <- shipprobs$score[which(shipprobs$class == shipclass)]+
    counprobs$score[which(counprobs$countries == country)]+
    ownsprobs$score[which(ownsprobs$owners == owner)]+
    constructionprobs$score[which(constructionprobs$yearbuilt == yearBuilt)]+
    fishprobs$score[which(fishprobs$species == fish)]+
    citesprobs$score[which(citesprobs$cites == cites)]+
    ownerflagprobs$score[which(ownerflagprobs$numflags == ownerflagged)]+
    countryflagprobs$score[which(countryflagprobs$flag == countryflagged)]+
    switchprobs$score[which(switchprobs$switch == shipToSwitch)]+
    AISprobs$score[which(AISprobs$AIS == AIS)]

  
  ProbNotIllegal <- 101 - ProbIllegal
  cat(x,'\n')
  Illegal <- sample(c(0,1),1,prob=c(ProbNotIllegal,ProbIllegal))
  return(Illegal)
  
})


TrainingDat <- Train5
TrainingDat$Illegal <- IllegalFishing


####### Save the data!!
write.csv(TrainingDat,"Training.csv",row.names=F)




##########################################################################
## Now we should check our assumptions
library(ggplot2)

alldata<- tbl_df(TrainingDat)

## Assumption 1
ShipGroups <- alldata %>% group_by(shipType,Illegal) %>% summarise(n = n())


p <- ggplot(data = ShipGroups, aes(x = factor(shipType), y = n,
                                   fill = factor(Illegal)))

p + geom_bar(stat = "identity",
             position = position_dodge(0.9)) +
  scale_fill_discrete(name = "Illegal Fishing")



## Assumption 2
ShipGroups <- alldata %>% group_by(country,Illegal) %>% summarise(n = n())


p <- ggplot(data = ShipGroups, aes(x = factor(country), y = n,
                                   fill = factor(Illegal)))

p + geom_bar(stat = "identity",
             position = position_dodge(0.9)) +
  scale_fill_discrete(name = "Illegal Fishing")



## Assumption 3
ShipGroups <- alldata %>% group_by(owners,Illegal) %>% summarise(n = n())


p <- ggplot(data = ShipGroups, aes(x = factor(owners), y = n,
                                   fill = factor(Illegal)))

p + geom_bar(stat = "identity",
             position = position_dodge(0.9)) +
  scale_fill_discrete(name = "Illegal Fishing")

## Assumption 4

ShipGroups <- alldata %>% group_by(yearBuilt,Illegal) %>% summarise(n = n())


p <- ggplot(data = ShipGroups, aes(x = factor(yearBuilt), y = n,
                                   fill = factor(Illegal)))

p + geom_bar(stat = "identity",
             position = position_dodge(0.9)) +
  scale_fill_discrete(name = "Illegal Fishing")


## Assumption 5

ShipGroups <- alldata %>% group_by(species,Illegal) %>% summarise(n = n())


p <- ggplot(data = ShipGroups, aes(x = factor(species), y = n,
                                   fill = factor(Illegal)))

p + geom_bar(stat = "identity",
             position = position_dodge(0.9)) +
  scale_fill_discrete(name = "Illegal Fishing")


## Assumption 6 

ShipGroups <- alldata %>% group_by(cites,Illegal) %>% summarise(n = n())


p <- ggplot(data = ShipGroups, aes(x = factor(cites), y = n,
                                   fill = factor(Illegal)))

p + geom_bar(stat = "identity",
             position = position_dodge(0.9)) +
  scale_fill_discrete(name = "Illegal Fishing")

## Assumption 7

ShipGroups <- alldata %>% group_by(Num.O.flags,Illegal) %>% summarise(n = n())


p <- ggplot(data = ShipGroups, aes(x = factor(Num.O.flags), y = n,
                                   fill = factor(Illegal)))

p + geom_bar(stat = "identity",
             position = position_dodge(0.9)) +
  scale_fill_discrete(name = "Illegal Fishing")

## Assumption 8

ShipGroups <- alldata %>% group_by(countryFlag,Illegal) %>% summarise(n = n())


p <- ggplot(data = ShipGroups, aes(x = factor(countryFlag), y = n,
                                   fill = factor(Illegal)))

p + geom_bar(stat = "identity",
             position = position_dodge(0.9)) +
  scale_fill_discrete(name = "Illegal Fishing")

## Assumption 9

ShipGroups <- alldata %>% group_by(ShipSwitch,Illegal) %>% summarise(n = n())


p <- ggplot(data = ShipGroups, aes(x = factor(ShipSwitch), y = n,
                                   fill = factor(Illegal)))

p + geom_bar(stat = "identity",
             position = position_dodge(0.9)) +
  scale_fill_discrete(name = "Illegal Fishing")


## Assumption 10

ShipGroups <- alldata %>% group_by(AIS,Illegal) %>% summarise(n = n())


p <- ggplot(data = ShipGroups, aes(x = factor(AIS), y = n,
                                   fill = factor(Illegal)))

p + geom_bar(stat = "identity",
             position = position_dodge(0.9)) +
  scale_fill_discrete(name = "Illegal Fishing")



#############################################################################
## We are quite satisfied that our assumptions have now been met...





