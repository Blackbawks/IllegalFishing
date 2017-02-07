#### Build the dataset ####

library(dplyr)

setwd("C:/Users/Grant/Dropbox/BlackBawks/BlogData/IUUFishing/")

ShipData <- read.csv("ships.csv")

### First is to set size of vessels wrt type and country
### We will manually set the size of the five case vessels - the ones here are for the training set

## Set random generator value
set.seed(1925)

### Buid the data for our known cases that we will use as examples
Case.ship.names <- c("Jimmy Buffet", "Neil Young", "Bruce Lee", "Christian Bale", "Jolly Roger")
Case.ship.years <- c(2002, 1994, 2014, 1975, 1961)
Case.ship.types <- c(1,2,3,4,5)
Case.ship.owners <- c("SparkleFish","FishRGud","KungFuFish","ScummyFishCo","FishARRRies")
Case.ship.countries <- c("Sidonia","Avalon","Noordilund","Slagovnia","Tortuga")
Case.ship.sizes <- c(70, 100, 80, 190, 200 )


#### Now that we have our cases, we'll build the dataset up around a series of assumptions to build a known data structure
#### allowing us to test the application. 

## Assumption 1) Vessels of class 1, 2 and 3 are smaller (60 - 139m in length), class 4,5 are large (140 - 200m), and 
##               The participation of various classes of vessels in illegal fishing is variable. 
## Assumption 2) The largest (class 5) and smallest (class 1) vessels are slightly more likely to engage in illegal fishing. 
##               This will help to create a bimodal distribution of the ship sizes engaging in illegal activities to 
##               demonstrate the non-parametric nature of the algorithm.
## Assumption 3) "responsible" countries with strong Illegal fishing laws are less likely to engage in illegal fishing.
##               In our dataset, Sidonia and Noordilund are countries with strong regulations, Avalon is in the middle
##               and Slagovnia and Tortuga have either little or no regulation
## Assumption 4) Companies with sustainable practices will almost never engage in illegal fisheries.
##               In our example, SparkleFish, and KungFuFish are the most sustainable, FishRGud are moderate, while 
##               ScummyFishCo and FishARRRies are the least sustainable
## Assumption 5) Older fishing vessels are more likely to engage in illegal fisheries as they are more likely to be used
##               by organizations wanting to cut costs and not prioritize safety features to save money (these are 
##               organizations likely to be more corrupt)
## Assumption 6) All companies operate a somewhat similar number of ships
## Assumption 7) the country of Slavognia is very rich and operates large vessels (class 5) primarily, Tortuga is poor 
##               and operates small ships (class 1) primarily. All other companies use multiple classes.  

CompanyRegister <- function(owner){
  ## Responsible more owners are likely to be associated with countries that have stronger regulations on illegal fishing
  ## We set the probabilities here accordingly to reflect which countries a ship will likely be registered in  
  if(owner == "SparkleFish" ){
    country <- sample( Case.ship.countries, 1, replace=TRUE, prob=c(0.6, 0.1, 0.2, 0.05, 0.05))
  }else if(owner == "FishRGud"){
    country <- sample( Case.ship.countries, 1, replace=TRUE, prob=c(0.1, 0.5, 0.1, 0.1, 0.1))
  }else if(owner == "KungFuFish"){
    country <- sample( Case.ship.countries, 1, replace=TRUE, prob=c(0.3, 0.1, 0.5, 0.05, 0.05))
  }else if(owner == "ScummyFishCo"){
    country <- sample( Case.ship.countries, 1, replace=TRUE, prob=c(0.025, 0.05, 0.025, 0.7, 0.2))
  }else if(owner == "FishARRRies"){
    country <- sample( Case.ship.countries, 1, replace=TRUE, prob=c(0.025, 0.05, 0.025, 0.3, 0.6))
  }
  return(country)
  
}

YearBuilt <- function(owner){
  ### Now we will assign years built as a function of the owner. 
  ### We use the negative binomial distribution to sort a skewed distribution of probabilities
  ### Sorting in ascending order (1960 - 2015) will give us newer ships
  ### Sorted in descending order (1960 - 2015) will give us older ships
  ### Changing the value of MU to higher values will bring the probability distribution closer to a uniform
  
  if(owner == "SparkleFish" ){
    ship.built <- sample(c(1960:2015),1,replace=T,prob=c(sort(rnbinom(56,mu=4,size=1000))))
  }else if(owner == "FishRGud"){
    ship.built <- sample(c(1960:2015),1,replace=T)
  }else if(owner == "KungFuFish"){
    ship.built <- sample(c(1960:2015),1,replace=T,prob=c(sort(rnbinom(56,mu=2,size=1000))))
  }else if(owner == "ScummyFishCo"){
    ship.built <- sample(c(2015:1960),1,replace=T,prob=c(sort(rnbinom(56,mu=4,size=1000))))
  }else if(owner == "FishARRRies"){
    ship.built <- sample(c(2015:1960),1,replace=T,prob=c(sort(rnbinom(56,mu=2,size=1000))))
  }
  return(ship.built)
  
}


## This function creates the rank (1 being least risk, 10 being most risk) in order to place ship classes with owner and country
TypeRanker <- function(owner, country, rankdf){
  ownRank <- rankdf$ranks[which(rankdf$owners == owner)]
  couRank <- rankdf$ranks[which(rankdf$countries == country)]
  Rank <- ownRank + couRank
  return(Rank)
}

ShipTypes <- function(owner,country){
  ### Now we'll assign ship types to the various owners and countries, keeping in mind that we want to make sure
  ### that the 'corrupt' companies/countries are more likely to engage in illegal fishing, AND that the largest
  ### and smallest vessels are more likely to be involved in illegal fishing
  ### We list and order the countries and owners by rank of "responsibility" 1 being most responsible,5 being least
  ranks <- c(1,2,3,4,5)
  owners <- c("KungFuFish","SparkleFish","FishRGud","ScummyFishCo","FishARRRies")
  countries <- c("Noordilund","Sidonia","Avalon","Slagovnia","Tortuga")
  rankdf <- data.frame(ranks = ranks, owners = owners, countries = countries)
  ## Get every possible combination of owners and countries
  rankexpand <- expand.grid(countries=countries, owners = owners)
  sapply(1:nrow(rankexpand),function(x){TypeRanker(rankexpand$owners[x],rankexpand$countries[x],rankdf)})
  
  
  ship.type <- sample(c(1:5))
  
  
}

Size <- function(class,country){
  
  #### class 1 is the smallest
  #### class 5 is the largest (factory trawlers for example)
  
  if(class == 1){
    min.size <- 60
    max.size <- 89
    }else if(class==2){
      min.size <- 90
      max.size <- 109
    }else if(class==3){
    min.size <- 110
    max.size <- 139
  }else if(class==4){
    min.size <- 140
    max.size <- 199
  }else if(class==5){
    min.size <- 200
    max.size <- 250
  }
  
  ### Here, we set the relationship so that ships from countries like Sidonia and Noordilund (e.g. 'responsible countries')
  ### end up being a bit smaller on average - under our assumption that smaller ships from clean countries
  ### are less likely to fish illegally. Ships from less responsible countries will tend to be larger
  
  if(country == 'Sidonia'){
    
    mx.size.new <- max.size * 0.8
    ship.size <- floor(runif(1,min.size,mx.size.new))
    
  }else if(country == 'Avalon'){
    ## Ships from Avalon are average
    ship.size <- floor(runif(1,min.size,max.size))
    
  }else if(country == 'Noordilund'){
    mx.size.new <- max.size * 0.8
    ship.size <- floor(runif(1,min.size,mx.size.new))
    
  }else if(country == 'Slagovnia'){
    mn.size.new <- min.size * 1.4
    ship.size <- floor(runif(1,mn.size.new,max.size))
    
  }else if(country == 'Tortuga'){
    mn.size.new <- min.size * 1.4
    ship.size <- floor(runif(1,mn.size.new,max.size))
    
  }
  
  return(ship.size)
  
}


CreateShips <- function(){
  # Create Ids of 1000 known ships operated by these five countries and companies
  # We aren't interested in the names because we're only using this to train our algorithm/application
  Ship.Id <- as.factor(c(1:1000))
  owners <- sample( Case.ship.owners, 3000, replace=TRUE, prob=c(0.2, 0.2, 0.2, 0.2, 0.2))
  
  # Country is a function of the owner with a certain probability
  countries <- sapply(owners,function(x){CompanyRegister(x)})
  
  year.constructed <- sapply(owners,function(x){YearBuilt(x)})
  
  ShipSizes <- sapply(1:nrow(ShipData),function(x){Size(class = ShipData$vessel.type[x], country = ShipData$country.registry[x])})
  
  
}



##### Now, have a check through to make sure... 

X.ships <- tbl_df(ShipData)

ShipGroups <- X.ships %>% group_by(country.registry,vessel.type) %>% summarise(avg.size <- mean(Ship.size.m),std.size <-sd(Ship.size.m))














