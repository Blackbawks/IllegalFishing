

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

## Used to generate probabilities that a class of ship is associated with an owner/country based on the rank
ProbabilityGenerator <- function(rank){
  c1andc5 <- (rank/2)/10
  c23and4 <- (1 - (c1andc5*2))/3
  pr <- c(c1andc5, c23and4,c23and4,c23and4,c1andc5) 
  return(pr)
}


### this creates a dataframe with ranks and probabilities for classing ships. 
rankCreator <- function(){
  ranks <- c(1,2,3,4,5)
  owns <- c("KungFuFish","SparkleFish","FishRGud","ScummyFishCo","FishARRRies")
  couns <- c("Noordilund","Sidonia","Avalon","Slagovnia","Tortuga")
  rankdf <- data.frame(ranks = ranks, owners = owns, countries = couns)
  ## Get every possible combination of owners and countries
  rankexpand <- expand.grid(countries=couns, owners = owns)
  Ranks <- sapply(1:nrow(rankexpand),function(x){TypeRanker(rankexpand$owners[x],rankexpand$countries[x],rankdf)})
  ### Next we subtract 1 from the rankings - this is so the probability generator can create probabilities that scale
  ### with the most responsible combinations never using high risk vessels, and the least responsible with some 
  ### probability that they may use low risk vessels. 
  
  Ranks <- Ranks - 1
  Probs <- sapply(Ranks,function(x){ProbabilityGenerator(x)})
  
  rankexpand$Ranks <- Ranks
  
  return(list(rankexpand,Probs))
}



ShipTypes <- function(owner,country,rankexpand,Probs){
  ### Now we'll assign ship types to the various owners and countries, keeping in mind that we want to make sure
  ### that the 'corrupt' companies/countries are more likely to engage in illegal fishing, AND that the largest
  ### and smallest vessels are more likely to be involved in illegal fishing
  ### We list and order the countries and owners by rank of "responsibility" 1 being most responsible,5 being least
  
  ## Which probability to use... 
  ptouse <- Probs[,which(rankexpand$countries == country & rankexpand$owners == owner)]
  ## Calculate the type of ship based on the probability
  ship.type <- sample(c(1:5),1,prob = ptouse,replace=TRUE)
  
  return(ship.type)
  
}

Size <- function(class,country){
  
  #### class 1 is the smallest
  #### class 5 is the largest (factory trawlers for example)
  
  if(class == 1){
    min.size <- 60
    max.size <- 100
  }else if(class==2){
    min.size <- 101
    max.size <- 130
  }else if(class==3){
    min.size <- 131
    max.size <- 170
  }else if(class==4){
    min.size <- 171
    max.size <- 220
  }else if(class==5){
    min.size <- 221
    max.size <- 300
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
    mn.size.new <- min.size * 1.2
    ship.size <- floor(runif(1,mn.size.new,max.size))
    
  }else if(country == 'Tortuga'){
    mn.size.new <- min.size * 1.2
    ship.size <- floor(runif(1,mn.size.new,max.size))
    
  }
  
  return(ship.size)
  
}

## Used for calculating standard error where needed
se <- function(x) sqrt(var(x)/length(x))


### Create the simulated ships
CreateShips <- function(){
  # Create Ids of 1000 known ships operated by these five countries and companies
  # We aren't interested in the names because we're only using this to train our algorithm/application
  Ship.Id <- as.factor(c(1:3000))
  owners <- sample( Case.ship.owners, 3000, replace=TRUE, prob=c(0.2, 0.2, 0.2, 0.2, 0.2))
  # Country is a function of the owner with a certain probability
  countries <- data.frame(sapply(owners,function(x){CompanyRegister(x)}))
  names(countries) <- "country"
  ### Create the ranks and probabilities for ship types
  rankex <- rankCreator()
  rankexpand <- data.frame(rankex[1])
  Probs <- data.frame(rankex[2])
  
  ship.types <- sapply(1:length(owners),function(x){ShipTypes(owner=owners[x],country = as.character(countries[x,]),rankexpand,Probs)})
  
  year.constructed <- sapply(owners,function(x){YearBuilt(x)})
  
  ShipSizes <- sapply(1:length(owners),function(x){Size(class = ship.types[x], country = as.character(countries[x,]))})
  
  
  OutShips <- data.frame(shipName = Ship.Id, owners = owners, countries = countries, shipType = ship.types, yearBuilt = year.constructed, shipSize = ShipSizes)
  
  return(OutShips)
  
}






####### This function gets the severity flags from the countries and spits it out so we can place it in the dataframe
Flagged <- function(C.flagged,Dat){
  YearFlagged <- C.flagged$yearflagged[which(C.flagged$countries == unique(Dat$country))]
  Severity <- C.flagged$severity[which(C.flagged$countries == unique(Dat$country))]
  out<-sapply(Dat$Years, function(x){
    if(x < YearFlagged){
      return(0)
    }else{
      return(Severity)
    }
  })
  return(out)
}



### This function will flag the operators/owners It will throw some warnings - but can be ignored
OFlagged <- function(O.expand,Dat){
  
  O <- O.expand[which(O.expand$owners == unique(Dat$owner)),]
  ## Matches the flagcount to the year
  OwnerFlags <- sapply(Dat$Years, function(yr){
    val <- O$flagcount[which(O$flagged == max(O$flagged[which(O$flagged <=yr)]))]
    if(length(val) == 0){
      return(0)
    }else{
      return(val)
    }
  })
  
  return(OwnerFlags)
}









