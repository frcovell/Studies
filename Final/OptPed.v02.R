
OptPed <- function(nGens = NULL, 
                   popSize = NULL, # n founders
                   maleRatio = NULL, 
                   #Chance of a male breeding
                   breedMale = NULL,
                   #Chance of female breeding
                   breedFemale = NULL,
                   #Chance of individuals dying each generation
                   breedingAge = NULL,
                   P.ages = NULL,
                   P.death = NULL,
                   P.babies = NULL,
                   nBabies = NULL)
{
  library(kinship2)
  # 2) Initialize temp and output variables for GENERATION 1
  Id <- 1:popSize
  
  Sex <- vector(mode = "character", length = popSize)
  
  ifelse(popSize %% 2 == 0,
         Sex <- c(rep("male", popSize*maleRatio), 
                  rep("female", popSize*(1-maleRatio)) ),
         Sex <- c(rep("male", trunc(popSize*maleRatio,0)), 
                  rep("female", trunc(popSize*(1-maleRatio),0)),
                  ifelse(runif(1,0,1) <= maleRatio, "male", "female")))
  
  Gen <- vector(mode = "integer", length = popSize)
  Gen[1:popSize] <- rep(1, each = popSize)
  
  Age <- vector(mode="integer", length=popSize)
  Age[1:popSize] <- rep(breedingAge, each = popSize)
  
  Sire <- vector(mode = "integer", length = popSize)
  Sire[1:popSize] <- rep(NA, popSize)
  
  Dam <- vector(mode="integer", length=popSize)
  Dam[1:popSize] <- rep(NA, popSize)
  

  
  # 3) decide who mates
  #NB this is first gen, need to run loop from gen = 2
  
     myPed<- data.frame(Id, Sire, Dam, Sex, Gen, Age)
    # myMales <- which(myPed$Sex=="male")
    # myFem <- which(myPed$Sex=="female") 
    myKin <-kinship(id= myPed$Id, dadid= myPed$Sire, momid= myPed$Dam, sex= myPed$Sex)
    listKin<-as.list(myKin)
    
    tempM <- which(Sex=="male" &  Age >= 3 ) 
    tempF <- which(Sex=="female" & Age >= 3 )
    
    #PICK ACTUAL BREEDERS
    breederM <- runif(length(tempM),0,1) < breedMale
    breederF <- runif(length(tempF),0,1) < breedFemale
    #CREATE BREEDING PAIRS
    pairsF <- tempF[breederF]  
    pairsM <- sample(x= which(listKin[tempM[breederM]] == min(myKin[pairsF,])),
                     size=length(pairsF), replace=T )
  
 
   
    #which(myKin[breederM] == min(myKin[pairsF]))
    babiesF <- vector(length(pairsF), mode="numeric")
    for(i in 1:length(pairsF)){
      babiesF[i] <- sample(x=nBabies, size=1, prob=P.babies)
    }
  
  
  # 4) Out put as a dataframe  
  # need to apend vector infor for next generation
  # what generation is it?
  # sum(nbabies) is the size of next gen
  
  Gen <- append(x=Gen,values = rep((max(Gen)+1),sum(babiesF), after = popSize))
  Id <- append(x=Id, values = seq(popSize+1, (popSize+sum(babiesF))), after = popSize)
  
  Dam <-append(x=Dam, values = rep(pairsF, each = 2), after = popSize) 
  
  Sire <-append(x=Sire, values = rep(pairsM, each = 2), after = popSize)
  
  Age <- (Age + 1)
  Age <- append(x=Age, values = rep(+1 , sum(babiesF)), after =  popSize)
  
  popSize <-(max(Id))
  
  Sex <- append(x=Sex, values=ifelse(runif(sum(babiesF), 0,1) > maleRatio, 
                                     "male", "female"), after = max(which(Gen < max(Gen))))
  
  Dead<- vector(length(popSize), mode="numeric")
  for(i in Id){
    Dead[i] <- sample(x=P.ages, size=1, prob=P.death)
  }
  AgeAtDeath <- vector(mode="integer", length=popSize) ### ERROR
  
  AgeAtDeath[1:popSize] <- ifelse(Age == Dead,Age,"NA" )

  #5) Creating full population
  
  #5.1 Use a while? While Gen < nGen loop, 3.3 to 4.3
  
  
  while(max(Gen)<nGens) ## not perfect
  {                           ## print all output of loop

        myPed<- data.frame(Id, Sire, Dam, Sex, Gen, Age, AgeAtDeath)
        # myMales <- which(myPed$Sex=="male")
        # myFem <- which(myPed$Sex=="female") 
        myKin <-kinship(id= myPed$Id, dadid= myPed$Sire, momid= myPed$Dam, sex= myPed$Sex)
        listKin<-as.list(myKin)
        
        tempM <- which(Sex=="male" & AgeAtDeath == "NA" & Age >= 3 ) 
        tempF <- which(Sex=="female" & AgeAtDeath == "NA"& Age >= 3 )
        breederM <- runif(length(tempM),0,1) < breedMale
        breederF <- runif(length(tempF),0,1) < breedFemale
        #CREATE BREEDING PAIRS
        pairsF <- tempF[breederF]  
        pairsM <- sample(x= which(listKin[tempM[breederM]] == min(myKin[pairsF,])),
                         size=length(pairsF), replace=T )
        
        #which(myKin[breederM] == min(myKin[pairsF]))
        babiesF <- vector(length(pairsF), mode="numeric")
        for(i in 1:length(pairsF)){
          babiesF[i] <- sample(x=nBabies, size=1, prob=P.babies)
        }
   #   }
    
    # 4) Out put as a dataframe  
    # need to apend vector infor for next generation
    # what generation is it?
    # sum(nbabies) is the size of next gen
    
    Gen <- append(x=Gen,values = rep((max(Gen)+1),sum(babiesF), after = popSize))
    
    Id <- append(x=Id, values = seq(popSize+1, (popSize+sum(babiesF))), after = popSize)
    
    Dam <-append(x=Dam, values = rep(pairsF, each = 2), after = popSize) 
    
    Sire <-append(x=Sire, values = rep(pairsM, each = 2), after = popSize)
    
    Age <- (Age + 1)
    Age <- append(x=Age, values = rep(+1 , sum(babiesF)), after =  popSize)
    
    
    popSize <-(max(Id))
    
    Sex <- append(x=Sex, values=ifelse(runif(sum(babiesF), 0,1) > maleRatio, 
                                       "male", "female"), after = max(which(Gen < max(Gen))))
    
    
    Dead<- vector(length(popSize), mode="numeric")
    for(i in Id){
      Dead[i] <- sample(x=P.ages, size=1, prob=P.death)
    }
    
    AgeAtDeath <- append(x=AgeAtDeath, value = rep( "NA", sum(babiesF)), after = max(which(Gen < max(Gen))))
    
    newDead<- which(Age==Dead & AgeAtDeath == "NA") 
    maxAge<- which(Age == max(P.ages)& AgeAtDeath == "NA")
    
    #AgeAtDeath[newDead]<- P.ages #Error
    AgeAtDeath[newDead]<-Age[newDead]
    AgeAtDeath[maxAge]<-max(P.ages)#Error
    #AgeAtDeath[newDead] <- ifelse(Age == Dead,Age,"NA" )
    myPed<- data.frame(Id, Sire, Dam, Sex, Gen, Age, AgeAtDeath)
    
    #if (max(Gen) == (nGens +1) ) break; return(myPed); }
    if (max(Gen) == (nGens +1) ){
      print(myPed)
      #return(as.data.frame((myPed)))
    }
    
}
  return((myPed)) #if (max(Gen) == (nGens +1) ) break; print(myPed<- data.frame(Id, Sire, Dam, Sex, Gen, Age, AgeAtDeath)); }
}
