##### create founders ####
nGens <- 20
popSize <- 10# n founders
maleRatio <- .5
#Chance of a male breeding
breedMale <-.9
#Chance of female breeding
breedFemale <-.9
#Chance of individuals dying each generation
breedingAge <-3
P.ages <-c(1,2,3,4,5,6)
P.death <- c(0.4, 0.45, 0.5, 0.55, 0.6, 1)
P.babies <-c(0,0,1,0)
nBabies <-c(0,1,2,3)

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

myPed<-data.frame(Id, Sex, Gen, Age, Sire, Dam)

###### create geno type ####
inBreeding <-0
nAllel <- 15
nLoci <-10
rSeed <-1010
  #Creating ancestor
  ancGeno <- function (y) matrix(sample(x=nAllel, nLoci*2, replace = T), 
                                 nrow=2, ncol=nLoci)
  nAncestor<-1
  x <- 1:nAncestor
  set.seed(rSeed)
  
  
  Ancestor <- lapply( X = 1:nAncestor,
                      FUN = ancGeno)
  
  #creating founder  
  mymat <- matrix(data=rep(0,2*nLoci), nrow=2, ncol=nLoci)
  FoundPed<-which(is.na(myPed$Sire)& is.na(myPed$Dam))
  Founder <- rep(list(mymat),length(FoundPed))
  
  
  for(i in 1:length(FoundPed))  {
    for(j in 1:nLoci){
      for(k in 1:2){
        ifelse(runif(1,1,100) < inBreeding*100,
               
               ifelse(runif(1,1,100) < 50,
                      Founder[[i]][[k,j]] <- Ancestor[[1]][[1,j]], ##is this only sampling from ancestory 1?
                      Founder[[i]][[k,j]] <- Ancestor[[1]][[2,j]]),
               
               Founder[[i]][[k,j]] <- sample(x= 1:nAllel, size = 1)  )
        
      }
    }
  }
##### calc founder het ####

    mymat <- matrix(data=rep(0,2*nLoci), nrow=1, ncol=(nLoci)/2)
    TPed<-which(is.na(myPed$Sire)& is.na(myPed$Dam))
    FHet2 <- rep(list(mymat),(length(TPed))/2)
    
   
  # FHet[[1]][,2]<- length(which(Founder[[1]][1,] != Founder[[2]][1,]))+length(which(Founder[[1]][1,] != Founder[[2]][1,]))
   #length(which(Founder[[1]][2,] != Founder[[2]][2,]))
    for (j in 1:(popSize/2)) {
      for (i in (popSize/2)+1:(popSize/2)) {
        
        FHet1[[j]][,i]<- length(which(Founder[[i]][1,] != Founder[[j]][1,]))
        +length(which(Founder[[i]][2,] != Founder[[j]][2,]))
      }
    }
    
    for (k in 1:(popSize/2)) {
    for (j in 1:(popSize/2)) {
      for (i in (popSize/2)+1:(popSize/2)) {
        
        FHet2[[k]][,j]<- length(which(Founder[[i]][1,] != Founder[[j]][1,]))
        +length(which(Founder[[i]][2,] != Founder[[j]][2,]))
      }
    }
    }
    length(which(Founder[[1]][1,] != Founder[[7]][1,])) +length(which(Founder[[1]][2,] != Founder[[7]][2,]))


    
#### picking breeders ####
    i=1
    #PICK POTENTIAL BREEDERS
    tempM <- which(Gen==i & Sex=="male")
    tempF <- which(Gen==i & Sex=="female")
    
    breederM <- runif(length(tempM),0,1) < breedMale
    breederF <- runif(length(tempF),0,1) < breedFemale
    
    #CREATE BREEDING PAIRS
    pairsF <- tempF[breederF]
    
    pairsM <- vector(length(pairsF), mode="numeric")
    
    for (i in 1:length(pairsF)) {
      
    pairsM <- sample(which(FHet[[i]]==max(FHet[[i]])))
    
    }
    
    babiesF <- vector(length(pairsF), mode="numeric")
    for(i in 1:length(pairsF)){
      babiesF[i] <- sample(x=nBabies, size=1, prob=P.babies)
    }
    
    which(FHet[[4]]==max(FHet[[4]]))
