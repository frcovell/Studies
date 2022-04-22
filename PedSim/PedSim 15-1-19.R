#####
#pedigree simulation function definition
#simulation at the POPULATION level

#assume diploid, 2 sexes

##ISSUSE##
# Dams = NA
# assigning gender to new generation gives warning 
# while loop outputs all iteration withing the loop not just final ped 

#initial values
nGens =10
popSize = 11
sexRatio = 0.5
breedMale = 0.9
breedFemale = 0.9
breedingAge = 3
P.ages = c(1,2,3,4,5,6)
P.death = c(0.42, 0.47, 0.53, 0.56, 0.63, 1)
pbabies = c(0,0,1,0)
nbabies = c(0,1,2,3)


# 1) define function and arguments
PedSim <- function(nGens = NULL, 
                   popSize = NULL, # n founders
                   sexRatio = NULL, 
                   #Chance of a male breeding
                   breedMale = NULL,
                   #Chance of female breeding
                   breedFemale = NULL,
                   #Chance of individuals dying each generation
                   breedingAge = NULL,
                   P.ages = NULL,
                   P.death = NULL,
                   pbabies = NULL,
                   nbabies = NULL)
{
  
# 2) Initialize temp and output variables for GENERATION 1
  Id <- 1:popSize
  
  Sex <- vector(mode = "character", length = popSize)
  
  ifelse(popSize %% 2 == 0,
    Sex[1:popSize] <- c(rep("male", popSize*sexRatio), 
                      rep("female", popSize*sexRatio) ),
    Sex[1:popSize] <- c(rep("male", trunc(popSize*sexRatio,0)), 
                        rep("female", trunc(popSize*sexRatio,0)),
                        ifelse(runif(1,0,1) > sexRatio, "male", "female")))
  
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
   
  i=1
  #PICK POTENTIAL BREEDERS
  tempM <- which(Gen==i & Sex=="male")
  tempF <- which(Gen==i & Sex=="female")
  
  #PICK ACTUAL BREEDERS
  breederM <- runif(length(tempM),0,1) < breedMale
  breederF <- runif(length(tempF),0,1) < breedFemale
  
  #CREATE BREEDING PAIRS
  pairsF <- tempF[breederF]
  pairsM <- sample(x=tempM[breederM], size=length(pairsF), replace=T )
  
  #make babies
  ##need to make a offspring number vector and a
  # vector of probabilities for repro success
  
  
  
 babiesF <- vector(length(pairsF), mode="numeric")
  for(i in 1:length(pairsF)){
    babiesF[i] <- sample(x=nbabies, size=1, prob=pbabies)
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

Sex <- append(x=Sex, values=ifelse(runif(sum(babiesF), 0,1) > sexRatio, 
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

  #PICK POTENTIAL BREEDERS
  tempM <- which(Sex=="male" & AgeAtDeath == "NA" & Age >= 3 ) 
  tempF <- which(Sex=="female" & AgeAtDeath == "NA"& Age >= 3 )
  breederM <- runif(length(tempM),0,1) < breedMale
  breederF <- runif(length(tempF),0,1) < breedFemale
  
  #CREATE BREEDING PAIRS
  pairsF <- tempF[breederF]
  pairsM <- sample(x=tempM[breederM], size=length(pairsF), replace=T )
  
  #make babies
  ##need to make a offspring number vector and a
  # vector of probabilities for repro success
  
  
  
  babiesF <- vector(length(pairsF), mode="numeric")
  for(i in 1:length(pairsF)){
    babiesF[i] <- sample(x=nbabies, size=1, prob=pbabies)
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
  
  Sex <- append(x=Sex, values=ifelse(runif(sum(babiesF), 0,1) > sexRatio, 
                                     "male", "female"), after = max(which(Gen < max(Gen))))
  

  Dead<- vector(length(popSize), mode="numeric")
  for(i in Id){
    Dead[i] <- sample(x=P.ages, size=1, prob=P.death)
  }
  
  AgeAtDeath <- append(x=AgeAtDeath, value = rep( "NA", sum(babiesF)), after = max(which(Gen < max(Gen))))
  
  newDead<- which(Age==Dead & AgeAtDeath == "NA") 
  maxAge<- which(Age == max(P.ages)& AgeAtDeath == "NA")
 
  AgeAtDeath[newDead]<-P.ages #Error
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


##issues## 
# AgeAtDeath > Age 

  
