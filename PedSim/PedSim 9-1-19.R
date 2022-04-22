#####
#pedigree simulation function definition
#simulation at the POPULATION level

#assume diploid, 2 sexes

##ISSUSE##
# Dams = NA
# assigning gender to new generation gives warning 
# while loop outputs all iteration withing the loop not just final ped 

#initial values
nGens = 10
popSize = 37
sexRatio = 0.5
breedMale = 0.9
breedFemale = 0.9
P.ages = c(1,2,3,4,5,6)
P.death = c(0.3, 0.3, 0.3, 0.3, 0.3, 1)
pbabies = c(0,0,1,0)
nbabies = c(0,1,2,3)

# 1) define function and arguments
PedSim <- function(nGens = 10, 
                   popSize = 11, # n founders
                   sexRatio = 0.5, 
                   #Chance of a male breeding
                   breedMale = 0.9,
                   #Chance of female breeding
                   breedFemale = 0.9,
                   #Chance of individuals dying each generation
                   deathRate = 0.3,
                   pbabies = c(0,0,1,0),
                   nbabies = c(0,1,2,3))
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
Id <- append(x=Id, values = seq(popSize+1, (popSize+sum(babiesF))))

Dam <-append(x=Dam, values = rep(pairsF, each = 2), after = popSize) 

Sire <-append(x=Sire, values = rep(pairsM, each = 2), after = popSize)

popSize <-(max(Id))

Sex <- append(x=Sex, values=ifelse(runif(sum(babiesF), 0,1) > sexRatio, 
              "male", "female"), after = max(which(Gen < max(Gen))))

## Killing off individuals for current generation

#DeathGen <- vector(mode="integer", length=popSize)
#DeathGen[1:popSize] <- rep(NA, popSize)
#Dead<-sample(Id, size = round(max(Id)*deathRate), prob = NULL)
#myPed$DeathGen<-ifelse(myPed$Id %in% Dead , "Dead", "NA")

### New Death script ####
Age<- which(Gen ==1 ) age = 3
Age <- append(x = Age, values = Age +1)

AgeAtDeath <- vector(length(popSize), mode="numeric")
for(i in 1:length(popSize)){
  popSize[i] <- sample(x=P.age, size=1, prob=P.death)
}

#5) Creating full population

#5.1 Use a while? While Gen < nGen loop, 3.3 to 4.3


while(max(myPed$Gen)<nGens) ## not perfect
{                           ## print all output of loop
  #PICK POTENTIAL BREEDERS
  tempM <- which(myPed$Sex=="male" & myPed$DeathGen != "Dead")
  tempF <- which(myPed$Sex=="female" & myPed$DeathGen != "Dead")
  
  #PICK ACTUAL BREEDERS
  breederM <- runif(5,0,1) < breedMale
  breederF <- runif(5,0,1) < breedFemale
  
  #CREATE BREEDING PAIRS
  pairsF <- tempF[breederF]
  pairsM <- sample(x=tempM[breederM], size=length(pairsF), replace=T )
  
  babies <- c(0,1,2,3)
  pbabies <- c(0,0,1,0)
  
  babiesF <- vector(length(pairsF), mode="numeric")
  for(i in 1:length(pairsF)){
    babiesF[i] <- sample(x=babies, size=1, prob=pbabies)
  }
  
  Gen <- append(x=Gen,values = rep((max(Gen)+1),sum(babiesF), after = popSize))
  
  
  Id <- append(x=Id, values = seq(popSize+1, (popSize+sum(babiesF))))
  
  Dam <-append(x=Dam, values = rep(pairsF, times = 2), after = popSize) 
  
  Sire <-append(x=Sire, values = rep(pairsM, times = 2), after = popSize)
  
  popSize <-(max(Id))
  
  Sex <-append(x= Sex, values =rep (ifelse(popSize %% 2 == 0, 
              Sex[1:sum(babiesF)] <- c(rep("male", sum(babiesF)*sexRatio), 
                                       rep("female", sum(babiesF)*sexRatio) ),
              Sex[1:sum(babiesF)] <- c(rep("male", trunc(sum(babiesF)*sexRatio,0)), 
                                       rep("female", trunc(sum(babiesF)*sexRatio,0)),
                                       ifelse(runif(1,0,1) > sexRatio, "male", "female"))),
               times = sum(babiesF)),
               after = max(Gen)-1)

  DeathGen <- vector(mode="integer", length=popSize)
  DeathGen[1:popSize] <- rep(NA, popSize)
  
  myPed <-data.frame(Id, Sire, Dam, Gen, Sex, DeathGen)
  
  
  Dead<-sample(Id, size = round(max(Id)*deathRate), prob = NULL)
  
  
  myPed$DeathGen<-ifelse(myPed$Id %in% Dead , "Dead", "NA")
  
  if (max(myPed$Gen) > nGens) break; print(myPed); }

}

##issues##
# Dams = NA
# assigning gender to new generation gives warning 
# while loop outputs all iteration withing the loop not just final ped 

  
