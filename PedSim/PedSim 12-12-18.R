#####
#pedigree simulation function definition
#simulation at the POPULATION level

#assume diploid, 2 sexes

# 1) define function and arguments
PedSim <- function(nGens = 10, 
                   popSize = 11, # n founders
                   sexRatio = 0.5, 
                   #Chance of a male breeding
                   breedMale = 0.9,
                   #Chance of female breeding
                   breedFemale = 0.9)
{
  
# 2) Initialize temp and output variables 
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
  breederM <- runif(5,0,1) < breedMale
  breederF <- runif(5,0,1) < breedFemale
  
  #CREATE BREEDING PAIRS
  pairsF <- tempF[breederF]
  pairsM <- sample(x=tempM[breederM], size=length(pairsF), replace=T )
  
  #make babies
  ##need to make a offspring number vector and a
  # vector of probabilities for repro success
  babies <- c(0,1,2,3)
  pbabies <- c(0,0,1,0)
  
 babiesF <- vector(length(pairsF), mode="numeric")
  for(i in 1:length(pairsF)){
    babiesF[i] <- sample(x=babies, size=1, prob=pbabies)
  }
  
 
# need to apend vector infor for next generation
# what generation is it?
 # sum(nbabies) is the size of next gen

Gen <- append(x=Gen,values = rep((mean(Gen)+1),sum(babiesF), after = popSize))
Id <- append(x=Id, values = seq(popSize+1, (popSize+sum(babiesF))))


myPed <-data.frame(Id, Sire, Dam, Sex, Gen)

#  i=2

#  Dom <- sample(Id[intersect(which(Gen == (i-1)) , which(Sex == "male"))],
#           round(domRate*length(Id[intersect(which(Gen == (i-1)) , which(Sex == "male"))])))
  
#  Sub <-Id[-c(match (Dom,Id),intersect(which(Gen == (i-1)) , which(Sex == "female")))] 

## 3.2) determine breeding pairs
   #for(i in 2:nGens){
   # sample(Id[intersect(which(Gen == (i-1)) , which(Sex == "male"))])
  #}
  
# for (i in Dom) {
#      (sample(Id[intersect(which(Gen == (i-1)) , which(Sex == "female"))],
#        round((matingSystem/length(Dom))*length(Id[intersect(which(Gen == (i-1)) , which(Sex == "female"))]))))
# }
  
   #if round set to 0.25* legth work 
   #if round set to .025 or .0025 -> integer (0)
# for (i in Sub) {
#      (sample(Id[intersect(which(Gen == (i-1)) , which(Sex == "female"))],
#        round((1-matingSystem)/length(Sub))*length(Id[intersect(which(Gen == (i-1)) , which(Sex == "female"))])))
# }

  
 

  ## 3.3) making a data frame
  #initialise second gen 
  #randomly selects how many offspring each pair will have
#  as.integer(rnorm(Dam, mean = nOffspring, sd = 1))
  
#  myPed <- data.frame(Id, Sire, Dam, Sex, Gen)
# for (i in 2:nGens){
#  as.integer(rnorm(10, mean = nOffspring, sd = 3))

