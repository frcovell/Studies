#####
#pedigree simulation function definition
#simulation at the POPULATION level

#assume diploid, 2 sexes

# 1) define function and arguments
PedSim <- function(nGens = 10, 
                   popSize = 10, 
                   sexRatio = 0.5, 
                   #Noffspring per pair per gen
                   nOffspring = 2,
                   domRate = 0.2,
                   matingSystem = 0.9)
{  
  
# 2) Initialize temp and output variables 
  Id <- 1:popSize
  
  Sex <- vector(mode = "character", length = popSize)
  Sex[1:popSize] <- c(rep("male", popSize*sexRatio), 
                      rep("female", popSize*sexRatio) )
  
  Gen <- vector(mode = "integer", length = popSize)
  Gen[1:popSize] <- rep(1, each = popSize)
  
  Sire <- vector(mode = "integer", length = popSize)
  Sire[1:popSize] <- rep(NA, popSize)
  
  Dam <- vector(mode="integer", length=popSize)
  Dam[1:popSize] <- rep(NA, popSize)

# 3) generate breeding list each generation
## 3.1) determine breeding probability of males 
  #if popsize=10 & domrate =0.1 -> integer (0)
  
i=2

  Dom <- sample(Id[intersect(which(Gen == (i-1)) , which(Sex == "male"))],
           round(domRate*length(Id[intersect(which(Gen == (i-1)) , which(Sex == "male"))])))
  
  Sub <-Id[-c(match (Dom,Id),intersect(which(Gen == (i-1)) , which(Sex == "female")))] 

## 3.2) determine breeding pairs
   #for(i in 2:nGens){
   # sample(Id[intersect(which(Gen == (i-1)) , which(Sex == "male"))])
  #}
  
 for (i in Dom) {
      (sample(Id[intersect(which(Gen == (i-1)) , which(Sex == "female"))],
        round((matingSystem/length(Dom))*length(Id[intersect(which(Gen == (i-1)) , which(Sex == "female"))]))))
 }
  
   #if round set to 0.25* legth work 
   #if round set to .025 or .0025 -> integer (0)
 for (i in Sub) {
      (sample(Id[intersect(which(Gen == (i-1)) , which(Sex == "female"))],
        round((1-matingSystem)/length(Sub))*length(Id[intersect(which(Gen == (i-1)) , which(Sex == "female"))])))
 } 
  

## 3.3) making a data frame
  #initialise second gen 
  
  
  myPed <- data.frame(Id, Sire, Dam, Sex, Gen)
 for (i in 2:nGens){

