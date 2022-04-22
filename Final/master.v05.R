##### 00 Master code for pedigree simulation ####
#

#Ed work PC
#setwd("E:/Dropbox/WORK/courses/course__PROJECTS/__BSc/2017.18/Francesca Covell bongo pedigree/2019.02.20 latest code")
#setwd("H:/Documents/yr 4/project/Code")
#setwd("D:/Fran/university/Final year/Project/_New hope/Finished code")
setwd("/media/frcovell/K BETA/Fran/R project 1")
#libraries
install.packages("kinship2")
install.packages("pedigree")
install.packages("tictoc")

library(tictoc)
library(kinship2)
library(pedigree)



#### 01 Load functions ####
source("PedSim.v03.R")
source("GenoSim.v05.R")
source("HeteroCalc.v03.R")


##### 02 Run the sims of random mating system ####

tic()
Ped1<-RandPed(nGens = 20,
             popSize = 10,
             maleRatio = 0.5,
             breedMale = 0.9,
             breedFemale = 0.9,
             breedingAge = 3,
             P.ages = c(1,2,3,4,5,6),
             P.death = c(0.4, 0.45, 0.5, 0.55, 0.6, 1),
             P.babies = c(0,0,1,0),
             nBabies = c(0,1,2,3))
toc()
Ped1


tic()
Founder1<-FounderGenom(inBreeding = 0.1,
                       myPed = Ped1,
                       nAllel = 15,
                       nLoci = 10,
                       rSeed = 1010)
toc()

Geno1<-GenoSim(nLoci = 10,
               myPed = Ped1,
               Founder = Founder1)

#### 02.1 Testing Whole Pop Inbreeding Based on Pedigree ####

#Ped1Fam<-makefamid(id=Ped1$Id, father.id=Ped1$Sire, mother.id=Ped1$Dam)
PedKin<-kinship(id=Ped1$Id, dadid=Ped1$Sire, momid=Ped1$Dam, sex=Ped1$Sex)

calcG(Geno1)
F1.1<- calcInbreeding(Ped1)
(meanF1.1 <- mean(F1.1))
(FGE1.1 <- 0.5/meanF1.1)



##### 02.2 Testing Founder and Living Pop Inbreeding Based in Genotype ####

(FH1<-FounderHet(myPed = Ped1,
           nLoci = 10,
           Founder = Founder1))

(LH1<-LivingHet(myPed = Ped1,
          nLoci = 10,
          Geno = Geno1))
(F1.2<- 1-(LH1/FH1))
(FGE1.2<-0.5/F1.2)

##### 03  Run the sims of optimal mating system ####
Ped2<-OptPed(nGens = 20, ##Not perfect sire younger then offspring
             popSize = 10,
             maleRatio = 0.5,
             breedMale = 0.9,
             breedFemale = 0.9,
             breedingAge = 3,
             P.ages = c(1,2,3,4,5,6),
             P.death = c(0.4, 0.45, 0.5, 0.55, 0.6, 1),
             P.babies = c(0,0,1,0),
             nBabies = c(0,1,2,3))

tic()
Founder2<-FounderGenom(inBreeding = 0.1,
                       myPed = Ped2,
                       nAllel = 15,
                       nLoci = 10,
                       rSeed = 1010)
toc()

Geno2<-GenoSim(nLoci = 10,
               myPed = Ped2,
               Founder = Founder2)

#### 03.1 Testing Whole Pop Inbreeding Based on Pedigree ####

Ped2Fam<-makefamid(id=Ped2$Id, father.id=Ped2$Sire, mother.id=Ped2$Dam)
PedKin2<-kinship(id=Ped2$Id, dadid=Ped2$Sire, momid=Ped2$Dam, sex=Ped2$Sex)

F2.1<- calcInbreeding(Ped2)
(meanF2.1 <- mean(F2.1))
(FGE2.1 <- 0.5/meanF2.1)



##### 03.2 Testing Founder and Living Pop Inbreeding Based in Genotype ####

(FH2<-FounderHet(myPed = Ped2,
                nLoci = 10,
                Founder = Founder2))

(LH2<-LivingHet(myPed = Ped2,
               nLoci = 10,
               Geno = Geno2))
(F2.2<- 1-(LH2/FH2))
(FGE2.2<-0.5/F2.2)

#### 04 comparing inbreeding and FGE ####

meanF1.1
F1.2
meanF2.1
F2.2

FGE1.1
FGE1.2
FGE2.1
FGE2.2

#### 05 Bongo Pedigree Work ####
# Import Bongo Dataset

test <- read.csv(file.choose())
length(test$Id)
pedigree<-test[(length(test$Id)):1,]
(ord<-orderPed(pedigree))
pedigree <- pedigree[order(ord),]
BongoF<-calcInbreeding(pedigree)  
  
meanF <- mean(BongoF)
FGE <- 0.5/meanF

BongoFam<- makefamid(id=test$Id, father.id = test$Sire, mother.id = test$Dam)
BongoKin<-kinship(id=test$Id, dadid= test$Sire, momid=test$Dam)
