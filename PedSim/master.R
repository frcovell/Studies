#####
#Master code for pedigree simulation
#

Ped1<-PedSim(nGens =10,
             popSize = 11,
             sexRatio = 0.5,
             breedMale = 0.9,
             breedFemale = 0.9,
             breedingAge = 3,
             P.ages = c(1,2,3,4,5,6),
             P.death = c(0.42, 0.47, 0.53, 0.56, 0.63, 1),
             pbabies = c(0,0,1,0),
             nbabies = c(0,1,2,3))
Ped1

Ped2<-PedSim(nGens =100,
             popSize = 11,
             sexRatio = 0.5,
             breedMale = 0.9,
             breedFemale = 0.9,
             breedingAge = 3,
             P.ages = c(1,2,3,4,5,6),
             P.death = c(0.42, 0.47, 0.53, 0.56, 0.63, 1),
             pbabies = c(0,0,1,0),
             nbabies = c(0,1,2,3))
Ped2
