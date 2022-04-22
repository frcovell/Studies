####Genosim####
## genotype simulation
##level of relatedness of founders can be infered by probability of allels occuring

GenoSim <- function(nAllel = NULL,
                    nLoci = NULL,
                    popSize = NULL,
                    P.allel = NULL, ##for random use set.seed, for explicit c(probabilities)
                    myPed = NULL){

  myFun <- function (y) matrix(sample(x=nAllel, size=(nLoci*2), prob=P.allel, replace = T), 
                               nrow=2, ncol=nLoci)
  
  x <- 1:popSize 
  
  founder <- lapply( X = 1:popSize,
                     FUN = myFun)

    mymat <- matrix(data=rep(0,2*nLoci), nrow=2, ncol=nLoci)
    genotypes <- rep(list(mymat),max(myPed$Id))
    
    for(i in 1:length(founder)){
      genotypes[[i]] <- founder[[i]]
    }
    
 for (i in which(myPed$Gen > min(myPed$Gen))){
   for (j in 1:nLoci) {
     genotypes[[i]][1,j] <- genotypes[[myPed$Sire[i]]][sample(1:2,1),j]
     
     genotypes[[i]][2,j] <- genotypes[[myPed$Dam[i]]][sample(1:2,1),j]
   }
 }

   print((genotypes))


    }
