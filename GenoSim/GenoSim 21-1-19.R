

GenoSim <- function(nallel = NULL,
                    nloci = NULL,
                    popsize = NULL,
                    sexratio = NULL,
                    rseed = NULL,
                    myPed = NULL){
  myFun <- function (y) matrix(sample(x, nloci*2, replace = T), 
                               nrow=2, ncol=nloci)
  x <- 1:nallel
  set.seed(rseed)

  ### choosing random or explicity relatedness
#  if (pAllel == NULL){ 
#    set.seed(1096)
#  } else{
#    sample(x=seq(nallel), size=nloci, prob=pAllel)
#  }
  
    founder <- lapply( X = 1:popsize,
                     FUN = myFun)
    mymat <- matrix(data=rep(0,2*nloci), nrow=2, ncol=nloci)
    genotypes <- rep(list(mymat),max(myPed$Id))
    
    for(i in 1:length(founder)){
      genotypes[[i]] <- founder[[i]]
    }
    
 for (i in which(myPed$Gen > min(myPed$Gen))){
   for (j in 1:nloci) {
     genotypes[[i]][1,j] <- genotypes[[myPed$Sire[i]]][sample(1:2,1),j]
     
     genotypes[[i]][2,j] <- genotypes[[myPed$Dam[i]]][sample(1:2,1),j]
   }
 }

   print((genotypes))


    }
