#### creating ancestors #### 
#AncestorGenom<-function(nAncestor = NULL,
#                        nAllel = NULL,
#                       nLoci = NULL,
#                      rSeed = NULL){


#ancGeno <- function (y) matrix(sample(x=nAllel, nLoci*2, replace = T), 
#                              nrow=2, ncol=nLoci)

#x <- 1:nAncestor
#set.seed(rSeed)


#Ancestor <- lapply( X = 1:nAncestor,
 #                  FUN = ancGeno)
#return(Ancestor)
#}

#### Genotyping founders ####
FounderGenom<- function(inBreeding = NULL,
                        myPed = NULL,
                        nAllel = NULL,
                        nLoci = NULL,
                        rSeed = NULL){
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
myPed<-which(is.na(Ped1$Sire)& is.na(Ped1$Dam))
Founder <- rep(list(mymat),length(myPed))


for(i in 1:length(myPed))  {
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

return(Founder)
}
#### Whole population genotype ####

GenoSim<- function (nLoci = NULL,
                    myPed = NULL,
                    Founder = NULL){

mymat <- matrix(data=rep(0,2*nLoci), nrow=2, ncol=nLoci)
Genotypes <- rep(list(mymat),max(myPed$Id))

for(i in 1:length(Founder)){
  Genotypes[[i]] <- Founder[[i]]
}

for (i in which(myPed$Gen > min(myPed$Gen))){
  for (j in 1:nLoci) {
    Genotypes[[i]][1,j] <- Genotypes[[myPed$Sire[i]]][sample(1:2,1),j]
    
    Genotypes[[i]][2,j] <- Genotypes[[myPed$Dam[i]]][sample(1:2,1),j]
  }
}


return(Genotypes)


}


