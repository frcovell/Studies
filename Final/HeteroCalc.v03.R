##### Working out founder heterozygosity #####

FounderHet<- function (myPed = NULL,
                       nLoci = NULL,
                       Founder = NULL){



mymat <- matrix(data=rep(0,2*nLoci), nrow=1, ncol=nLoci)
TPed<-which(is.na(myPed$Sire)& is.na(myPed$Dam))
FHet <- rep(list(mymat),length(TPed))

for (i in 1:length(TPed)) {
  for (j in 1:nLoci) {
    if(Founder[[i]][1,j] != Founder[[i]][2,j]){
      FHet[[i]][,j]<-1
    }
    
  }
  
}



IndFHet<-vector(mode = "integer", length = length(TPed))

for (i in 1:length(TPed)) {
  IndFHet[[i]] <- sum(FHet[[i]])/nLoci
} 

FoundHet<-sum(IndFHet)/length(TPed)

return(FoundHet)}


##### Working out current living population heterozygosity #####

LivingHet<- function(myPed = NULL,
                     nLoci = NULL,
                     Geno = NULL){
  
mymat <- matrix(data=rep(0,2*nLoci), nrow=1, ncol=nLoci)
TPed<-which(myPed$AgeAtDeath == "NA")
PHet <- rep(list(mymat),length(TPed))
test<-matrix(data = TPed, nrow = 1, ncol = length(TPed))


for (i in 1:length(TPed)) {
  k<-TPed[i]
  for (j in 1:nLoci) {
    if(Geno[[k]][1,j] != Geno[[k]][2,j]){
      PHet[[i]][,j]<-1
      }
  }
}

IndPHet<-vector(mode = "integer", length = length(TPed))

for (i in 1:length(TPed)) {
  IndPHet[[i]] <- sum(PHet[[i]])/nLoci
} 

PopHet<-sum(IndPHet)/length(TPed)
return(PopHet)}


