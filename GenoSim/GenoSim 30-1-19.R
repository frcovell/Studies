#### creating ancestors #### 
nAncestor <- 1
nAllel <- 15
nLoci <- 10
rSeed <-1010


ancGeno <- function (y) matrix(sample(x=nAllel, nLoci*2, replace = T), 
                              nrow=2, ncol=nLoci)

x <- 1:nAncestor
set.seed(rSeed)


Ancestor <- lapply( X = 1:nAncestor,
                   FUN = ancGeno)

#### Genotyping founders ####
inBreeding <- 0.3
popSize <- 11
nAllel <- 15
nLoci <- 10

mymat <- matrix(data=rep(0,2*nLoci), nrow=2, ncol=nLoci)
Founder <- rep(list(mymat),popSize)

#for individuals in founder genotype
  # for each allel at each locus if allel from founder sample .5 from founder
   # else random sample




