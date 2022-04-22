OptimalMate<- function(myPed=NULL){
  library(kinship2)
  library(pedigree)
  
for (i in 1:length(myMales)) {
w<-which(myKin[myFem] == min(myKin[myMales,1]))

TAge<-as.integer(myPed$Age)
TDeath<-as.integer(myPed$AgeAtDeath)
test<-TAge-TDeath
test[test<0] = 0 

x<-which(myPed$Age >= test)
y<- which(myPed$Id %in% myFem[w])
z<-which(x %in% y)
print(paste("For male", myMales[i]))
print(paste("Females", colnames(myKin[myMales,myFem])[temp1],
            "have the lowest kinship coefficient ==", min(myKin[myMales,myFem][i,])))
  }
}


