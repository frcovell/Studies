
myPed<-Ped1
OptimalMate<- function(myPed=NULL){
  library(kinship2)
  myMales <- which(myPed$Sex=="male")
  myFem <- which(myPed$Sex=="female") 
  myKin <-kinship(id=Ped1$Id, dadid=Ped1$Sire, momid=Ped1$Dam, sex=Ped1$Sex)

  for (i in 1:length(myMales)) {
w<-which(myKin[myMales] == min(myKin[myFem,i]))

TAge<-as.integer(myPed$Age)
TDeath<-as.integer(myPed$AgeAtDeath)
test<-TAge-TDeath
test[test<0] = 0 

x<-which(myPed$Age >= test)
y<- which(myPed$Id %in% myFem[w])
z<-which(x %in% y)



print(paste("For male", myMales[i]))
print(paste("Females", colnames(myKin[myMales,myFem])[w],
           "have the lowest kinship coefficient ==", min(myKin[myMales,myFem][i,])))
  }
}



