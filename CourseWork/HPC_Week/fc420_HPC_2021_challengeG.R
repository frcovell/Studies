# CMEE 2021 HPC excercises R code challenge G pro forma

rm(list=ls()) # nothing written elsewhere should be needed to make this work

# please edit these data to show your information.
name <- "Francesca Covell"
preferred_name <- "Fran"
email <- "f.covell20@imperial.ac.uk"
username <- "fc420"

# don't worry about comments for this challenge - the number of characters used will be counted starting from here
G<-function(x,y,z,d){if(z>0.01){n<-turtle(c(x[1],x[2]),y,z)|d<-d*-1|
  G(c(n[1],n[2]),y,(z*0.87),d)|if(d==-1){G(c(n[1],n[2]),y+(pi/4),(z*0.38),d*-1)}else{G(c(n[1],n[2]),y-(pi/4),(z*0.38),d)}}}
