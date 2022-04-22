## Example Function ##

# This function calculates heights of trees given distance of each tree 
# from its base and angle to its top, using  the trigonometric formula 
#
# height = distance * tan(radians)
#
# ARGUMENTS
# degrees:   The angle of elevation of tree
# distance:  The distance from base of tree (e.g., meters)
#
# OUTPUT
# The heights of the tree, same units as "distance"

#Function to calculate Hight based on angle and distance 
TreeHeight <- function(degrees, distance){
  radians <- degrees * pi / 180
  height <- distance * tan(radians)
  print(paste("Tree height is:", height))
  
  return(height)
}

#test function
TreeHeight(37, 40)

#import csv file
tree <- read.csv("../data/trees.csv")
tree

#run function on csv 
Height <-vector() 
for (i in 1:length(tree$Species)) {
  Height[i]<-TreeHeight(tree$Angle.degrees[i], tree$Distance.m[i])
  
}

#create data frame of results 
TreeHts<- data.frame(Species =  tree$Species,
                     Distance.m = tree$Distance.m,
                     Angle.degrees = tree$Angle.degrees,
                     Tree.hight.m = Height )

#output dataframe as csv
write.csv( TreeHts, "../results/TreeHts.csv")
