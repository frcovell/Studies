# Data Analysis
library(tidyverse)
library(gtools)
library(parallel)


#### Species Richness ####
# import data set
# measure species richness over percentage cover
# plot in line graph

## set up ##
gpath = "/home/frcovell/Project/Results/TNM_Output"
setwd(gpath)

# seach for path with str_subset(pattern)
h <- seq(from = 100, to = 500, by = 100) # Create sequence of first Param Seed No.

Result_Paths = list.dirs(path = gpath, recursive = TRUE) %>% str_subset(pattern = paste0("Seed_",h, "/")) %>% str_subset(pattern = "Seed_")
%>% str_subset(pattern = "txt/Results") %>% mixedsort() #Vector of Paths to Results


# function 
#Function to read in any TNM output from any seed, giving the path and the data you want as input
read_Output = function(res_Path, res) {
  
  if(res == "/totalPop.txt") {
    cols = c("g", "n")
  } else if(res == "/totalPopSpec.txt") {
    cols = c("g", "s", "n")
  } else if(res == "/cellPop.txt") {
    cols = c("g", "c", "n")
  } else if(res == "/cellPopSpec.txt") {
    cols = c("g", "c", "s", "n")
  } else if(res == "/cellRich.txt") {
    cols = c("g", "c", "n")
  }
  
  if(res == "/cellPopSpec.txt") {
    f <- function(x, pos) subset(x, g == 9976)
    data = read_delim_chunked(file = paste0(res_Path, res), delim = " ", col_names = cols, chunk_size = 10000, callback = DataFrameCallback$new(f), 
                              progress = FALSE)
  } else {
    data = read.table(file = paste0(res_Path, res), col.names = cols)
  }
  
  seed = str_split(res_Path, pattern = "/")[[1]][7] %>% str_split(., pattern = "_") %>% simplify() %>% nth(2)
  run = str_split(res_Path, pattern = "/")[[1]][9] %>% str_split(., pattern = "_") %>% simplify() %>% nth(2)
  patch = str_split(res_Path, pattern = "/")[[1]][10] %>% str_split(., pattern = "_") %>% simplify() %>% nth(2)%>% str_split(., pattern = ".t") %>% simplify() %>% nth(1)
  
  data = data %>% add_column(seed = seed, run = run, patch = patch)
  
  return(data)
  
}

SE = function(x) {
  sample_Size = length(x)
  SD = sd(x)
  SE = SD/sqrt(sample_Size)
  return(SE)
}

## testing species richness ##

totalPopSpecs = mcmapply(read_Output, Result_Paths, "/totalPopSpec.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
richness_SE = totalPopSpecs %>% filter(n > 0) %>% group_by(g, seed, run, patch) %>% summarise(rich = length(n)) %>% ungroup() %>% 
  group_by(patch, g) %>% summarise(mean_Rich = mean(rich), SE = SE(rich))

SppRchData <- as.data.frame(richness_SE)

## Visualising ##
# Line plots
# 1 for start gen of ragmentation value
# 1 for end gen fragmentation value
# Panneld by Immigration rate
# X= %Cover
# Y=SppRich
# Line coloured by disperal rate

ggplot(SppRchData, aes(x = g, y = mean_Rich, fill = patch)) + geom_line() + 
  geom_ribbon(aes(ymax = mean_Rich + SE, ymin = mean_Rich - SE), alpha = 0.2) +
  labs(title ="Baseline", x = "Generation", y = "Mean Species Richness") + 
  theme_bw()
ggplot(SppRchData, aes(x=patch, y=mean_Rich)) + 
  geom_line() + 
  geom_ribbon(aes(ymax = mean_Rich + SE, ymin = mean_Rich - SE), alpha = 0.2) +
  labs(title ="Baseline", x = "Generation", y = "Mean Species Richness") + 
  theme_bw()
