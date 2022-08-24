#### Data analysis set up ####
gpath = "/home/frcovell/Project/Results/TNM_Output"
setwd(gpath)

## Requirements ##
library(tidyverse)
library(gtools)
library(parallel)
library(ggpubr)
library(janitor)
library(lemon)
library(vegan)
library(ggsci)
library(RColorBrewer)
library(cowplot)
##Load functions ##
source("../../Code/TNM_Code/DataAnalysis/Data_Analysis_Functions.R")

## Create Dataframe for analysis ##
# search Directory path for fragmantation 
fragSeedPaths = list.files(path = gpath, pattern = "Landscape", recursive = T, full.name = T, include.dirs = T) %>% 
  grep(pattern = "Fragmentation", ., value = T)
# search for File path 
Param_Paths = list.files(path = "../../Results/TNM_Output", pattern = "^Parameters.txt", recursive = T, full.name = T) ## ^ means it is the beginning of the string

# create DataFrame
parameters = mclapply(Param_Paths, get_Param, mc.cores = 6) %>% bind_rows() ## If you are not using a Linux OS you will have to just use lapply and deal with the speed

frags = sapply(fragSeedPaths, get_Frags, simplify = F) %>% bind_rows()
params = frags %>% left_join(parameters)
params$frag <- as.numeric(params$frag)

#Calling data
totalPopSpec = get_Data("totalPopSpec") %>% left_join(params)

#### Species Richness ####
Mean_Rich = totalPopSpec %>% filter(pop > 0) %>% group_by(seed, nseed, frag) %>% summarise(rich = length(pop)) %>% ungroup() %>% 
  group_by(nseed) %>% summarise(Mean_Rich = mean(rich), SE = SE(rich))
SppRich_SE = params %>% left_join(Mean_Rich)

## Dispersal ##
DispSppRich0Imm <- ggplot(filter(SppRich_SE, intra == 0 & inter == 0 & disp != 1.5 & imm== 0),aes(x=frag, y=Mean_Rich, col = factor(disp)))+ 
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth(method = "loess", se=FALSE)+
  theme_classic2()+
  theme(legend.position="none", plot.title=element_text(size=9), axis.title=element_text(size=9), plot.tag = element_text(size =9))+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)))+
  scale_color_manual(values=c("red", "green", "blue" ), labels= c("Zero", "Low","High"))+
  xlab("Habitat Cover %")+
  ylab("Species Richness")+
  labs(color="Dispersal\nDistance", tag = "A", title = "No Immigration Rate")

DispSppRichImm <- ggplot(filter(SppRich_SE, intra == 0 & inter == 0 & disp != 1.5 & imm== 0.00016529),aes(x=frag, y=Mean_Rich, col = factor(disp)))+ 
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth(method = "loess",se=FALSE)+
  theme_classic2()+
  theme(legend.position="none", plot.title=element_text(size=9),axis.title=element_text(size=9),plot.tag = element_text(size =9))+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)))+
  scale_color_manual(values=c("red", "green", "blue" ), labels= c("Zero", "Low","High"))+
  xlab("Habitat Cover %")+
  ylab("Species Richness")+
  labs(color="Dispersal\nDistance", tag = "E", title = "Immigration")

## Neighbourhood Area ##

NeighSppRich0Disp <- ggplot(filter(SppRich_SE, intra == 0 & disp == 0 & imm == 0.00016529),aes(x=frag, y=Mean_Rich, col = factor(inter)))+ 
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth(method = "loess", se=FALSE)+
  theme_classic2()+
  theme(legend.position= "none", plot.title=element_text(size=9), axis.title=element_text(size=9), plot.tag = element_text(size =9))+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)))+
  scale_color_manual(values=c("brown4", "chartreuse4", "deepskyblue4"),labels= c("Zero", "Low","Medium"))+
  xlab("Habitat Cover %")+
  ylab("Species Richness")+
  labs(color="Neighbourhood\nArea", tag = "A", title = "No Disperal Area")

NeighSppRichDisp <- ggplot(filter(SppRich_SE, intra == 0 & disp == 1 & imm == 0.00016529),aes(x=frag, y=Mean_Rich, col = factor(inter)))+ 
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth(method = "loess", se=FALSE)+
  theme_classic2()+
  theme(legend.position= "none", plot.title=element_text(size=9), axis.title=element_text(size=9), plot.tag = element_text(size =9))+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)))+
  scale_color_manual(values=c("brown4", "chartreuse4", "deepskyblue4"),labels= c("Zero", "Low","Medium"))+
  xlab("Habitat Cover %")+
  ylab("Species Richness")+
  labs(color="Dispersal\nDistance", tag = "C", title = "Dispersal Area")

## Intraspecific Competition ##
IntaSppRich0Neigh <- ggplot(filter(SppRich_SE, inter == 0 & disp == 1 & imm == 0.00016529),aes(x=frag, y=Mean_Rich, col = factor(intra)))+ 
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth(method = "loess", se=FALSE)+
  theme_classic2()+
  theme(legend.position= "none", plot.title=element_text(size=9), axis.title=element_text(size=9), plot.tag = element_text(size =9))+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)))+
  scale_color_manual(values=c("tomato1", "springgreen1", "steelblue1", "tan1" ),  labels= c("High", "Medium","Low","Zero"))+ 
  xlab("Habitat Cover %")+
  ylab("Species Richness")+
  labs(color="Intraspecific\nCompetition", tag = "A",  title = "No Neighbourhood Area")

IntaSppRichNeigh <- ggplot(filter(SppRich_SE, inter == 1 & disp == 1 & imm == 0.00016529),aes(x=frag, y=Mean_Rich, col = factor(intra)))+ 
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth(method = "loess", se=FALSE)+
  theme_classic2()+
  theme(legend.position= "none", plot.title=element_text(size=9), axis.title=element_text(size=9), plot.tag = element_text(size =9))+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)))+
  scale_color_manual(values=c("tomato1", "springgreen1", "steelblue1", "tan1" ),  labels= c("High", "Medium","Low","Zero"))+  
  xlab("Habitat Cover %")+
  ylab("Species Richness")+
  labs(color="Intraspecific\nCompetition", tag = "C", title = "Neighbourhood Area")

#### Mean SvG for Dispersal ####
## Call data ##
AbundancePopSpec = within(totalPopSpec, rm(spec))
colnames(AbundancePopSpec)[2] <- "spec"
## first filter  and set up ##
DispAbundance<-filter(AbundancePopSpec, disp== 0 & inter == 0 & imm == 0.00016529)

Seeds = unique(DispAbundance$seed)

#PerCov1
PerCov1SVG<-vector()
PerCov1Spp<-vector()
for (i in 1:length(Seeds)) {
  SvG= as.data.frame(read.table(paste0("/home/frcovell/Project/Results/TNM_Output/Seed_",Seeds[i],"/SvG.txt")))
  df=data.frame(Spp = seq(1, 95))
  df[ , ncol(df) + 1] <- SvG 
  k = 1
  for (j in 1:5) {
    SppSvG= read.table(paste0("/home/frcovell/Project/Results/TNM_Output/Seed_",Seeds[i],"/Fragmentation/Seed_",as.numeric(Seeds[i])+(j),"/Landscape1_1.txt/Final_Frag_Output/final_frag_totalPopSpec.txt")) 
    df[ , ncol(df) + 1] <- SppSvG 
    df<-filter_if(df, is.numeric, all_vars((.) != 0))
    PerCov1Spp<-append(PerCov1Spp, length(df$V1.1))
    PerCov1SVG<-append(PerCov1SVG, df$V1)   
  }
}

#PerCov0
PerCov0SVG<-vector()
PerCov0Spp<-vector()
for (i in 1:length(Seeds)) {
  SvG= as.data.frame(read.table(paste0("/home/frcovell/Project/Results/TNM_Output/Seed_",Seeds[i],"/SvG.txt")))
  df=data.frame(Spp = seq(1, 95))
  df[ , ncol(df) + 1] <- SvG 
  k = 2 
  for (j in 6:10) {
    SppSvG = read.table(paste0("/home/frcovell/Project/Results/TNM_Output/Seed_",Seeds[i],"/Fragmentation/Seed_",as.numeric(Seeds[i])+(j),"/Landscape1_0.txt/Final_Frag_Output/final_frag_totalPopSpec.txt")) 
    df[ , ncol(df) + 1] <- SppSvG 
    df<-filter_if(df, is.numeric, all_vars((.) != 0))
    PerCov0Spp<-append(PerCov0Spp, length(df$V1.1))
    PerCov0SVG<-append(PerCov0SVG, df$V1)
    
  }
}
#PerCov 0.1 - 0.9
Covers=c(1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
PerCovsSvG<-vector()
nSpp<-vector()
MeanSVG<-vector()
MeanNo.Spp<-vector()
SVGPerCov<-vector()
SppPerCov<-vector()
Disp <- vector()
Imm<-vector()
CovSvG<-data.frame(SVGPerCov= integer,
                   MeanSVG=integer)
CovMeans<-data.frame(SVGPerCov= c(1,0),
                     MeanSVG=c(mean(PerCov1SVG),mean(PerCov0SVG)),
                     MeanNo.Spp= c(mean(PerCov1Spp),mean(PerCov0Spp)),
                     Imm=rep(DispAbundance$imm[1], 2),
                     Disp=rep(DispAbundance$disp[1], 2))
CovMeans$SVGPerCov<-c(1,0)

s=11
e=15
for (i in 1:length(Seeds)) {
  SvG= as.data.frame(read.table(paste0("/home/frcovell/Project/Results/TNM_Output/Seed_",Seeds[i],"/SvG.txt")))
  df=data.frame(Spp = seq(1, 95))
  df[ , ncol(df) + 1] <- SvG 
  for (k in 3:11) {
    for (j in s:e) {
      SppSvG = read.table(paste0("/home/frcovell/Project/Results/TNM_Output/Seed_",Seeds[i],"/Fragmentation/Seed_",as.numeric(Seeds[i])+(j),"/AmalgamationLandscape1_",Covers[k],".txt/Final_Frag_Output/final_frag_totalPopSpec.txt")) 
      df[ , ncol(df) + 1] <- SppSvG
      df<-filter_if(df, is.numeric, all_vars((.) != 0))
      PerCovsSvG<-append(PerCovsSvG, df$V1)
      nSpp<-append(nSpp, length(df$V1.1))
      df=data.frame(Spp = seq(1, 95))
      df[ , ncol(df) + 1] <- SvG 
    }
    MeanSVG<-append(MeanSVG,PerCovsSvG)
    SVGPerCov<-append(SVGPerCov,rep(Covers[k], length(PerCovsSvG)))
    Disp<-append(Disp,CovMeans$Disp[1])
    Imm<-append(Disp,CovMeans$Imm[1])
    HolderSVG<-data.frame(SVGPerCov, MeanSVG)
    MeanNo.Spp<-append(MeanNo.Spp,nSpp)
    SppPerCov<-append(SppPerCov,rep(Covers[k], length(nSpp)))
    HolderSpp<-data.frame(SppPerCov, MeanNo.Spp)
    PerCovsSvG<-vector()
    nSpp<-vector()
    PerCovsSvG<-vector()
    s=s+5
    e=e+5
    if (s == 56) {
      s=11
      e=15
    }
  }  
  
  CovSvG<-rbind(CovSvG, HolderSVG)
}



CovSvG<-CovSvG[order(CovSvG$SVGPerCov),]
HolderSpp<-HolderSpp[order(HolderSpp$SppPerCov),]
for (i in 3:11) {
  Holder1<-data.frame(SVGPerCov = Covers[i], 
                      MeanSVG =mean(CovSvG$MeanSVG[min(which(CovSvG$SVGPerCov == Covers[i])):max(which(CovSvG$SVGPerCov == Covers[i]))]),
                      MeanNo.Spp = mean(HolderSpp$MeanNo.Spp[min(which(HolderSpp$SppPerCov == Covers[i])):max(which(HolderSpp$SppPerCov == Covers[i]))]),
                      Disp = CovMeans$Disp[1],
                      Imm = CovMeans$Imm[1])
  CovMeans<-rbind(CovMeans, Holder1)
}

### repeate following for all filters ###
DispAbundance<-filter(AbundancePopSpec, disp== 0 & inter == 0 & imm == 0) #!= 1.6529e-05 )
#DispAbundance<-filter(AbundancePopSpec, disp== 1 & inter == 0 & imm == 0.00016529) #!= 1.6529e-05 )
#DispAbundance<-filter(AbundancePopSpec, disp== 1 & inter == 0 & imm == 0) #!= 1.6529e-05 )
#DispAbundance<-filter(AbundancePopSpec, disp== 1.5 & inter == 0 & imm == 0.00016529) #!= 1.6529e-05 )
#DispAbundance<-filter(AbundancePopSpec, disp== 1.5 & inter == 0 & imm == 0) #!= 1.6529e-05 )
#DispAbundance<-filter(AbundancePopSpec, disp== 2.5 & inter == 0 & imm == 0.00016529) #!= 1.6529e-05 )
#DispAbundance<-filter(AbundancePopSpec, disp== 2.5 & inter == 0 & imm == 0) #!= 1.6529e-05 )


Seeds = unique(DispAbundance$seed)
#PerCov1
PerCov1SVG<-vector()
PerCov1Spp<-vector()

for (i in 1:length(Seeds)) {
  SvG= as.data.frame(read.table(paste0("/home/frcovell/Project/Results/TNM_Output/Seed_",Seeds[i],"/SvG.txt")))
  df=data.frame(Spp = seq(1, 95))
  df[ , ncol(df) + 1] <- SvG 
  k = 1
  for (j in 1:5) {
    SppSvG= read.table(paste0("/home/frcovell/Project/Results/TNM_Output/Seed_",Seeds[i],"/Fragmentation/Seed_",as.numeric(Seeds[i])+(j),"/Landscape1_1.txt/Final_Frag_Output/final_frag_totalPopSpec.txt")) 
    df[ , ncol(df) + 1] <- SppSvG 
    df<-filter_if(df, is.numeric, all_vars((.) != 0))
    PerCov1Spp<-append(PerCov1Spp, length(df$V1.1))
    PerCov1SVG<-append(PerCov1SVG, df$V1)   
  }
}

#PerCov0
PerCov0SVG<-vector()
PerCov0Spp<-vector()
for (i in 1:length(Seeds)) {
  SvG= as.data.frame(read.table(paste0("/home/frcovell/Project/Results/TNM_Output/Seed_",Seeds[i],"/SvG.txt")))
  df=data.frame(Spp = seq(1, 95))
  df[ , ncol(df) + 1] <- SvG 
  k = 2 
  for (j in 6:10) {
    SppSvG = read.table(paste0("/home/frcovell/Project/Results/TNM_Output/Seed_",Seeds[i],"/Fragmentation/Seed_",as.numeric(Seeds[i])+(j),"/Landscape1_0.txt/Final_Frag_Output/final_frag_totalPopSpec.txt")) 
    df[ , ncol(df) + 1] <- SppSvG 
    df<-filter_if(df, is.numeric, all_vars((.) != 0))
    PerCov0Spp<-append(PerCov0Spp, length(df$V1.1))
    PerCov0SVG<-append(PerCov0SVG, df$V1)
    
  }
}
#PerCov 0.1 - 0.9
Covers=c(1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
PerCovsSvG<-vector()
nSpp<-vector()
MeanSVG<-vector()
MeanNo.Spp<-vector()
SVGPerCov<-vector()
SppPerCov<-vector()
CovSvG<-data.frame(SVGPerCov= integer,
                   MeanSVG=integer)
HoldCovMeans<-data.frame(SVGPerCov= c(1,0),
                         MeanSVG=c(mean(PerCov1SVG),mean(PerCov0SVG)),
                         MeanNo.Spp= c(mean(PerCov1Spp),mean(PerCov0Spp)),
                         Imm=rep(DispAbundance$imm[1], 2),
                         Disp=rep(DispAbundance$disp[1], 2))
CovMeans<-rbind(CovMeans,HoldCovMeans)

s=11
e=15
for (i in 1:length(Seeds)) {
  SvG= as.data.frame(read.table(paste0("/home/frcovell/Project/Results/TNM_Output/Seed_",Seeds[i],"/SvG.txt")))
  df=data.frame(Spp = seq(1, 95))
  df[ , ncol(df) + 1] <- SvG 
  for (k in 3:11) {
    for (j in s:e) {
      SppSvG = read.table(paste0("/home/frcovell/Project/Results/TNM_Output/Seed_",Seeds[i],"/Fragmentation/Seed_",as.numeric(Seeds[i])+(j),"/AmalgamationLandscape1_",Covers[k],".txt/Final_Frag_Output/final_frag_totalPopSpec.txt")) 
      df[ , ncol(df) + 1] <- SppSvG
      df<-filter_if(df, is.numeric, all_vars((.) != 0))
      PerCovsSvG<-append(PerCovsSvG, df$V1)
      nSpp<-append(nSpp, length(df$V1.1))
      df=data.frame(Spp = seq(1, 95))
      df[ , ncol(df) + 1] <- SvG 
    }
    MeanSVG<-append(MeanSVG,PerCovsSvG)
    SVGPerCov<-append(SVGPerCov,rep(Covers[k], length(PerCovsSvG)))
    Disp<-append(Disp,CovMeans$Disp[length(CovMeans$Disp)])
    Imm<-append(Disp,CovMeans$Imm[length(CovMeans$Imm)])
    HolderSVG<-data.frame(SVGPerCov, MeanSVG)
    MeanNo.Spp<-append(MeanNo.Spp,nSpp)
    SppPerCov<-append(SppPerCov,rep(Covers[k], length(nSpp)))
    HolderSpp<-data.frame(SppPerCov, MeanNo.Spp)
    PerCovsSvG<-vector()
    nSpp<-vector()
    PerCovsSvG<-vector()
    s=s+5
    e=e+5
    if (s == 56) {
      s=11
      e=15
    }
  }  
  
  CovSvG<-rbind(CovSvG, HolderSVG)
}



CovSvG<-CovSvG[order(CovSvG$SVGPerCov),]
HolderSpp<-HolderSpp[order(HolderSpp$SppPerCov),]
for (i in 3:11) {
  Holder1<-data.frame(SVGPerCov = Covers[i], 
                      MeanSVG =mean(CovSvG$MeanSVG[min(which(CovSvG$SVGPerCov == Covers[i])):max(which(CovSvG$SVGPerCov == Covers[i]))]),
                      MeanNo.Spp = mean(HolderSpp$MeanNo.Spp[min(which(HolderSpp$SppPerCov == Covers[i])):max(which(HolderSpp$SppPerCov == Covers[i]))]),
                      Disp = CovMeans$Disp[length(CovMeans$Disp)],
                      Imm = CovMeans$Imm[length(CovMeans$Imm)])
  CovMeans<-rbind(CovMeans, Holder1)
}


#Plot

#CovMeans%>% arrange(desc(Disp))
SVG0Imm<-ggplot(filter(CovMeans%>% arrange(desc(Disp)), Disp != 1.5, Imm == 0), aes(x=SVGPerCov, y=MeanSVG, col = factor(Disp)))+ 
  geom_point(size = 1) +
  geom_smooth(method = "loess", se=FALSE)+
  theme_classic2()+
  theme(legend.position="none", plot.title=element_text(size=9), axis.title=element_text(size=9), plot.tag = element_text(size =9))+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)))+
  scale_color_manual(values=c("red", "green", "blue"  ), labels= c("Zero", "Low","High"))+
  xlab("Habitat Cover %")+
  ylab("Mean Species Specialism")+
  labs(color="Dispersal\nDistance", tag = "D", title = "No Immigration")


SVGImm<-ggplot(filter(CovMeans%>% arrange(desc(Disp)), Disp != 1.5, Imm ==0.00016529 ), aes(x=SVGPerCov, y=MeanSVG, col = factor(Disp), group=rev(Disp)))+ 
  geom_point(size = 1) +
  geom_smooth(method = "loess",se=FALSE)+
  theme_classic2()+
  theme(legend.position="none", plot.title=element_text(size=9), axis.title=element_text(size=9),  plot.tag = element_text(size =9))+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)))+
  scale_color_manual(values=c("red", "green", "blue" ), labels= c("Zero", "Low","High"))+ 
  xlab("Habitat Cover %")+
  ylab("Mean Species Specialism")+
  labs(color="Dispersal\nDistance", tag = "H", title = "Immigration")

#### Similarity ####
## Requirements ##
library(segmented) # needs to be called after species richness complete, bad interaction is tidyverse

## Call data ##
AbundancePopSpec = within(totalPopSpec, rm(spec))
colnames(AbundancePopSpec)[2] <- "spec"

## Similarity Loop ##
# to be run with filtered data
#rest vectors
DisSim<-vector()
Percov<-vector()
TestSeed<-vector()
TestnSeed1<-vector()
TestnSeed2<-vector()
Disp<- vector()
Imm<- vector()
Intra<- vector()
Inter<- vector()
UniqSeed<-unique(DispAbundance$seed)


for (i in 1:length(UniqSeed)) {
  FilterSeed<-UniqSeed[i]
  FilterAbundance <- filter(DispAbundance, seed == as.numeric(FilterSeed))
  for (j in 1:5) {
    nseed1 <- as.numeric(FilterSeed)+j
    for (k in 1:55) {
      nseed2 <-as.numeric(FilterSeed)+k
      if (k != j) {
        x <- filter(FilterAbundance, nseed == nseed1| nseed == nseed2)%>% pivot_wider(names_from = spec, values_from = pop)
        y <- within(x, rm(nseed, seed, frag, disp, inter, intra, imm))
        #DisSim<-append(DisSim,1-vegdist(y, method = "bray", binary=FALSE),after = max(length(DisSim))) #Sorensen method
        DisSim<-append(DisSim,1-vegdist(y, method = "jaccard"),after = max(length(DisSim))) #Jaccard method
        Percov<- append(Percov, x$frag[2],after = max(length(Percov)))
        TestSeed<- append(TestSeed, x$seed[1],after = max(length(TestSeed)))
        TestnSeed1<- append(TestnSeed1, x$nseed[1],after = max(length(TestnSeed1)))
        TestnSeed2<- append(TestnSeed2, x$nseed[2],after = max(length(TestnSeed2)))
        Disp<- append(Disp, FilterAbundance$disp[1], after = max(length(Disp)))
        Imm<- append(Imm, FilterAbundance$imm[1], after = max(length(Imm)))
        Intra<- append(Intra, FilterAbundance$intra[1], after = max(length(Intra)))
        Inter<- append(Inter, FilterAbundance$inter[1], after = max(length(Inter)))
      } else{ 
        x <- filter(FilterAbundance, nseed == nseed1| nseed == nseed2)%>% pivot_wider(names_from = spec, values_from = pop)
        DisSim<-append(DisSim,1 ,after = max(length(DisSim)))
        Percov<- append(Percov, x$frag,after = max(length(Percov)))
        TestSeed<- append(TestSeed, x$seed,after = max(length(TestSeed)))
        TestnSeed1<- append(TestnSeed1, x$nseed,after = max(length(TestnSeed1)))
        TestnSeed2<- append(TestnSeed2, x$nseed,after = max(length(TestnSeed2)))
        Disp<- append(Disp, FilterAbundance$disp[1], after = max(length(Disp)))
        Imm<- append(Imm, FilterAbundance$imm[1], after = max(length(Imm)))
        Intra<- append(Intra, FilterAbundance$intra[1], after = max(length(Intra)))
        Inter<- append(Inter, FilterAbundance$inter[1], after = max(length(Inter)))
      }
    }
  }
}

## Dispersal ##
DispAbundance<-filter(AbundancePopSpec, intra == 0 & disp != 1.5 &inter == 0 & imm != 1.6529e-05 & imm != 0.002)
# reset vectors and run loop using Jaccard method#
JacData<-data_frame(TestnSeed1,TestnSeed2, Percov, DisSim, Disp, Imm, Intra, Inter)
#JacData$Method <- rep("Jac", length(JacData$TestnSeed1))
# reset vectors and run loop using Sorrensen method#
SorData<-data_frame(TestnSeed1,TestnSeed2, Percov, DisSim, Disp, Imm, Intra, Inter)
#SorData$Method <- rep("Sor", length(SorData$TestnSeed1))

#Data<-rbind(JacData, SorData)

JacDispSim0Imm<-ggplot(filter(JacData, Imm ==0), aes(x=Percov, y=DisSim, col = factor(Disp)))+
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth(method = "loess", se=FALSE)+
  theme_classic2()+
  theme(legend.position="none", plot.title=element_text(size=9),axis.title=element_text(size=9),  plot.tag = element_text(size =9))+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)))+
  scale_color_manual(values=c("red", "green", "blue" ), labels= c("Zero", "Low","High"))+
  xlab("Habitat Cover %")+
  ylab("Similarity")+
  labs(color="Dispersal\nDistance", tag = "B", title = " Jaccard No Immigration")

SorDispSim0Imm<-ggplot(filter(SorData, Imm ==0), aes(x=Percov, y=DisSim, col = factor(Disp)))+
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth(method = "loess",se=FALSE)+
  theme_classic2()+
  theme(legend.position="none", plot.title=element_text(size=9),axis.title=element_text(size=9),  plot.tag = element_text(size =9))+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)))+
  scale_color_manual(values=c("red", "green", "blue" ), labels= c("Zero", "Low","High"))+
  xlab("Habitat Cover %")+
  ylab("Similarity")+
  labs(color="Dispersal\nDistance", tag = "C", title = " Sorensen No Immigration")

JacDispSimImm<-ggplot(filter(JacData, Imm ==0.00016529), aes(x=Percov, y=DisSim, col = factor(Disp)))+
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth(method = "loess", se=FALSE)+
  theme_classic2()+
  theme(legend.position="none", plot.title=element_text(size=9),axis.title=element_text(size=9),  plot.tag = element_text(size =9))+
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.1)))+
  scale_color_manual(values=c("red", "green", "blue" ), labels= c("Zero", "Low","High"))+
    xlab("Habitat Cover %")+
  ylab("Similarity")+
  labs(color="Dispersal\nDistance",  tag = "F", title = " Jaccard Immigration")

SorDispSimImm<-ggplot(filter(SorData, Imm ==0.00016529), aes(x=Percov, y=DisSim, col = factor(Disp)))+
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth(method = "loess",se=FALSE)+
  theme_classic2()+
  theme(legend.position="none",plot.title=element_text(size=9),axis.title=element_text(size=9),  plot.tag = element_text(size =9))+
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.1)))+
  scale_color_manual(values=c("red", "green", "blue" ), labels= c("Zero", "Low","High"))+
  xlab("Habitat Cover %")+
  ylab("Similarity")+
  labs(color="Dispersal\nDistance",  tag = "G", title = " Sorrensen Disperal")

## Neighbourhood ##

DispAbundance<-filter(AbundancePopSpec,intra == 0 & disp ==0 & imm == 0.00016529)
#run above loop
Data<-data_frame(TestnSeed1,TestnSeed2, Percov, DisSim, Disp, Imm, Intra, Inter)
#unique(Data$Disp)

NeighSim0Disp<-ggplot(Data, aes(x=Percov, y=DisSim, col = factor(Inter)))+
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth(method = "loess",se=FALSE)+
  theme_classic2()+
  theme(legend.position = 'none', plot.title=element_text(size=9), axis.title=element_text(size=9),  plot.tag = element_text(size =9))+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)))+
  scale_color_manual(values=c("brown4", "chartreuse4", "deepskyblue4"),labels= c("Zero", "Low","Medium"))+
  xlab("Habitat Cover %")+
  ylab("Similarity")+
  labs(color="Neighbourhood\nArea", tag = "B", title = " Jaccard\nNo Dispersal")

DispAbundance<-filter(AbundancePopSpec,intra == 0 & disp ==1 & imm == 0.00016529)
#run above loop
Data<-data_frame(TestnSeed1,TestnSeed2, Percov, DisSim, Disp, Imm, Intra, Inter)

NeighSimDisp<-ggplot(Data, aes(x=Percov, y=DisSim, col = factor(Inter)))+
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth(method = "loess",se=FALSE)+
  theme_classic2()+
  theme(legend.position = 'none', plot.title=element_text(size=9), axis.title=element_text(size=9), plot.tag = element_text(size =9))+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)))+
  scale_color_manual(values=c("brown4", "chartreuse4", "deepskyblue4"),labels= c("Zero", "Low","Medium"))+
  xlab("Habitat Cover %")+
  ylab("Similarity")+
  labs(color="Neighbourhood\nArea", tag = "D", title = " Jaccard\nDispersal")

## Intraspecific Competition ##
DispAbundance<-filter(AbundancePopSpec, inter == 0 & disp == 1 & imm == 0.00016529)
#run loops
Data<-data_frame(TestnSeed1,TestnSeed2, Percov, DisSim, Disp, Imm, Intra, Inter)

IntaSim0Neigh <- ggplot(Data, aes(x=Percov, y=DisSim, col = factor(Intra)))+ 
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth(method = "loess",se=FALSE)+
  theme_classic2()+
  theme(legend.position= "none", plot.title=element_text(size=9), axis.title=element_text(size=9), plot.tag = element_text(size =9))+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)))+
  scale_color_manual(values=c("tomato1", "springgreen1", "steelblue1", "tan1" ),  labels= c("High", "Medium","Low","Zero"))+ 
  xlab("Habitat Cover %")+
  ylab("Similarity")+
  labs(color="Intraspecific\nCompetition", tag = "B",  title = "Jaccard\nNo Neighbourhood Area")

DispAbundance<-filter(AbundancePopSpec, inter == 1 & disp == 1 & imm == 0.00016529)
#run loops
Data<-data_frame(TestnSeed1,TestnSeed2, Percov, DisSim, Disp, Imm, Intra, Inter)

IntaSimNeigh <- ggplot(Data, aes(x=Percov, y=DisSim, col = factor(Intra)))+ 
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth(method = "loess",se=FALSE)+
  theme_classic2()+
  theme(legend.position= "none", plot.title=element_text(size=9), axis.title=element_text(size=9), plot.tag = element_text(size =9))+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)))+
  scale_color_manual(values=c("tomato1", "springgreen1", "steelblue1", "tan1" ),  labels= c("High", "Medium","Low","Zero"))+  
  xlab("Habitat Cover %")+
  ylab("Similarity")+
  labs(color="Intraspecific\nCompetition", tag = "D", title = "Jaccard\nNeighbourhood Area")
#### Critical Threcholds ####
library(segmented) #If similarity not run
## Call data ##
AbundancePopSpec = within(totalPopSpec, rm(spec))
colnames(AbundancePopSpec)[2] <- "spec"

## Similarity Loop ##
# to be run with filtered data
#rest vectors
DisSim<-vector()
Percov<-vector()
TestSeed<-vector()
TestnSeed1<-vector()
TestnSeed2<-vector()
Disp<- vector()
Imm<- vector()
Intra<- vector()
Inter<- vector()
UniqSeed<-unique(DispAbundance$seed)


for (i in 1:length(UniqSeed)) {
  FilterSeed<-UniqSeed[i]
  FilterAbundance <- filter(DispAbundance, seed == as.numeric(FilterSeed))
  for (j in 1:5) {
    nseed1 <- as.numeric(FilterSeed)+j
    for (k in 1:55) {
      nseed2 <-as.numeric(FilterSeed)+k
      if (k != j) {
        x <- filter(FilterAbundance, nseed == nseed1| nseed == nseed2)%>% pivot_wider(names_from = spec, values_from = pop)
        y <- within(x, rm(nseed, seed, frag, disp, inter, intra, imm))
        #DisSim<-append(DisSim,1-vegdist(y, method = "bray", binary=FALSE),after = max(length(DisSim))) #Sorensen method
        DisSim<-append(DisSim,1-vegdist(y, method = "jaccard"),after = max(length(DisSim))) #Jaccard method
        Percov<- append(Percov, x$frag[2],after = max(length(Percov)))
        TestSeed<- append(TestSeed, x$seed[1],after = max(length(TestSeed)))
        TestnSeed1<- append(TestnSeed1, x$nseed[1],after = max(length(TestnSeed1)))
        TestnSeed2<- append(TestnSeed2, x$nseed[2],after = max(length(TestnSeed2)))
        Disp<- append(Disp, FilterAbundance$disp[1], after = max(length(Disp)))
        Imm<- append(Imm, FilterAbundance$imm[1], after = max(length(Imm)))
        Intra<- append(Intra, FilterAbundance$intra[1], after = max(length(Intra)))
        Inter<- append(Inter, FilterAbundance$inter[1], after = max(length(Inter)))
      } else{ 
        x <- filter(FilterAbundance, nseed == nseed1| nseed == nseed2)%>% pivot_wider(names_from = spec, values_from = pop)
        DisSim<-append(DisSim,1 ,after = max(length(DisSim)))
        Percov<- append(Percov, x$frag,after = max(length(Percov)))
        TestSeed<- append(TestSeed, x$seed,after = max(length(TestSeed)))
        TestnSeed1<- append(TestnSeed1, x$nseed,after = max(length(TestnSeed1)))
        TestnSeed2<- append(TestnSeed2, x$nseed,after = max(length(TestnSeed2)))
        Disp<- append(Disp, FilterAbundance$disp[1], after = max(length(Disp)))
        Imm<- append(Imm, FilterAbundance$imm[1], after = max(length(Imm)))
        Intra<- append(Intra, FilterAbundance$intra[1], after = max(length(Intra)))
        Inter<- append(Inter, FilterAbundance$inter[1], after = max(length(Inter)))
      }
    }
  }
}

## Run using Jaccard ##
DispAbundance<-filter(AbundancePopSpec, inter == 0 & disp == 0 & imm != 1.6529e-05  & imm != 0.002 )
#rest vectors and run loop
Data<-data_frame(TestnSeed1,TestnSeed2, Percov, DisSim, Disp, Imm, Intra, Inter)

#Break points#
Break_Imm <-vector()
Break_Disp<-vector()
Break_Inter<-vector()
Break_Intra<-vector()
Break<-vector()
StdErr<-vector()

fit<-lm(DisSim~Percov, Data[which(Data$Intra==0 & Imm == 0),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Zero" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Zero" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Zero" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "Zero", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

fit<-lm(DisSim~Percov, Data[which(Data$Intra==0 & Imm == 1.6529e-04),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Low" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Zero" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Zero" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "Zero", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

fit<-lm(DisSim~Percov, Data[which(Data$Intra==-0.05 & Imm == 0),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Zero" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Zero" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Zero" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "Low", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

fit<-lm(DisSim~Percov, Data[which(Data$Intra==-0.05 & Imm == 1.6529e-04),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Low" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Zero" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Zero" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "Low", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

fit<-lm(DisSim~Percov, Data[which(Data$Intra==-0.5 & Imm == 0),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Zero" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Zero" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Zero" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "High", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

fit<-lm(DisSim~Percov, Data[which(Data$Intra==-0.5 & Imm == 1.6529e-04),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Low" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Zero" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Zero" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "High", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

#inter 1
DispAbundance<-filter(AbundancePopSpec, inter == 1 & disp == 0 & imm != 1.6529e-05 & imm != 0.002)
#rest vectors and run loop
Data<-data_frame(TestnSeed1,TestnSeed2, Percov, DisSim, Disp, Imm, Intra, Inter)
## Break Point ##
fit<-lm(DisSim~Percov, Data[which(Data$Intra==0 & Imm == 0),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Zero" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Zero" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Low" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "Zero", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

fit<-lm(DisSim~Percov, Data[which(Data$Intra==0 & Imm == 1.6529e-04),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Low" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Zero" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Low" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "Zero", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

fit<-lm(DisSim~Percov, Data[which(Data$Intra==-0.05 & Imm == 0),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Zero" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Zero" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Low" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "Low", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

fit<-lm(DisSim~Percov, Data[which(Data$Intra==-0.05 & Imm == 1.6529e-04),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Low" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Zero" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Low" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "Low", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

fit<-lm(DisSim~Percov, Data[which(Data$Intra==-0.5 & Imm == 0),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Zero" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Zero" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Low" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "High", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

fit<-lm(DisSim~Percov, Data[which(Data$Intra==-0.5 & Imm == 1.6529e-04),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Low" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Zero" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Low" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "High", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

#Disp 1 different interaction
#inter 0
DispAbundance<-filter(AbundancePopSpec, inter == 0 & disp == 1 & imm != 1.6529e-05 & imm != 0.002 )
#rest vectors and run loop
Data<-data_frame(TestnSeed1,TestnSeed2, Percov, DisSim, Disp, Imm, Intra, Inter)
#Break Point## 
fit<-lm(DisSim~Percov, Data[which(Data$Intra==0 & Imm == 0),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Zero" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Low" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Zero" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "Zero", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

fit<-lm(DisSim~Percov, Data[which(Data$Intra==0 & Imm == 1.6529e-04),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Low" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Low" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Zero" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "Zero", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

fit<-lm(DisSim~Percov, Data[which(Data$Intra==-0.05 & Imm == 0),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)


Break_Imm <- append(Break_Imm,"Zero" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Low" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Zero" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "Low", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

fit<-lm(DisSim~Percov, Data[which(Data$Intra==-0.05 & Imm == 1.6529e-04),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Low" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Low" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Zero" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "Low", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

fit<-lm(DisSim~Percov, Data[which(Data$Intra==-0.5 & Imm == 0),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Zero" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Low" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Zero" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "High", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

fit<-lm(DisSim~Percov, Data[which(Data$Intra==-0.5 & Imm == 1.6529e-04),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Low" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Low" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Zero" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "High", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

#inter 1
DispAbundance<-filter(AbundancePopSpec, inter == 1 & disp == 1 & imm != 1.6529e-05 & imm != 0.002  )
#rest vectors and run loop
Data<-data_frame(TestnSeed1,TestnSeed2, Percov, DisSim, Disp, Imm, Intra, Inter)

## Break Point ##
fit<-lm(DisSim~Percov, Data[which(Data$Intra==0 & Imm == 0),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Zero" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Low" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Low" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "Zero", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

fit<-lm(DisSim~Percov, Data[which(Data$Intra==0 & Imm == 1.6529e-04),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Low" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Low" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Low" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "Zero", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

fit<-lm(DisSim~Percov, Data[which(Data$Intra==-0.05 & Imm == 0),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Zero" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Low" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Low" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "Low", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

fit<-lm(DisSim~Percov, Data[which(Data$Intra==-0.05 & Imm == 1.6529e-04),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Low" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Low" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Low" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "Low", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

fit<-lm(DisSim~Percov, Data[which(Data$Intra==-0.5 & Imm == 0),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Zero" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Low" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Low" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "High", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))

fit<-lm(DisSim~Percov, Data[which(Data$Intra==-0.5 & Imm == 1.6529e-04),])
segmented.fit<-segmented(fit, seg.Z = ~Percov, psi = 0.5)
t<-data.frame(segmented.fit$psi)

Break_Imm <- append(Break_Imm,"Low" ,after = max(length(Break_Imm)))
Break_Disp <- append(Break_Disp,"Low" ,after = max(length(Break_Disp)))
Break_Inter <-append(Break_Inter,"Low" ,after = max(length(Break_Inter)))
Break_Intra<- append(Break_Intra, "High", after = max(length(Break_Intra)))
Break <- append(Break,t$Est. ,after = max(length(Break)))
StdErr <- append(StdErr,t$St.Err ,after = max(length(StdErr)))
Method <- rep("Jaccard", length(Break_Imm))

Jac_Intra_Break<-data.frame(Break_Imm,
                            Break_Disp,
                            Break_Inter,
                            Break_Intra,
                            Break,
                            StdErr,
                            Method)
## Repeat with Sorensen ##

Method <- rep("Sorensen", length(Break_Imm))

Sor_Intra_Break<-data.frame(Break_Imm,
                            Break_Disp,
                            Break_Inter,
                            Break_Intra,
                            Break,
                            StdErr,
                            Method)
CTdata <- rbind(Jac_Intra_Break, Sor_Intra_Break)

#lit<-c(From log book)
liturature <-c(0.3, 0.4, 0.3, 0.41, 0.51, 0.44, 0.88, 0.1, 0.3, 0.19,0.35, 0.25, 0.6)

litrev<- data.frame(Value = c(0.3, 0.4, 0.3, 0.41, 0.51, 0.44, 0.88, 0.1, 0.3, 0.19,0.35, 0.25, 0.6, 0.59),
                   Method= c("Liturature", "Liturature","Liturature","Liturature","Liturature","Liturature","Liturature","Liturature","Liturature","Liturature","Liturature","Liturature","Liturature", "Percolation Threshold"))



litrevCT<-ggplot(litrev, aes(x = 1:nrow(litrev), y = Value, col= factor(Method))) +  # Apply nrow function
  geom_point(size = 1)+
  ylab("") +
  xlab("")+
  theme_classic2()+
  scale_color_manual(values=c( "orange", "black"))+
  theme(plot.title=element_text(size=9), axis.title=element_text(size=9), plot.tag = element_text(size =9),
        legend.text = element_text(size=7), legend.title = element_text(size=7),
        axis.text.x=element_blank())+
  theme(legend.position = 'bottom')+
  labs(color='Colation Method ',tag = "D", title = "Liturature Review Critical Thresholds")


CTNoIntra<-ggplot(filter(CTdata, Break_Intra == "Zero"), aes(x=Break_Disp ,y=Break,color = factor(Break_Imm):factor(Method)))+ 
  geom_point(aes(shape = factor(Break_Inter)),
             position=position_jitter(width = 0.2, seed = 123))+
  geom_linerange(aes(ymin=Break-StdErr, ymax=Break+StdErr),
                 position = position_jitter(width = 0.2, seed = 123))+
  geom_hline(yintercept = c(0.59), linetype = "dotted", color= "black")+   
  geom_hline(yintercept = c(median(liturature)), linetype = "dashed", color= "orange")+
  xlab("Dispersal Area")+
  ylab("Critical Threshold")+
  theme_classic2()+
  theme(legend.position= "none",plot.title=element_text(size=9), axis.title=element_text(size=9), plot.tag = element_text(size =9),
        legend.text = element_text(size=7), legend.title = element_text(size=7))+
  scale_y_continuous(limits = c(0, 0.6), expand = expansion(mult = c(0, 0.1)))+
  scale_color_manual(values=c( "firebrick", "royalblue4", "olivedrab3", "maroon3"), labels= c("Low Jaccard","Low Sorensen", "Zero Jaccard", "Zero Sorensen")) +
  scale_x_discrete(limits=c("Zero","Low"))+
  scale_shape_discrete(name  ="Neighbourhood Area")+
  labs(color='Immigration Rate',tag = "A", title = "No Intraspecific Competition")

CTLowIntra<-ggplot(filter(CTdata, Break_Intra == "Low"), aes(x=Break_Disp ,y=Break,color = factor(Break_Imm):factor(Method)))+ 
  geom_point(aes(shape = factor(Break_Inter)),
             position=position_jitter(width = 0.2, seed = 123))+
  geom_linerange(aes(ymin=Break-StdErr, ymax=Break+StdErr),
                 position = position_jitter(width = 0.2, seed = 123))+
  geom_hline(yintercept = c(0.59), linetype = "dotted", color= "black")+   
  geom_hline(yintercept = c(mean(liturature)), linetype = "dashed", color= "orange")+
  xlab("Dispersal Area")+
  ylab("")+
  theme_classic2()+
  theme(legend.position= "none", plot.title=element_text(size=9), axis.title=element_text(size=9), plot.tag = element_text(size =9),
        legend.text = element_text(size=7), legend.title = element_text(size=7))+
  scale_y_continuous(limits = c(0, 0.6), expand = expansion(mult = c(0, 0.1)))+
  scale_color_manual(values=c( "firebrick", "royalblue4", "olivedrab3", "maroon3"), labels= c("Low Jaccard","Low Sorensen", "Zero Jaccard", "Zero Sorensen")) +
  scale_x_discrete(limits=c("Zero","Low"))+
  scale_shape_discrete(name  ="Neighbourhood Area")+
  labs(color='Immigration Rate',tag = "B", title = "Low Intraspecific Competition")


CTHighIntra<-ggplot(filter(CTdata, Break_Intra == "High"), aes(x=Break_Disp ,y=Break,color = factor(Break_Imm):factor(Method)))+ 
  geom_point(aes(shape = factor(Break_Inter)),
             position=position_jitter(width = 0.2, seed = 123))+
  geom_linerange(aes(ymin=Break-StdErr, ymax=Break+StdErr),
                 position = position_jitter(width = 0.2, seed = 123))+
  geom_hline(yintercept = c(0.59), linetype = "dotted", color= "black")+   
  geom_hline(yintercept = c(mean(liturature)), linetype = "dashed", color= "orange")+
  xlab("Dispersal Area")+
  ylab("")+
  theme_classic2()+
  theme(legend.position = 'bottom', legend.box = "vertical",
        plot.title=element_text(size=9), axis.title=element_text(size=9), plot.tag = element_text(size =9),
        legend.text = element_text(size=7), legend.title = element_text(size=7))+
  scale_y_continuous(limits = c(0, 0.6), expand = expansion(mult = c(0, 0.1)))+
  scale_color_manual(values=c( "firebrick", "royalblue4", "olivedrab3", "maroon3"), labels= c("Low Jaccard","Low Sorensen", "Zero Jaccard", "Zero Sorensen")) +
  scale_x_discrete(limits=c("Zero","Low"))+
  scale_shape_discrete(name  ="Neighbourhood Area")+
  labs(color='Immigration Rate',tag = "C", title = "High Intraspecific Competition")
#### Plot combinations ####

#Dispersal
plot_path<-("../Plots/")
Combine_Disp<-ggarrange(DispSppRich0Imm, JacDispSim0Imm, SorDispSim0Imm, SVG0Imm, DispSppRichImm, JacDispSimImm, SorDispSimImm, SVGImm,
                        ncol = 4,
                        nrow = 2,
                        common.legend = TRUE,
                        legend = "bottom")
ggsave(filename = file.path(paste0(plot_path,"finale_Combine_Disp.pdf")), width = 27, height = 15, units = "cm")

#Neighbourhood
plot_path<-("../Plots/")
Combine_Nighbour<-ggarrange(NeighSppRich0Disp, NeighSim0Disp, NeighSppRichDisp, NeighSimDisp,
                        ncol = 2,
                        nrow = 2,
                        common.legend = TRUE,
                        legend = "bottom")
ggsave(filename = file.path(paste0(plot_path,"final_Combine_Neighbour.pdf")), width = 15, height = 15, units = "cm")

#Intraspecific competition
plot_path<-("../Plots/")

Combine_IntraSpecific<-ggarrange(IntaSppRich0Neigh, IntaSim0Neigh, IntaSppRichNeigh, IntaSimNeigh,
                            ncol = 2,
                            nrow = 2,
                            common.legend = TRUE,
                            legend = "bottom")
ggsave(filename = file.path(paste0(plot_path,"final_Combine_IntraSpecific.pdf")), width = 15, height = 15, units = "cm")


#critical threshold
plot_path<-("../Plots/")
Combine_CT<-ggarrange(CTNoIntra,CTLowIntra,CTHighIntra,#litrevCT,
                            ncol = 3,
                            nrow = 1,
                      widths = c(0.8,0.8,0.8,1),
                      common.legend = T,
                            legend = "bottom")
full_Combine_CT<-ggarrange(Combine_CT,litrevCT,
                      ncol = 2,
                      nrow = 1,
                      widths = c(0.9,0.5),
                      #common.legend = T,
                      legend = "bottom")

ggsave(filename = file.path(paste0(plot_path,"testres_final_Combine_CT.pdf")), width = 27, height = 15, units = "cm",res=300)
