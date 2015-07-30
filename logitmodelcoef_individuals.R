rm(list=ls())
require(plyr)
require(tidyr)

source("data_cleanup.R")

DCE_data2 <- data.frame(cbind(names=as.character(stacked3$name),answer=stacked3$answer,rep_scale=stacked3$rep_scale,choice_set=stacked3$choice_set,pg=stacked3$pg))
DCE_data2 <- spread(DCE_data2,rep_scale,answer)
names(DCE_data2)=c('names','DCE_pg','i','DCE_least','DCE_most')
DCE_data2$i <- as.numeric(DCE_data2$i)
DCE_data2$DCE_least <- as.numeric(DCE_data2$DCE_least)
DCE_data2$DCE_most <- as.numeric(DCE_data2$DCE_most)

###### make stacked layout ####
card_key <- read.csv("card_key.csv") 
stacked=cbind(DCE_data2,
              FoodProvision=NA,
              AboriginalNeeds=NA,
              NaturalProducts=NA,
              CarbonStorage=NA,
              CoastalProtection=NA,
              CoastalLivelihoods=NA,
              TourismRecreation=NA,
              IconicPlacesSPecies=NA,
              CleanWaters=NA,
              Biodiversity=NA)
#stacked[is.na(stacked)]=.
for(i in 1:10){
  stacked[stacked$DCE_pg==i,5+card_key[card_key$card==i,2]]=0
}
for(j in 1:dim(stacked)[1]){
  stacked[j,5+stacked$DCE_least[j]]=-1
  stacked[j,5+stacked$DCE_most[j]]=1
}

library(BiasedUrn)
library(foreign)
library(mlogit)
source("functions.R")

#itData = read.spss("http://surveyanalysis.org/images/0/06/ItMaxDiff.sav", use.value.labels = FALSE, to.data.frame = TRUE)
# Selecting the variables containing the max-diff data
#z = itData[,-1:-5]
z=data.frame(matrix(1,ncol=100,nrow=nrow(raw)))
id=unique(stacked$names)
for(i in 1:length(id)){
  for(j in 6:15){
    z[i,0:9*10+j-5]=as.vector(stacked[stacked$names==id[i],j])
  }
}
head(z)


#z=t(stacked[,6:15])
# stacking the data (one set per row)
alternativeNames = c("FoodProvision",
                     "AboriginalNeeds",
                     "NaturalProducts",
                     "CarbonStorage",
                     "CoastalProtection",
                     "CoastalLivelihoods",
                     "TourismRecreation",
                     "IconicPlacesSPecies",
                     "CleanWaters",
                     "Biodiversity")
nAlternatives = length(alternativeNames)
nBlocks = ncol(z) / nAlternatives
nAltsPerSet = 5
n = nrow(z)
nObservations = n * nBlocks 
itMaxDiffData = matrix(as.numeric(t(z)),ncol = nAlternatives,byrow = TRUE, dimnames = list(1:nObservations, alternativeNames))
head(itMaxDiffData)
#########################################################################################################################

counts = apply(itMaxDiffData, 2, mean, na.rm = TRUE)
ranks = nAlternatives + 1 - rank(counts)
cbind(Counts = counts, Ranks = ranks)

#########################################################################################################################
id = rep(1:n,rep(nBlocks,n))
individualCounts = aggregate(itMaxDiffData,list(id),mean, na.rm = TRUE)[,-1]
round(individualCounts[1:10,],1) #show at data for first 10 respondents
#########################################################################################################################
set.seed(0) # setting the random number seed to enhance comparability
indidualCountsNoTies = individualCounts + matrix(runif(n * nAlternatives)/100000, n) #adding random numbers to break ties
ranks = nAlternatives + 1 - apply(indidualCountsNoTies,1,rank) #ranks
### ranks[9,2]=9 needed this because small sample, missing values
rankProportions = t(apply(ranks,1,function(x) table(factor(x,levels=1:nBlocks))) / n * 100)

round(rankProportions,1)

rankCumProportions = t(apply(rankProportions,1,cumsum))
round(rankCumProportions,1)

aveRank = rankProportions %*% (1:10)/100
write.csv(aveRank,"aveRank.csv")

cbind(aveRank, Rank = rank(aveRank))
#########################################################################################################################
nRows = sum(is.na(itMaxDiffData)) * 2
longData = matrix(0, nRows,nAlternatives + 3)
counter = 0
setCounter = 0
for (rr in 1:nObservations){
  nAlts = 0
  alternatives = NULL
  respondent = floor((rr-1)/nBlocks) + 1
  for (cc in 1:nAlternatives){
    v = itMaxDiffData[rr,cc]
    if (!is.na(v)){
      nAlts = nAlts + 1
      alternatives[nAlts] = cc
      if (v == 1)
        best = cc
      if (v == -1)
        worst = cc
    }
  }
  setCounter = setCounter + 1
  for (a in 1:nAlts){
    counter = counter + 1
    this_a = alternatives[a]
    if (this_a == best)
      longData[counter,3] = 1
    else if (this_a == worst)
      longData[counter + nAlts,3] = 1
    longData[counter, 1] = respondent 
    longData[counter + nAlts,1] = respondent 
    longData[counter, 2] = setCounter 
    longData[counter + nAlts, 2] = setCounter + 1
    longData[counter,3 + this_a] = 1
    longData[counter + nAlts,3 + this_a] = -1
  }
  setCounter = setCounter + 1
  counter = counter + nAlts
}
longData[1:20,]
longData = as.data.frame(longData)
names(longData) = c("ID","Set","Choice",alternativeNames)
#write.csv(longData,"longData.csv")
#longData2=longData[c(1:5761),]
#########################################################################################################################
#logitModel = mlogit(Choice ~ Microsoft+IBM+Google+Intel+HewlettPackard+Sony+Dell+Yahoo+Nokia | 0, data = longData, alt.levels = paste(1:nAltsPerSet), shape = "long")
#summary(logitModel)
#longData$Choice=as.logical(longData$Choice)
lmc <- individualCounts
lmc[] <- 0
for(i in seq_along(unique(longData$ID))){
  logitModel = mlogit(Choice ~ FoodProvision+
                        #AboriginalNeeds+
                        NaturalProducts+
                        CarbonStorage+
                        CoastalProtection+
                        CoastalLivelihoods+
                        TourismRecreation+
                        IconicPlacesSPecies+
                        CleanWaters+
                        Biodiversity| 0, data = longData[longData$ID==i,], alt.levels = paste(1:nAltsPerSet), shape = "long")
  summary(logitModel)
  lmc[i,] <- as.numeric(c(logitModel$coefficients[1],0,logitModel$coefficients[2:9]))
  print(i)
}

lmc2 <- lmc+1+abs(apply(lmc,1,min))
lmc3 <- lmc2/apply(lmc2,1,sum)
