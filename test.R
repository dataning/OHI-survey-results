library(BiasedUrn)
library(foreign)
library(mlogit)
source("~/Documents/R/OHI survey results/functions.R")


#itData = read.spss("http://surveyanalysis.org/images/0/06/ItMaxDiff.sav", use.value.labels = FALSE, to.data.frame = TRUE)
#write.csv(itData,"~/Documents/R/OHI survey results/itData.csv")
itData=read.csv("~/Documents/R/OHI survey results/itData.csv")
# Selecting the variables containing the max-diff data
z = itData[,-1:-6]
# stacking the data (one set per row)
alternativeNames = c("Apple","Microsoft","IBM","Google","Intel","HewlettPackard","Sony","Dell","Yahoo","Nokia")
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
rankProportions = t(apply(ranks,1,table) / n * 100)
round(rankProportions,1)

rankCumProportions = t(apply(rankProportions,1,cumsum))
round(rankCumProportions,1)

aveRank = rankProportions %*% (1:10)/100
cbind(aveRank, Rank = rank(aveRank))
#########################################################################################################################
nRows = sum(is.na(itMaxDiffData)) * 2
longData = matrix(0, nRows,nAlternatives + 3)
counter = 0
setCounter = 0
for (rr in 1:nObservations){
  nAlts = 0
  alternatives = NULL
  respondent = floor(rr/nBlocks) + 1
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
#########################################################################################################################
logitModel = mlogit(Choice ~ Microsoft+IBM+Google+Intel+HewlettPackard+Sony+Dell+Yahoo+Nokia | 0, data = longData, alt.levels = paste(1:nAltsPerSet), shape = "long")
summary(logitModel)
#########################################################################################################################
#########################################################################################################################
