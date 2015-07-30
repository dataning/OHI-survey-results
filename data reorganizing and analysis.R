rm(list=ls())

userstats_BC_subsample <- read.csv("userstats_BC_subsample_usable.csv")

require(plyr)
userstats_BC_subsample$sample <- revalue(userstats_BC_subsample$sample,c("BC"="BC subsample"))

userstats_english <- read.csv("userstats_english_usable.csv")
userstats_French <- read.csv("userstats_French_usable.csv")

#all_data=rbind(userstats_BC_subsample,userstats_english,userstats_French)
all_data=rbind(userstats_english,userstats_French)

####### data for Wolfgang #########
all_data2 <- userstats_BC_subsample
all_data2[is.na(all_data2)] <- ""
all_data2$DCEclick_1 <- revalue(all_data2$DCEclick_1,c("checked"="1"))
all_data2$DCEclick_2 <- revalue(all_data2$DCEclick_2,c("checked"="1"))
all_data2$DCEclick_3 <- revalue(all_data2$DCEclick_3,c("checked"="1"))
all_data2$DCEclick_4 <- revalue(all_data2$DCEclick_4,c("checked"="1"))
all_data2$DCEclick_5 <- revalue(all_data2$DCEclick_5,c("checked"="1"))
all_data2$DCEclick_6 <- revalue(all_data2$DCEclick_6,c("checked"="1"))
all_data2$DCEclick_7 <- revalue(all_data2$DCEclick_7,c("checked"="1"))
all_data2$DCEclick_8 <- revalue(all_data2$DCEclick_8,c("checked"="1"))
all_data2$DCEclick_9 <- revalue(all_data2$DCEclick_9,c("checked"="1"))
all_data2$DCEclick_10 <- revalue(all_data2$DCEclick_10,c("checked"="1"))
sum(as.numeric(all_data2=="checked"),na.rm=T)

unique(all_data2$ocean_days)
all_data2$ocean_days <- revalue(all_data2$ocean_days,c("lessthan5"="3"))
all_data2$ocean_days <- revalue(all_data2$ocean_days,c("5to9"="7"))
all_data2$ocean_days <- revalue(all_data2$ocean_days,c("10to29"="20"))
all_data2$ocean_days <- revalue(all_data2$ocean_days,c("30to49"="40"))
all_data2$ocean_days <- revalue(all_data2$ocean_days,c("50to100"="75"))
all_data2$ocean_days <- revalue(all_data2$ocean_days,c("morethan100"="150"))
all_data2$ocean_days <- revalue(all_data2$ocean_days,c("dontknow"=""))
all_data2$ocean_days <- revalue(all_data2$ocean_days,c("none"="0"))
unique(all_data2$ocean_days)

unique(all_data2$seafood_eat)
all_data2$seafood_eat <- revalue(all_data2$seafood_eat,c("Never"="0"))
all_data2$seafood_eat <- revalue(all_data2$seafood_eat,c("atleast1wk"="52"))
all_data2$seafood_eat <- revalue(all_data2$seafood_eat,c("3month"="36"))
all_data2$seafood_eat <- revalue(all_data2$seafood_eat,c("1to2month"="16"))
all_data2$seafood_eat <- revalue(all_data2$seafood_eat,c("7to11yr"="9"))
all_data2$seafood_eat <- revalue(all_data2$seafood_eat,c("3to6yr"="4.5"))
all_data2$seafood_eat <- revalue(all_data2$seafood_eat,c("1to2yr"="1.5"))
unique(all_data2$seafood_eat)

unique(all_data2$demotwo_livedCanada)
all_data2$demotwo_livedCanada <- revalue(all_data2$demotwo_livedCanada,c("2to5"="3.5"))
all_data2$demotwo_livedCanada <- revalue(all_data2$demotwo_livedCanada,c("6to10"="8"))
all_data2$demotwo_livedCanada <- revalue(all_data2$demotwo_livedCanada,c("11to20"="16"))
all_data2$demotwo_livedCanada <- revalue(all_data2$demotwo_livedCanada,c("morethan20"="32"))
unique(all_data2$demotwo_livedCanada)

unique(all_data2$demoone_education)
all_data2$demoone_education <- revalue(all_data2$demoone_education,c("elementary"="1"))
all_data2$demoone_education <- revalue(all_data2$demoone_education,c("HS"="2"))
all_data2$demoone_education <- revalue(all_data2$demoone_education,c("somepost"="3"))
all_data2$demoone_education <- revalue(all_data2$demoone_education,c("college"="4"))
all_data2$demoone_education <- revalue(all_data2$demoone_education,c("bachelors"="5"))
all_data2$demoone_education <- revalue(all_data2$demoone_education,c("graduate"="6"))
unique(all_data2$demoone_education)

unique(all_data2$demothree_age)
all_data2$demothree_age <- revalue(all_data2$demothree_age,c("20to24"="22"))
all_data2$demothree_age <- revalue(all_data2$demothree_age,c("25to34"="30"))
all_data2$demothree_age <- revalue(all_data2$demothree_age,c("35to44"="40"))
all_data2$demothree_age <- revalue(all_data2$demothree_age,c("45to54"="50"))
all_data2$demothree_age <- revalue(all_data2$demothree_age,c("55to64"="60"))
all_data2$demothree_age <- revalue(all_data2$demothree_age,c("65plus"="70"))
unique(all_data2$demothree_age)

unique(all_data2$demoone_gender)
all_data2$demoone_gender <- revalue(all_data2$demoone_gender,c("female"="2"))
all_data2$demoone_gender <- revalue(all_data2$demoone_gender,c("male"="1"))
all_data2$demoone_gender <- revalue(all_data2$demoone_gender,c("noresponse"="0"))
unique(all_data2$demoone_gender)

unique(all_data2$political_party)
all_data2$political_party <- revalue(all_data2$political_party,c("dontknow"="0"))
all_data2$political_party <- revalue(all_data2$political_party,c("noresponse"="1"))
all_data2$political_party <- revalue(all_data2$political_party,c("noparty"="2"))
all_data2$political_party <- revalue(all_data2$political_party,c("Bloc"="3"))
all_data2$political_party <- revalue(all_data2$political_party,c("NDP"="4"))
all_data2$political_party <- revalue(all_data2$political_party,c("green"="5"))
all_data2$political_party <- revalue(all_data2$political_party,c("liberal"="6"))
all_data2$political_party <- revalue(all_data2$political_party,c("CPC"="7"))
unique(all_data2$political_party)

for(i in 1:110){
  names(all_data2)[i] <- strtrim(names(all_data2)[i],8)
}


##########################

## isolate and combine data sources
i=which(names(userstats_BC_subsample)=="DCE_pg1"|names(userstats_BC_subsample)=="DCE_most10")

DCE_data=rbind(#userstats_BC_subsample[userstats_BC_subsample$completed=="y",i[1]:i[2]],
               userstats_english[userstats_english$completed=="y",i[1]:i[2]],
               userstats_French[userstats_French$completed=="y",i[1]:i[2]]
)
DCE_data$names = c(#as.character(userstats_BC_subsample$name[userstats_BC_subsample$completed=="y"]),
                   as.character(userstats_english$name[userstats_english$completed=="y"]),
                   as.character(userstats_French$name[userstats_French$completed=="y"])
)
row.names(DCE_data)=1:length(DCE_data$names)

# filter data
# remove bc subsample
DCE_data=DCE_data[all_data$sample!="BC subsample",]
all_data=all_data[all_data$sample!="BC subsample",]

# filter by region
# unique(all_data$sample)
# Prov="All"
#DCE_data=DCE_data[all_data$sample==Prov,]
#all_data=all_data[all_data$sample==Prov,]

# # filter by age ( 65plus 55to64 35to44 45to54 25to34        20to24)
# unique(all_data$demothree_age)
# Prov="20to24"
# DCE_data=DCE_data[all_data$demothree_age==Prov,]
# all_data=all_data[all_data$demothree_age==Prov,]

# find which goals are in each set
for(i in 1:10){
  if(i==1) {
    DCE_data2=cbind(names=DCE_data$names,i,DCE_data[,1:3])
    names(DCE_data2)=c('names','i','DCE_pg','DCE_least','DCE_most')
  }else{
    temp=DCE_data[,(((i-1)*3)+(1:3))]
    names(temp)=c('DCE_pg','DCE_least','DCE_most')
    DCE_data2=rbind(DCE_data2,cbind(names=DCE_data$names,i,temp))
  }   
}

for(i in 1:10){
  DCE_options=unique(c(unique(DCE_data2[DCE_data2$DCE_pg==i,4]),unique(DCE_data2[DCE_data2$DCE_pg==i,5])))
  #DCE_options=unique(c(unique(DCE_data2[DCE_data2$i==i,4]),unique(DCE_data2[DCE_data2$i==i,5])))
  print(DCE_options)
}


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
z=data.frame(matrix(1,ncol=100,nrow=nrow(DCE_data)))
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
#write.csv(longData,"longData.csv")
#longData2=longData[c(1:5761),]
#########################################################################################################################
#logitModel = mlogit(Choice ~ Microsoft+IBM+Google+Intel+HewlettPackard+Sony+Dell+Yahoo+Nokia | 0, data = longData, alt.levels = paste(1:nAltsPerSet), shape = "long")
#summary(logitModel)
#longData$Choice=as.logical(longData$Choice)
logitModel = mlogit(Choice ~ FoodProvision+
                    #AboriginalNeeds+
                    NaturalProducts+
                    CarbonStorage+
                    CoastalProtection+
                    CoastalLivelihoods+
                    TourismRecreation+
                    IconicPlacesSPecies+
                    CleanWaters+
                    Biodiversity| 0, data = longData, alt.levels = paste(1:nAltsPerSet), shape = "long")
summary(logitModel)
#########################################################################################################################
#########################################################################################################################
lmc=as.numeric(c(logitModel$coefficients[1],0,logitModel$coefficients[2:9]))
weights=10-aveRank

par(mfrow=c(1,2))

plot(weights,lmc,,ylab="Logit Model Coefficient",xlab="10 minus Average Rank");title(Prov)
fit=lm(lmc~weights)
abline(lm(lmc~weights))
text(3.65,4,paste("lmc = ",round(fit$coefficients[2],3)," * weight + ",round(fit$coefficients[1],3)))
text(3.65,3.5,paste("R^2= ",round(summary(fit)$adj.r.squared,3)))

############# importance ################
importance <- c(FoodProvision=mean(all_data$foodprovision_general,na.rm=T),
                AboriginalNeeds=mean(all_data$aboriginalneeds_general,na.rm=T),
                NaturalProducts=mean(all_data$nonfood_general,na.rm=T),
                CarbonStorage=mean(all_data$carbonstorage_general,na.rm=T),
                CoastalProtection=mean(all_data$coastalprotection_general,na.rm=T),
                CoastalLivelihoods=mean(all_data$coastallivelihood_general,na.rm=T),
                TourismRecreation=mean(all_data$tourismrec_general,na.rm=T),
                IconicPlacesSPecies=mean(all_data$icons_general,na.rm=T),
                CleanWaters=mean(all_data$cleanwater_general,na.rm=T),
                Biodiversity=mean(all_data$biodiversity_general,na.rm=T)
                )

plot(importance,lmc,,ylab="Logit Model Coefficient",xlab="Average Importance");title(Prov)
fit=lm(lmc~importance)
abline(lm(lmc~importance))
text(3.75,4,paste("lmc = ",round(fit$coefficients[2],3)," * importance + ",round(fit$coefficients[1],3)))
text(3.75,3.5,paste("R^2= ",round(summary(fit)$adj.r.squared,3)))

Prov
summary(logitModel)
cbind(aveRank, Rank = rank(aveRank))
importance


weights <- as.data.frame(cbind(importance,BWrank=as.numeric(aveRank),lmc))

weights$importance <- weights$importance/sum(weights$importance)*10
weights$BWrank <- (10-weights$BWrank)/sum(weights$BWrank)*10
weights$lmc1 <- weights$lmc+1
weights$lmc10 <- weights$lmc+10
weights$lmc <- weights$lmc/sum(weights$lmc)*10
weights$lmc1 <- weights$lmc1/sum(weights$lmc1)*10
weights$lmc10 <- weights$lmc10/sum(weights$lmc10)*10
weights$equal <- 1


write.csv(weights,"weights.csv")
write.csv(weights,paste0("weights_",Prov,".csv"))

