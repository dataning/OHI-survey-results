require(dplyr)
require(plyr)
#load files
raw <- read.csv("~/R/OHI survey results/Latent Gold/userstats_bothlang_usable.csv")
stacked <- read.csv("~/R/OHI survey results/Latent Gold/choice.csv")
#join files
stacked2 <- left_join(stacked,raw,by="name")
#### calculate ocean distance based on FSA ####
FSA_dist <- read.csv("~/R/OHI survey results/GIS/FSA_dist.txt")
stacked2$distance_FSA <- (left_join(stacked2,FSA_dist,by=c("demotwo_fsa"="CFSAUID")))$NEAR_DIST
stacked2$distance_FSA <- as.numeric(gsub(",","",stacked2$distance_FSA))
stacked2$distance_FSAc <- ">1000km"
stacked2$distance_FSAc[stacked2$distance_FSA<500000] <- "<500km"
stacked2$distance_FSAc[stacked2$distance_FSA<100000] <- "<100km"
hist(as.numeric(stacked2$distance_FSA),nclass=2000)


#### reorganize rec and jobs ####
rec_types=c("rec_swim","rec_boat","rec_surf","rec_scuba","rec_powerboat",
"rec_recfish","rec_sightsee","rec_none","rec_other","rec_specify2")
jobs_types=c("jobs_comfish","jobs_aquaculture","jobs_firstnations","jobs_tourism","jobs_transport",
"jobs_port","jobs_govt","jobs_none","jobs_other","jobs_specify2")

stacked2$rec_specify2 <- 1
stacked2$rec_specify2[stacked2$rec_specify==""] <- 0
stacked2$rec <- rowSums(stacked2[,(names(stacked2) %in% rec_types)],na.rm=T)

stacked2$jobs_specify2 <- 1
stacked2$jobs_specify2[stacked2$jobs_specify==""] <- 0
stacked2$jobsnum <- rowSums(x <- stacked2[,(names(stacked2) %in% jobs_types)],na.rm=T)
stacked2$jobs <- "none"
stacked2$jobs[stacked2$jobs_comfish==1] <- "comfish"
stacked2$jobs[stacked2$jobs_aquaculture==1] <- "aquaculture"
stacked2$jobs[stacked2$jobs_firstnations==1] <- "firstnations"
stacked2$jobs[stacked2$jobs_tourism==1] <- "tourism"
stacked2$jobs[stacked2$jobs_transport==1] <- "transport"
stacked2$jobs[stacked2$jobs_port==1] <- "port"
stacked2$jobs[stacked2$jobs_govt==1] <- "govt"
stacked2$jobs[stacked2$jobs_other==1] <- "other"
stacked2$jobs[stacked2$jobsnum>1] <- "many"

stacked2$jobs2 <- "none"
stacked2$jobs2[stacked2$jobs_comfish==1] <- "extract"
stacked2$jobs2[stacked2$jobs_aquaculture==1] <- "extract"
stacked2$jobs2[stacked2$jobs_firstnations==1] <- "conservation"
stacked2$jobs2[stacked2$jobs_tourism==1] <- "conservation"
stacked2$jobs2[stacked2$jobs_transport==1] <- "extract"
stacked2$jobs2[stacked2$jobs_port==1] <- "extract"
stacked2$jobs2[stacked2$jobs_govt==1] <- "conservation"
stacked2$jobs2[stacked2$jobs_other==1] <- "other"
stacked2$jobs2[stacked2$jobsnum>1] <- "many"

#### fix householdsize ####

stacked2$demothree_householdsize[stacked2$demothree_householdsize=="On"] <- 1
stacked2$demothree_householdsize[stacked2$demothree_householdsize=="One"] <- 1
stacked2$demothree_householdsize[stacked2$demothree_householdsize=="on"] <- 1
stacked2$demothree_householdsize[stacked2$demothree_householdsize=="one"] <- 1
stacked2$demothree_householdsize[stacked2$demothree_householdsize==""] <- 0
stacked2$demothree_householdsize[is.na(stacked2$demothree_householdsize)] <- 0
stacked2$demothree_householdsize <- as.numeric(stacked2$demothree_householdsize)

#### fix seafood_eat ####
stacked2$seafood_eat <- as.character(stacked2$seafood_eat)
stacked2$seafood_eat[stacked2$seafood_eat=="Never"] <- 0
stacked2$seafood_eat[stacked2$seafood_eat==""] <- 0
stacked2$seafood_eat[stacked2$seafood_eat=="1to2yr"] <- 1.5
stacked2$seafood_eat[stacked2$seafood_eat=="3to6yr"] <- 4.5
stacked2$seafood_eat[stacked2$seafood_eat=="7to11yr"] <- 9
stacked2$seafood_eat[stacked2$seafood_eat=="1to2month"] <- 18
stacked2$seafood_eat[stacked2$seafood_eat=="3month"] <- 36
stacked2$seafood_eat[stacked2$seafood_eat=="atleast1wk"] <- 52
stacked2$seafood_eat <- as.numeric(stacked2$seafood_eat)

#### numeric age class ####
stacked2$demothree_age_num <- as.character(stacked2$demothree_age)
stacked2$demothree_age_num[stacked2$demothree_age=="19"] <- 19
stacked2$demothree_age_num[stacked2$demothree_age=="20to24"] <- 22
stacked2$demothree_age_num[stacked2$demothree_age=="25to34"] <- 29.5
stacked2$demothree_age_num[stacked2$demothree_age=="35to44"] <- 39.5
stacked2$demothree_age_num[stacked2$demothree_age=="45to54"] <- 49.5
stacked2$demothree_age_num[stacked2$demothree_age=="55to64"] <- 59.5
stacked2$demothree_age_num[stacked2$demothree_age=="65plus"] <- 69.5
stacked2$demothree_age_num <- as.numeric(stacked2$demothree_age_num)

#### fix political party ####
stacked2$political_party2 <- as.character(stacked2$political_party)
stacked2$political_party2[stacked2$political_party=="Bloc"] <- "Other"
stacked2$political_party2[stacked2$political_party=="CPC"] <- "CPC"
stacked2$political_party2[stacked2$political_party=="green"] <- "NDP/Green"
stacked2$political_party2[stacked2$political_party=="liberal"] <- "Liberal"
stacked2$political_party2[stacked2$political_party=="NDP"] <- "NDP/Green"
stacked2$political_party2[stacked2$political_party=="noparty"] <- "Other"
stacked2$political_party2[stacked2$political_party=="dontknow"] <- "Other"
stacked2$political_party2[stacked2$political_party=="noresponse"] <- "Other"
stacked2$political_party2[stacked2$political_party==""] <- "Other"
unique(stacked2$political_party2)

#### deal with missing values ####
stacked2$envr_org[stacked2$envr_org==""] <- "dontknow"

#### remove useless variables ####
names(stacked2)
keeps <- c("id.x","name","pbw_cs","pbw_choice","pbw_choicenumeric","sweight","seq","task",
           "sample","ocean_days","seafood_eat","demoone_gender","demoone_education","demotwo_livedCanada",
           "envr_org","envr_protest","political_party","political_party2","demothree_income","demothree_householdsize",
           "demothree_age","demothree_age_num","distance_FSA","distance_FSAc","rec","jobs","jobs2")
stacked3 <-stacked2[,(names(stacked2) %in% keeps)]

write.csv(stacked3,"~/R/OHI survey results/Latent Gold/choice_cov.csv")





#### isolate best and worst answers ####
stacked3_best <-stacked3[stacked3$sweight==1,] 
stacked3_worst <- stacked3[stacked3$sweight==(-1),] 
write.csv(stacked3_best,"~/R/OHI survey results/Latent Gold/choice_cov_best.csv")
write.csv(stacked3_worst,"~/R/OHI survey results/Latent Gold/choice_cov_worst.csv")

#### age and province specific ####
for(a in 1:length(unique(stacked3$demothree_age))){
  stacked4 <- stacked3[stacked3$demothree_age==unique(stacked3$demothree_age)[a],]
  write.csv(stacked4,paste0("~/R/OHI survey results/Latent Gold/choice_cov_age_",unique(stacked3$demothree_age)[a],".csv"))
  
}

for(a in 1:length(unique(stacked3$sample))){
  stacked4 <- stacked3[stacked3$sample==unique(stacked3$sample)[a],]
  write.csv(stacked4,paste0("~/R/OHI survey results/Latent Gold/choice_cov_area_",unique(stacked3$sample)[a],".csv"))
}




#### calculate and insert LG weights in csv's ####
LGcoefficients <- read.csv("~/R/OHI survey results/Latent Gold/LGcoefficients.csv")
varia <- c('All','20to24','25to34','35to44','45to54','55to64','65plus','QC','PR','ON','MR','BC')
for(i in 1:length(varia)){
  weights<- read.csv(paste0("~/GitHub/ohi-canada/eezCHONE/rawdata.Canada-CHONe2014/weights/weights_",varia[i],".csv"))
  LG <-(LGcoefficients[1+i]+0.1)/sum((LGcoefficients[1+i]+0.1))*10;weights$LG <- LG[,1]
  LG1 <- (LGcoefficients[1+i]+1)/sum((LGcoefficients[1+i]+1))*10;weights$LG1 <- LG1[,1]
  LG10 <- (LGcoefficients[1+i]+10)/sum((LGcoefficients[1+i]+10))*10;weights$LG10 <- LG10[,1]
  
  write.csv(weights,paste0("~/GitHub/ohi-canada/eezCHONE/rawdata.Canada-CHONe2014/weights/weights_",varia[i],".csv"))
}
