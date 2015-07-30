

raw <- read.csv("~/R/OHI survey results/Latent Gold/userstats_bothlang_usable.csv")
data <- raw[,grep("general",names(raw))]
#remove blanks
# raw <- raw[!apply(data,1,function(x) any(is.na(x))),]
# data <- data[!apply(data,1,function(x) any(is.na(x))),]

#### cleanup data
#### cleanup FSA ####
raw$demotwo_fsa[substr(raw$demotwo_fsa,2,2)=="O"] <- gsub("O","0",raw$demotwo_fsa[substr(raw$demotwo_fsa,2,2)=="O"])
raw$demotwo_fsa <- as.character(raw$demotwo_fsa)

#### rural vs urban ####
raw$rural <- "urban"
raw$rural[substr(raw$demotwo_fsa,2,2)=="0"] <- "rural"

#### calculate ocean distance based on FSA ####
require(dplyr)
FSA_dist <- read.csv("~/R/OHI survey results/GIS/FSA_dist.txt",stringsAsFactors=F)
raw$distance_FSA <- (left_join(raw,FSA_dist,by=c("demotwo_fsa"="CFSAUID")))$NEAR_DIST
raw$distance_FSA <- as.numeric(gsub(",","",raw$distance_FSA))
raw$distance_FSAc <- ">1000km"
raw$distance_FSAc[raw$distance_FSA<500000] <- "<500km"
raw$distance_FSAc[raw$distance_FSA<100000] <- "<100km"
hist(as.numeric(raw$distance_FSA),nclass=2000)


#### reorganize rec and jobs ####
rec_types=c("rec_swim","rec_boat","rec_surf","rec_scuba","rec_powerboat",
            "rec_recfish","rec_sightsee","rec_none","rec_other","rec_specify2")
jobs_types=c("jobs_comfish","jobs_aquaculture","jobs_firstnations","jobs_tourism","jobs_transport",
             "jobs_port","jobs_govt","jobs_none","jobs_other","jobs_specify2")

raw$rec_specify2 <- 1
raw$rec_specify2[raw$rec_specify==""] <- 0
raw$rec <- rowSums(raw[,(names(raw) %in% rec_types)],na.rm=T)

raw$jobs_specify2 <- 1
raw$jobs_specify2[raw$jobs_specify==""] <- 0
raw$jobsnum <- rowSums(x <- raw[,(names(raw) %in% jobs_types)],na.rm=T)
raw$jobs <- "none"
raw$jobs[raw$jobs_comfish==1] <- "comfish"
raw$jobs[raw$jobs_aquaculture==1] <- "aquaculture"
raw$jobs[raw$jobs_firstnations==1] <- "firstnations"
raw$jobs[raw$jobs_tourism==1] <- "tourism"
raw$jobs[raw$jobs_transport==1] <- "transport"
raw$jobs[raw$jobs_port==1] <- "port"
raw$jobs[raw$jobs_govt==1] <- "govt"
raw$jobs[raw$jobs_other==1] <- "other"
raw$jobs[raw$jobsnum>1] <- "many"

raw$jobs2 <- "none/other"
raw$jobs2[raw$jobs_comfish==1] <- "extract"
raw$jobs2[raw$jobs_aquaculture==1] <- "extract"
raw$jobs2[raw$jobs_transport==1] <- "extract"
raw$jobs2[raw$jobs_port==1] <- "extract"
raw$jobs2[raw$jobs_firstnations==1] <- "conservation"
raw$jobs2[raw$jobs_tourism==1] <- "conservation"
raw$jobs2[raw$jobs_govt==1] <- "conservation"
raw$jobs2[raw$jobs_other==1] <- "none/other"

#### fix householdsize ####

raw$demothree_householdsize[raw$demothree_householdsize=="On"] <- 1
raw$demothree_householdsize[raw$demothree_householdsize=="One"] <- 1
raw$demothree_householdsize[raw$demothree_householdsize=="on"] <- 1
raw$demothree_householdsize[raw$demothree_householdsize=="one"] <- 1
raw$demothree_householdsize[raw$demothree_householdsize==""] <- 0
raw$demothree_householdsize[is.na(raw$demothree_householdsize)] <- 0
raw$demothree_householdsize <- as.numeric(raw$demothree_householdsize)

#### fix education ####

raw$demoone_education2 <- as.character(raw$demoone_education)
raw$demoone_education2[raw$demoone_education=="elementary"] <- 'grade'
raw$demoone_education2[raw$demoone_education=="HS"] <- 'grade'
raw$demoone_education2[raw$demoone_education=="somepost"] <- 'uni'
raw$demoone_education2[raw$demoone_education=="bachelors"] <- 'uni'
raw$demoone_education2[raw$demoone_education=="college"] <- 'uni'
raw$demoone_education2[raw$demoone_education=="graduate"] <- 'uni'

#### fix seafood_eat ####
raw$seafood_eat <- as.character(raw$seafood_eat)
raw$seafood_eat[raw$seafood_eat=="Never"] <- 0
raw$seafood_eat[raw$seafood_eat==""] <- 0
raw$seafood_eat[raw$seafood_eat=="1to2yr"] <- 1.5
raw$seafood_eat[raw$seafood_eat=="3to6yr"] <- 4.5
raw$seafood_eat[raw$seafood_eat=="7to11yr"] <- 9
raw$seafood_eat[raw$seafood_eat=="1to2month"] <- 18
raw$seafood_eat[raw$seafood_eat=="3month"] <- 36
raw$seafood_eat[raw$seafood_eat=="atleast1wk"] <- 52
raw$seafood_eat <- as.numeric(raw$seafood_eat)

#### numeric age class ####
raw$demothree_age_num <- as.character(raw$demothree_age)
raw$demothree_age_num[raw$demothree_age=="19"] <- 19
raw$demothree_age_num[raw$demothree_age=="20to24"] <- 22
raw$demothree_age_num[raw$demothree_age=="25to34"] <- 29.5
raw$demothree_age_num[raw$demothree_age=="35to44"] <- 39.5
raw$demothree_age_num[raw$demothree_age=="45to54"] <- 49.5
raw$demothree_age_num[raw$demothree_age=="55to64"] <- 59.5
raw$demothree_age_num[raw$demothree_age=="65plus"] <- 69.5
raw$demothree_age_num <- as.numeric(raw$demothree_age_num)

#### numeric ocean days ####
raw$ocean_days <- as.character(raw$ocean_days)
raw$ocean_days[raw$ocean_days=="none"] <- 0
raw$ocean_days[raw$ocean_days=="lessthan5"] <- 2.5
raw$ocean_days[raw$ocean_days=="5to9"] <- 7
raw$ocean_days[raw$ocean_days=="10to29"] <- 19.5
raw$ocean_days[raw$ocean_days=="30to49"] <- 39.5
raw$ocean_days[raw$ocean_days=="50to100"] <- 75
raw$ocean_days[raw$ocean_days=="morethan100"] <- 150
raw$ocean_days[raw$ocean_days=="dontknow"] <- 0
raw$ocean_days <- as.numeric(raw$ocean_days)

#### fix political party ####
raw$political_party2 <- as.character(raw$political_party)
raw$political_party2[raw$political_party=="Bloc"] <- "Other"
raw$political_party2[raw$political_party=="CPC"] <- "CPC"
raw$political_party2[raw$political_party=="green"] <- "NDP/Green"
raw$political_party2[raw$political_party=="liberal"] <- "Liberal"
raw$political_party2[raw$political_party=="NDP"] <- "NDP/Green"
raw$political_party2[raw$political_party=="noparty"] <- "Other"
raw$political_party2[raw$political_party=="dontknow"] <- "Other"
raw$political_party2[raw$political_party=="noresponse"] <- "Other"
raw$political_party2[raw$political_party==""] <- "Other"
unique(raw$political_party2)

#### deal with missing values ####
raw$envr_org[raw$envr_org==""] <- "dontknow"

#### remove useless variables ####
names(raw)
keeps <- c("id.x","name","pbw_cs","pbw_choice","pbw_choicenumeric","sweight","seq","task",
           "sample","ocean_days","seafood_eat","demoone_gender","demoone_education","demoone_education2","demotwo_livedCanada",
           "envr_org","envr_protest","political_party2","demothree_income","demothree_householdsize",
           "demothree_age","demothree_age_num","rural","distance_FSA","distance_FSAc","rec","jobsnum","jobs2")
raw <-raw[,(names(raw) %in% keeps)]

pca <- prcomp(lmc2,
                 center = TRUE,
                 scale. = TRUE) 
print(pca)
summary(pca)


# library(devtools)
# install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(pca, choices=1:2,obs.scale = 1, var.scale = 1, 
              groups = raw$jobs2, ellipse = TRUE, 
              circle = TRUE,varname.size=5)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

require(ggplot2)

theta <- seq(0,2*pi,length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))
p <- ggplot(circle,aes(x,y)) + geom_path()

loadings <- data.frame(pca$rotation, 
                       .names = row.names(pca$rotation))
p + geom_text(data=loadings, 
              mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
  coord_fixed(ratio=1) +
  labs(x = "PC1", y = "PC2")


