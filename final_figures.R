# source("anova.R")

res <- 400
qual <- 100
#Figure widths
single <- 8.6/2.54
twothirds <- 11/2.54
full <- 17.3/2.54

GoalNames = c("Food Provision",
                     "Aboriginal Needs",
                     "Natural Products",
                     "Carbon Storage",
                     "Coastal Protection",
                     "Coastal Livelihoods",
                     "Tourism & Recreation",
                     "Iconic Places & Species",
                     "Clean Waters",
                     "Biodiversity")

#### Figure 1 ####
library(readxl)
# 1 class
LG_data_1class <- read_excel("Latent Gold/final_analyses.xlsx",sheet=1,col_names=T,skip=4)[1:18,1:8]

#clean up the LG results
LG_data_1class$Attributes <- LG_data_1class$Attributes[c(nrow(LG_data_1class),1:(nrow(LG_data_1class)-1))]
LG_data_1class[3,] <- 0
LG_data_1class$Attributes[3] <- "AboriginalNeeds"
LG_data_1class <- LG_data_1class[!is.na(LG_data_1class$Attributes),]
LG_data_1class$Class1 <- as.numeric(LG_data_1class$Class1)+1
LG_data_1class$s.e. <- LG_data_1class$s.e./sum(LG_data_1class$Class1)*10
LG_data_1class$Class1 <- LG_data_1class$Class1/sum(LG_data_1class$Class1)*10

#calculate the Likert scale importance
library(dplyr)
library(tidyr)
Likert <- raw %>% 
  select(ends_with("general")) %>%
  gather(goal) %>% 
  group_by(goal) %>% 
  summarize(mean_imp=mean(value,na.rm=TRUE),
            sd_imp=sd(value,na.rm=TRUE))



# plot

# pdf(paste('figures/f1_classweights.pdf'),height=7,width=single)
jpeg(paste('figures/f1_classweights.jpg'),height=9,width=single,unit="in",res=res,qual=qual)
layout(matrix(c(1:3),nrow=3),heights=17+c(1,2,14))

#Likert
par(mar=c(1,5,0,0))
hh <- t(Likert$mean_imp)
se <- t(Likert$sd_imp)

# colnames(hh) <- LG_data_1class$Attributes
mp <- barplot(hh,las=3,ylim=c(0,1.2*max(hh+se)),cex.axis=1.5)
segments(mp, hh, mp, hh + se)
segments(mp+0.2, hh + se, mp-0.2, hh + se)
title(ylab='Likert Importance',cex.lab=1.5)
axis(1,at=mp,labels=F)
box(bty='l')

# latent gold
par(mar=c(1,5,1,0))
hh <- t(LG_data_1class$Class1)
se <- t(LG_data_1class$s.e.)

# colnames(hh) <- LG_data_1class$Attributes
mp <- barplot(hh,las=3,ylim=c(0,1.2*max(hh+se)),cex.axis=1.5)
segments(mp, hh, mp, hh + se)
segments(mp+0.2, hh + se, mp-0.2, hh + se)
title(ylab='Relative Importance',cex.lab=1.5)
axis(1,at=mp,labels=F)
box(bty='l')

# 2 classes
LG_data_2class <- read_excel("Latent Gold/final_analyses.xlsx",sheet=2,col_names=T,skip=4)[1:18,1:13]

#clean up the LG results
LG_data_2class$Attributes <- LG_data_2class$Attributes[c(nrow(LG_data_2class),1:(nrow(LG_data_2class)-1))]
LG_data_2class[3,] <- 0
LG_data_2class$Attributes[3] <- "AboriginalNeeds"
LG_data_2class <- LG_data_2class[!is.na(LG_data_2class$Attributes),]
LG_data_2class$Class1 <- as.numeric(LG_data_2class$Class1)+1
LG_data_2class$Class2 <- as.numeric(LG_data_2class$Class2)+1
LG_data_2class$s.e. <- as.numeric(LG_data_2class$s.e.)/sum(LG_data_2class$Class1)*10
LG_data_2class$Class1 <- LG_data_2class$Class1/sum(LG_data_2class$Class1)*10
LG_data_2class$s.e.2 <- as.numeric(LG_data_2class$s.e.2)/sum(LG_data_2class$Class2)*10
LG_data_2class$Class2 <- LG_data_2class$Class2/sum(LG_data_2class$Class2)*10

# plot
par(mar=c(13,5,1,0))
hh <- t(cbind(LG_data_2class$Class1,LG_data_2class$Class2))
se <- t(cbind(LG_data_2class$s.e.,LG_data_2class$s.e.2))

#colnames(hh) <- LG_data_2class$Attributes
mp <- barplot(hh,
        beside=TRUE,
        legend=c('Class 1','Class 2'),
        las=3,
        ylim=c(0,1.2*max(hh+se)),
        args.legend = list(x=13,y=2.3,bty='n',cex=1.5),cex.axis=1.5)
title(ylab='Relative Importance',cex.lab=1.5)
segments(mp, hh, mp, hh + se)
segments(mp+0.2, hh + se, mp-0.2, hh + se)
axis(1,at=seq(2,29,by=3),labels=F)
box(bty='l')
mp
text(cex=1.5, x=seq(3,30,by=3), y=-0.15, GoalNames, xpd=TRUE, srt=50, pos=2)
dev.off()

LG_data_2class %>% arrange(desc(Class1))
LG_data_2class %>% arrange(desc(Class2))


#### Figure 2 ####
predictors <- c("sample","ocean_days","seafood_eat","demoone_gender","demotwo_livedCanada","envr_org","envr_protest","demothree_income2","demothree_householdsize","rural","distance_FSA","rec","jobs2","demoone_education2","demothree_age_num","political_party2")
predictor_names <- c("province","ocean days","seafood eat","gender","lived in Canada","envr org","envr protest","income","householdsize","rural","distance","recreation","job type","education","age","political party")

ind_lmc <- cbind(lmc2,raw[,names(raw)%in%predictors])

fit <- manova(as.formula(paste("as.matrix(ind_lmc[,!names(ind_lmc)%in%predictors])~",paste(paste0("ind_lmc$",predictors), collapse= "+"))))

# pdf(paste('figures/f2_ANOVAS.pdf'),height=10,width=full)
jpeg(paste('figures/f2_ANOVAS.jpg'),height=8,width=full,units = "in",res=res,qual=qual)

aovs <- summary.aov(fit)
layout(matrix(c(1:10),nrow=5),heights=c(2,2,2,2,3.25))

for(i in seq_len(sum(!names(ind_lmc)%in%predictors))){
  if(i==5|i==10){
    mar <- c(7,5,1,1)
    effects_barplot(aovs[[i]][,4],aovs[[i]][,1],1942,aovs[[i]][,5],rep("",length(predictor_names)),mar,ylim=c(0,9))
    text(cex=1, x=seq(1,19,by=1.2)+0.4, y=-0.7, predictor_names, xpd=TRUE, srt=50, pos=2)
    box(bty='l')
    title(GoalNames[i])
    
  } else {
    mar <- c(1,5,1,1)
    effects_barplot(aovs[[i]][,4],aovs[[i]][,1],1942,aovs[[i]][,5],rep("",length(predictor_names)),mar,ylim=c(0,9))
    box(bty='l')
    title(GoalNames[i])
  }
}
dev.off()

#### Figure 3 ####
# pdf(paste('figures/f3_users.pdf'),height=10,width=full)
jpeg(paste('figures/f3_users.jpg'),height=6,width=single,units = "in",res=res,qual=qual)

aovs <- summary.aov(fit)
layout(matrix(c(1:2),nrow=2))
par(mar=c(5,4,1,0))
boxplot(FoodProvision~seafood_eat,data=ind_lmc)
title(ylab='Food Provision',xlab='Seafood eating frequency (yr^-1)')

ind_lmc$rec2 <- ceiling(ind_lmc$rec/4)*4
boxplot(TourismRecreation~rec2,data=ind_lmc,names=c("0","1-4","5-8"))
title(ylab='Tourism & Recreation',xlab='Number of Rec. Act.')

dev.off()


#### Figure 4 ####
# pdf(paste('figures/f4_sociodemo.pdf'),height=10,width=full)
jpeg(paste('figures/f4_users.jpg'),height=10,width=full,units = "in",res=res,qual=qual)
aovs <- summary.aov(fit)
layout(matrix(c(1:12),nrow=4,byrow = T))

#age
par(mar=c(5,4,0,1))
boxplot(CoastalLivelihoods~demothree_age_num,names=c("20-24","22-34","35-54","35-44","55-64","65+"),las=3,data=ind_lmc)
title(ylab='CoastalLivelihoods')

boxplot(CarbonStorage~demothree_age_num,data=ind_lmc,names=c("20-24","22-34","35-54","35-44","55-64","65+"),las=3)
title(ylab='CarbonStorage',xlab='Age')

boxplot(Biodiversity~demothree_age_num,data=ind_lmc,names=c("20-24","22-34","35-54","35-44","55-64","65+"),las=3)
title(ylab='Biodiversity')


#political party
boxplot(AboriginalNeeds~political_party2,data=ind_lmc,names=c('CPC','LIB','N/G','Other'))
title(ylab='AboriginalNeeds')

boxplot(TourismRecreation~political_party2,data=ind_lmc,names=c('CPC','LIB','N/G','Other'))
title(ylab='Tourism & Recreation',xlab='Political Party')

boxplot(Biodiversity~political_party2,data=ind_lmc,names=c('CPC','LIB','N/G','Other'))
title(ylab='Biodiversity')


# env org
boxplot(CoastalLivelihoods~as.character(envr_org),data=ind_lmc,names=c('Unknown','No','Yes'))
title(ylab='CoastalLivelihoods')

boxplot(CarbonStorage~as.character(envr_org),ind_lmc,names=c('Unknown','No','Yes'))
title(ylab='CarbonStorage',xlab='ENGO Membership')

boxplot(Biodiversity~as.character(envr_org),data=ind_lmc,names=c('Unknown','No','Yes'))
title(ylab='Biodiversity')

# other
boxplot(CleanWaters~jobs2,data=ind_lmc,names=c('Conser.','Extract.','None'))
title(ylab='CleanWaters',xlab='Type of job')

boxplot(CoastalLivelihoods~jobs2,data=ind_lmc,names=c('Conser.','Extract.','None'))
title(ylab='CoastalLivelihoods',xlab='Type of job')

boxplot(Biodiversity~demoone_education2,data=ind_lmc,names=c('Unknown','K-12','PS'))
title(ylab='CleanWaters',xlab='Education')

dev.off()
