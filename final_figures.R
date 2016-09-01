source("functions.R")
source("logitmodelcoef_individuals.R")

res <- 1200
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
require(PMCMR)

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
detach(package:plyr)
library(PMCMR)
library(HH)


Likert <- raw %>% 
  dplyr::select(ends_with("general")) %>%
  gather(goal) %>% 
  group_by(goal) %>% 
  summarize(mean_imp=mean(value,na.rm=TRUE),
            sd_imp=sd(value,na.rm=TRUE))

Likert <- Likert[c(7,1,9,3,6,5,10,8,4,2),]

# stats
Likert2 <- raw %>% 
  dplyr::select(ends_with("general")) %>%
  gather(goal)
Likert2$goal <- as.factor(Likert2$goal)
kruskal.test(value ~ goal, data = Likert2)
posthoc.kruskal.nemenyi.test(value ~ goal, data = Likert2)

# plot

# pdf(paste('figures/f1_classweights.pdf'),height=7,width=single)
png(paste('figures/f1_classweights.png'),height=9,width=single,unit="in",res=res)
layout(matrix(c(1:3),nrow=3),heights=17+c(2.5,2,14))

#Likert
par(mar=c(1,5,1.5,0))
hh <- t(Likert$mean_imp)
se <- t(Likert$sd_imp)

# colnames(hh) <- LG_data_1class$Attributes
mp <- barplot(hh,las=3,ylim=c(0,5.5),cex.axis=1.5)
segments(mp, hh, mp, hh + se)
segments(mp+0.2, hh + se, mp-0.2, hh + se)
title(ylab='Likert Importance',cex.lab=1.5)
axis(1,at=mp,labels=F)
box(bty='l')
mtext("A)",adj=-0.2)
x <- c("a","b","c","d","c","be","e","be","f","g")
x <- x[c(7,1,9,3,6,5,10,8,4,2)]
axis(3,at=mp,labels=as.character(x),tick=FALSE,padj=1.5)
# Likert2 <- table(Likert2) %>% data.frame()
# likert(goal ~ value , value='Freq', data=Likert2,
#        as.percent=TRUE,
#        ylab="Percent Relative Ranking\n Low                                    Neutral                                 High",
#        xlab="Communication Category", 
#        main="Ranking of Graduate Training",
#        horizontal = FALSE,
#        rightAxis = FALSE)

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
mtext("B)",adj=-0.2)

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
mtext("C)",adj=-0.2)
mp
text(cex=1.5, x=seq(3,30,by=3), y=-0.15, GoalNames, xpd=TRUE, srt=50, pos=2)
dev.off()

LG_data_2class[,1:6] %>% arrange(desc(Class1))
LG_data_2class[,1:6] %>% arrange(desc(Class2))


#### Figure 2 ####
library(multcomp)

predictors <- c("sample","ocean_days","seafood_eat","demoone_gender","envr_org","envr_protest","demothree_income2","demothree_householdsize","rural","distance_FSA","rec","jobs2","demoone_education2","demothree_age_num","political_party2")
predictor_names <- c("region","ocean days","seafood eat","gender","envr org","envr protest","income","householdsize","rural","distance","recreation","job type","education","age","political party")

ind_lmc <- cbind(lmc2,raw[,names(raw)%in%predictors])
ind_lmc$distance_FSA2 <- round(ind_lmc$distance_FSA/200000)*200000
ind_lmc$rec2 <- ceiling(ind_lmc$rec/4)*4
ind_lmc$demothree_income3 <- as.numeric(ind_lmc$demothree_income2)/1000
levels(ind_lmc$envr_org)[1] <- NA
levels(ind_lmc$envr_protest)[1] <- NA
levels(ind_lmc$demoone_gender)[1] <- NA


for(i in 11:28){
  if(is.character(ind_lmc[,i])){
    ind_lmc[,i] <- factor(ind_lmc[,i])
  }
}

# fit <- manova(as.formula(paste("as.matrix(ind_lmc[,1:10])~",paste(paste0("ind_lmc$",predictors), collapse= "+"))))

# pdf(paste('figures/f2_ANOVAS.pdf'),height=10,width=full)
png(paste('figures/f2_ANOVAS.png'),height=8,width=full,units = "in",res=res)

# aovs <- summary.aov(fit)
layout(matrix(c(1:10),nrow=5),heights=c(2,2,2,2,3.25))

for(i in 1:10){
  fit <- glm(as.formula(paste("ind_lmc[,i]~",paste(paste0("ind_lmc$",predictors), collapse= "+"))))
  fit <- summary(aov(fit))[[1]]
  if(i==5|i==10){
    mar <- c(7,5,1,1)
    effects_barplot(fit$`F value`,fit$Df,1659,fit$`Pr(>F)`,rep("",length(predictor_names)),mar,ylim=c(0,9))
    text(cex=1, x=seq(1,18,by=1.2)+0.4, y=-0.7, predictor_names, xpd=TRUE, srt=50, pos=2)
    box(bty='l')
    title(GoalNames[i])
  } else {
    mar <- c(1,5,1,1)
    effects_barplot(fit$`F value`,fit$Df,1659,fit$`Pr(>F)`,rep("",length(predictor_names)),mar,ylim=c(0,9))
    box(bty='l')
    title(GoalNames[i])
  }
  mtext(paste0(LETTERS[i],")"),adj=-0.2,cex=2/3)
}
dev.off()


#### Figure 3 ####
require(multcomp)

ind_lmc_rescaled <- ind_lmc
ind_lmc_rescaled[,1:10] <- ind_lmc_rescaled[,1:10]/sum(apply(ind_lmc_rescaled[,1:10],2,mean))*10

# predictors <- c("sample","ocean_days","seafood_eat","demoone_gender","demotwo_livedCanada","envr_org","envr_protest","demothree_income3","demothree_householdsize","rural","distance_FSA2","rec","jobs2","demoone_education2","demothree_age_num","political_party2")
# aovs <- summary.aov(fit)
# for(i in 11:28){
#   if(is.numeric(ind_lmc[,i])){
#     ind_lmc[,i] <- ordered(ind_lmc[,i])
#   }
# }
# 
# plot_numeric <- function(xvar,yvar,xname,yname,pred,temp_data){
#   require(dplyr)
#   xs <- select(temp_data,get(xvar))[,1]
#   ys <- select(temp_data,get(yvar))[,1]
#   plot(ys~xs,2,xlab="",ylab="",pch="+")
#   title(ylab=yname,xlab=xname)
#   fit <- summary(glm(as.formula(paste("ys~",pred)),data=temp_data))$coefficients
#   x <- sort(as.numeric(unique(xs)))
#   y <- coefficients(lm(ys~xs))[1]+x*fit[row.names(fit)==xvar,1]
#   lines(x,y,lwd=2)
#   y <- coefficients(lm(ys~xs))[1]+x*(fit[row.names(fit)==xvar,1]+1.96*sqrt(2026)*fit[row.names(fit)==xvar,2])
#   lines(x,y,lty=3,lwd=2)
#   y <- coefficients(lm(ys~xs))[1]+x*(fit[row.names(fit)==xvar,1]-1.96*sqrt(2026)*fit[row.names(fit)==xvar,2])
#   lines(x,y,lty=3,lwd=2)
# }

# pdf(paste('figures/f3_users.pdf'),height=10,width=full)
png(paste('figures/f3_maineffects.png'),height=10,width=full ,units = "in",res=res)

layout(matrix(c(1:10),nrow=5))
par(mar=c(5,4,1,1))
boxplot(FoodProvision~seafood_eat,data=ind_lmc_rescaled,las=3)
mtext('Food Provision',2,padj=-3.5,cex=2/3);title(xlab='Seafood eating frequency (yr^-1)')
fit <- summary(glm(as.formula(paste("FoodProvision~",paste(predictors,collapse=" + "))),data=ind_lmc_rescaled))$coefficients
pvalue <- round(fit[row.names(fit)=="seafood_eat",4],3)
if(pvalue==0) pvalue <- "<0.001"
mtext(paste("S = ",signif(fit[row.names(fit)=="seafood_eat",1],2),
            "SE =",signif(fit[row.names(fit)=="seafood_eat",2],2),
            "p =",pvalue),side=3,cex=0.7)
mtext("A)",adj=-0.2,cex=2/3)


boxplot(AboriginalNeeds~as.character(envr_org),data=ind_lmc_rescaled,names=c('No','Yes'))
mtext('Aboriginal Needs',2,padj=-3.5,cex=2/3);title(xlab='ENGO Membership')
fit <- aov(as.formula(paste("AboriginalNeeds~",paste(predictors,collapse=" + "))),data=ind_lmc_rescaled)
x <- as.data.frame(cld(glht(fit,linfct=mcp(envr_org="Tukey")))$mcletters$Letters)
axis(3,c(1:nrow(x)),labels=as.character(x[,1]),tick=FALSE,padj=1.5)
mtext("B)",adj=-0.2,cex=2/3)


boxplot(NaturalProducts~political_party2,data=ind_lmc_rescaled,names=c('CPC','LIB','NDP/Green','Other'))
mtext('Natural Products',2,padj=-3.5,cex=2/3);title(xlab='Political Party')
fit <- aov(as.formula(paste("NaturalProducts~",paste(predictors,collapse=" + "))),data=ind_lmc_rescaled)
x <- as.data.frame(cld(glht(fit,linfct=mcp(political_party2="Tukey")))$mcletters$Letters)
axis(3,c(1:nrow(x)),labels=as.character(x[,1]),tick=FALSE,padj=1.5)
mtext("C)",adj=-0.2,cex=2/3)

boxplot(CarbonStorage~demothree_age_num,data=ind_lmc_rescaled,names=c("20-24","22-34","35-54","35-44","55-64","65+"),las=3)
mtext('Carbon Storage',2,padj=-3.5,cex=2/3);title(xlab='Age')
fit <- summary(glm(as.formula(paste("CarbonStorage~",paste(predictors,collapse=" + "))),data=ind_lmc_rescaled))$coefficients
pvalue <- round(fit[row.names(fit)=="demothree_age_num",4],3) 
if(pvalue==0) pvalue <- "<0.001" 
mtext(paste("S = ",signif(fit[row.names(fit)=="demothree_age_num",1],2),
            "SE =",signif(fit[row.names(fit)=="demothree_age_num",2],2),
            "p =",pvalue),side=3,cex=0.7)
mtext("D)",adj=-0.2,cex=2/3)

boxplot(CoastalProtection~jobs2,data=ind_lmc_rescaled,names=c('Conservation','Exploitation','Non-ocean'))
mtext('Coastal Protection',2,padj=-3.5,cex=2/3);title(xlab='Type of job')
fit <- aov(as.formula(paste("CoastalProtection~",paste(predictors,collapse=" + "))),data=ind_lmc_rescaled)
x <- as.data.frame(cld(glht(fit,linfct=mcp(jobs2="Tukey")))$mcletters$Letters)
axis(3,c(1:nrow(x)),labels=as.character(x[,1]),tick=FALSE,padj=1.5)
mtext("E)",adj=-0.2,cex=2/3)

boxplot(CoastalLivelihoods~demothree_age_num,names=c("20-24","22-34","35-54","35-44","55-64","65+"),las=3,data=ind_lmc_rescaled)
mtext('Coastal Livelihoods',2,padj=-3.5,cex=2/3);title(xlab='Age')
fit <- summary(glm(as.formula(paste("CoastalLivelihoods~",paste(predictors,collapse=" + "))),data=ind_lmc_rescaled))$coefficients
pvalue <- round(fit[row.names(fit)=="demothree_age_num",4],3) 
if(pvalue==0) pvalue <- "<0.001" 
mtext(paste("S = ",signif(fit[row.names(fit)=="demothree_age_num",1],2),
            "SE =",signif(fit[row.names(fit)=="demothree_age_num",2],2),
            "p =",pvalue),side=3,cex=0.7)
mtext("H)",adj=-0.2,cex=2/3)

boxplot(TourismRecreation~rec2,data=ind_lmc_rescaled,names=c("0","1-4","5-8"))
mtext('Tourism & Recreation',2,padj=-3.5,cex=2/3);title(xlab='Number of Recreational Activities')
fit <- summary(glm(as.formula(paste("TourismRecreation~",paste(predictors,collapse=" + "))),data=ind_lmc_rescaled))$coefficients
pvalue <- round(fit[row.names(fit)=="rec",4],3) 
if(pvalue==0) pvalue <- "<0.001" 
mtext(paste("S = ",signif(fit[row.names(fit)=="rec",1],2),
            "SE =",signif(fit[row.names(fit)=="rec",2],2),
            "p =",pvalue),side=3,cex=0.7)
mtext("G)",adj=-0.2,cex=2/3)

boxplot(IconicPlacesSPecies~demothree_age_num,names=c("20-24","22-34","35-54","35-44","55-64","65+"),las=3,data=ind_lmc_rescaled)
mtext('Iconic Places & Species',2,padj=-3.5,cex=2/3);title(xlab='Age')
fit <- summary(glm(as.formula(paste("IconicPlacesSPecies~",paste(predictors,collapse=" + "))),data=ind_lmc_rescaled))$coefficients
pvalue <- round(fit[row.names(fit)=="demothree_age_num",4],3) 
if(pvalue==0) pvalue <- "<0.001" 
mtext(paste("S = ",signif(fit[row.names(fit)=="demothree_age_num",1],2),
            "SE =",signif(fit[row.names(fit)=="demothree_age_num",2],2),
            "p =",pvalue),side=3,cex=0.7)
mtext("H)",adj=-0.2,cex=2/3)

boxplot(CleanWaters~jobs2,data=ind_lmc_rescaled,names=c('Conservation','Exploitation','Non-ocean'))
mtext('Clean Waters',2,padj=-3.5,cex=2/3);title(xlab='Type of job')
fit <- aov(as.formula(paste("CleanWaters~",paste(predictors,collapse=" + "))),data=ind_lmc_rescaled)
x <- as.data.frame(cld(glht(fit,linfct=mcp(jobs2="Tukey")))$mcletters$Letters)
axis(3,c(1:nrow(x)),labels=as.character(x[,1]),tick=FALSE,padj=1.5)
mtext("I)",adj=-0.2,cex=2/3)


boxplot(Biodiversity~demothree_age_num,data=ind_lmc_rescaled,names=c("20-24","22-34","35-54","35-44","55-64","65+"),las=3)
mtext('Biodiversity',2,padj=-3.5,cex=2/3);title(xlab='Age')
fit <- summary(glm(as.formula(paste("Biodiversity~",paste(predictors,collapse=" + "))),data=ind_lmc_rescaled))$coefficients
pvalue <- round(fit[row.names(fit)=="demothree_age_num",4],3) 
if(pvalue==0) pvalue <- "<0.001" 
mtext(paste("S = ",signif(fit[row.names(fit)=="demothree_age_num",1],2),
            "SE =",signif(fit[row.names(fit)=="demothree_age_num",2],2),
            "p =",pvalue),side=3,cex=0.7)
mtext("J)",adj=-0.2,cex=2/3)

dev.off()



#### Figure 4 extractive vs non:extractive ####
require(compute.es)
lmc_categorized <- as.data.frame(cbind(NE=apply(with(lmc2,cbind(CarbonStorage,CoastalProtection,TourismRecreation,IconicPlacesSPecies,CleanWaters,Biodiversity)),1,mean),
                         E=apply(with(lmc2,cbind(FoodProvision,AboriginalNeeds,NaturalProducts,CoastalLivelihoods)),1,mean)))
sum(lmc_categorized$E>lmc_categorized$NE)/nrow(lmc_categorized)

ind_lmc_rescaled$E_NE_ratio <- lmc_categorized$E/lmc_categorized$NE
fit <- aov(as.formula(paste("log(ind_lmc_rescaled$E_NE_ratio) ~",paste(predictors,collapse=" + "))),data=ind_lmc_rescaled)
summary(fit)

sum(log(ind_lmc_rescaled$E_NE_ratio)<0)/nrow(ind_lmc_rescaled)

# pdf(paste('figures/f3_users.pdf'),height=10,width=full)
png(paste('figures/f4_E_VS_NE.png'),height=8,width=full ,units = "in",res=res)

layout(matrix(c(1:8),nrow=4))

effects_barplot(summary(fit)[[1]][,4],summary(fit)[[1]][,1],1658,summary(fit)[[1]][,5],rep("",length(predictor_names)),mar=c(5,4,1,1),ylim=c(0,9))
text(cex=0.75, x=seq(1,18,by=1.2)+0.4, y=-0.7, predictor_names, xpd=TRUE, srt=50, pos=2)
mtext("A)",adj=-0.2,cex=2/3)


par(mar=c(5,4,1,1))

boxplot(log(E_NE_ratio)~demothree_age_num,data=ind_lmc_rescaled,names=c("20-24","22-34","35-54","35-44","55-64","65+"),las=3)
mtext('log(E:NE)',2,padj=-3.5,cex=2/3);title(xlab='Age')
fit <- summary(glm(as.formula(paste("log(E_NE_ratio)~",paste(predictors,collapse=" + "))),data=ind_lmc_rescaled))$coefficients
pvalue <- round(fit[row.names(fit)=="demothree_age_num",4],3) 
if(pvalue==0) pvalue <- "<0.001" 
mtext(paste("S = ",signif(fit[row.names(fit)=="demothree_age_num",1],2),
            "SE =",signif(fit[row.names(fit)=="demothree_age_num",2],2),
            "p =",pvalue),side=3,cex=0.6)
mtext("B)",adj=-0.2,cex=2/3)


boxplot(log(E_NE_ratio)~seafood_eat,data=ind_lmc_rescaled,las=3)
mtext('log(E:NE)',2,padj=-3.5,cex=2/3);title(xlab=expression('Seafood eating frequency'~(yr^{-1})))
fit <- summary(glm(as.formula(paste("log(E_NE_ratio)~",paste(predictors,collapse=" + "))),data=ind_lmc_rescaled))$coefficients
pvalue <- round(fit[row.names(fit)=="seafood_eat",4],3)
if(pvalue==0) pvalue <- "<0.001"
mtext(paste("S = ",signif(fit[row.names(fit)=="seafood_eat",1],2),
            "SE =",signif(fit[row.names(fit)=="seafood_eat",2],2),
            "p =",pvalue),side=3,cex=0.6)
mtext("C)",adj=-0.2,cex=2/3)


boxplot(log(E_NE_ratio)~rural,data=ind_lmc_rescaled,names=c("Rural","Urban"))
title(ylab='log(E:NE)')
fit <- aov(as.formula(paste("log(E_NE_ratio)~",paste(predictors,collapse=" + "))),data=ind_lmc_rescaled)
x <- as.data.frame(cld(glht(fit,linfct=mcp(rural="Tukey")))$mcletters$Letters)
axis(3,c(1:nrow(x)),labels=as.character(x[,1]),tick=FALSE,padj=1.5)
mtext("D)",adj=-0.2,cex=2/3)

boxplot(log(E_NE_ratio)~rec2,data=ind_lmc_rescaled,names=c("0","1-4","5-8"))
mtext('log(E:NE)',2,padj=-3.5,cex=2/3);title(xlab='Number of Recreational Activities')
fit <- summary(glm(as.formula(paste("log(E_NE_ratio)~",paste(predictors,collapse=" + "))),data=ind_lmc_rescaled))$coefficients
pvalue <- round(fit[row.names(fit)=="rec",4],3) 
if(pvalue==0) pvalue <- "<0.001" 
mtext(paste("S = ",signif(fit[row.names(fit)=="rec",1],2),
            "SE =",signif(fit[row.names(fit)=="rec",2],2),
            "p =",pvalue),side=3,cex=0.6)
mtext("E)",adj=-0.2,cex=2/3)

boxplot(log(E_NE_ratio)~as.character(envr_org),data=ind_lmc_rescaled,names=c('No','Yes'))
mtext('log(E:NE)',2,padj=-3.5,cex=2/3);title(xlab='ENGO Membership')
fit <- aov(as.formula(paste("log(E_NE_ratio)~",paste(predictors,collapse=" + "))),data=ind_lmc_rescaled)
x <- as.data.frame(cld(glht(fit,linfct=mcp(envr_org="Tukey")))$mcletters$Letters)
axis(3,c(1:nrow(x)),labels=as.character(x[,1]),tick=FALSE,padj=1.5)
mtext("F)",adj=-0.2,cex=2/3)

boxplot(log(E_NE_ratio)~political_party2,data=ind_lmc_rescaled,names=c('CPC','LIB','NDP/Green','Other'))
mtext('log(E:NE)',2,padj=-3.5,cex=2/3);title(xlab='Political Party')
fit <- aov(as.formula(paste("log(E_NE_ratio)~",paste(predictors,collapse=" + "))),data=ind_lmc_rescaled)
x <- as.data.frame(cld(glht(fit,linfct=mcp(political_party2="Tukey")))$mcletters$Letters)
axis(3,c(1:nrow(x)),labels=as.character(x[,1]),tick=FALSE,padj=1.5)
mtext("G)",adj=-0.2,cex=2/3)

boxplot(log(E_NE_ratio)~as.character(demoone_gender),data=ind_lmc_rescaled,names=c('Female','Male'))
mtext('log(E:NE)',2,padj=-3.5,cex=2/3)
fit <- aov(as.formula(paste("log(E_NE_ratio)~",paste(predictors,collapse=" + "))),data=ind_lmc_rescaled)
x <- as.data.frame(cld(glht(fit,linfct=mcp(envr_protest="Tukey")))$mcletters$Letters)
axis(3,c(1:nrow(x)),labels=as.character(x[,1]),tick=FALSE,padj=1.5)
mtext("H)",adj=-0.2,cex=2/3)

dev.off()

#### Figure Appendix ####


source('Appendix_figs.R')




