source("logitmodelcoef_individuals.R")
require(compute.es)
require(reshape2)
predictors <- c("sample","ocean_days","seafood_eat","demoone_gender","demotwo_livedCanada","envr_org","envr_protest","demothree_income2","demothree_householdsize","rural","distance_FSA","rec","jobsnum","jobs2","demoone_education2","demothree_age_num","political_party2")
predictor_names <- c("province","ocean days","seafood eat","gender","lived in Canada","envr org","envr protest","income","householdsize","rural","distance_FSA","recreation","jobs (num)","jobs (cat)","education","age","political party")


ind_lmc <- cbind(lmc2,raw[,names(raw)%in%predictors])
fit <- manova(as.formula(paste("as.matrix(ind_lmc[,!names(ind_lmc)%in%predictors])~",paste(paste0("ind_lmc$",predictors), collapse= "+"))))

effects_barplot <- function(f,n1,n2,p,predictor_names=NULL,mar=c(7,5,1,2),ylim=NULL){
  par(mar=mar)
  x <- fes(f,n1,n2,verbose=F)
  if(is.null(ylim)) ylim <- c(0,max(x$d+sqrt(x$var.d),na.rm=TRUE)*1.1)
  mp <- barplot(x$d,ylim=ylim,ylab="Effect Size (Cohen's d)")
  axis(1,mp[-length(mp)],predictor_names,las=2)
  segments(mp[-length(mp)], x$d, mp[-length(mp)], x$d + sqrt(x$var.d))
  segments(mp[-length(mp)] - 0.2, x$d + sqrt(x$var.d), mp[-length(mp)] + 0.2, x$d + sqrt(x$var.d))
  significant <- p<0.05&p>0.01
  text(mp[-length(mp)][significant], (ylim[2]/10)+(x$d[-length(mp)][significant] + sqrt(x$var.d[-length(mp)][significant])),"*")
  significant <- p<0.01&p>0.001
  text(mp[-length(mp)][significant], (ylim[2]/10)+(x$d[-length(mp)][significant] + sqrt(x$var.d[-length(mp)][significant])),"**")
  significant <- p<0.001
  text(mp[-length(mp)][significant], (ylim[2]/10)+(x$d[-length(mp)][significant] + sqrt(x$var.d[-length(mp)][significant])),"***")
}


res <- 400
qual <- 100
# jpeg(paste('figures/MANOVA.jpg'),height=10,width=17,units="cm",res=res,qual=qual)

effects_barplot(summary(fit)$stats[, "approx F"],
                summary(fit)$stats[, "num Df"],
                summary(fit)$stats[, "den Df"],
                summary(fit)$stats[, "Pr(>F)"],
                predictor_names)
title("MANOVA")
# dev.off()


# jpeg(paste('figures/ANOVAS.jpg'),height=24,width=17,units="cm",res=res,qual=qual)

aovs <- summary.aov(fit)
layout(matrix(c(1:10),nrow=5),heights=c(2,2,2,2,3.25))
for(i in seq_len(sum(!names(ind_lmc)%in%predictors))){
  if(i==5|i==10){
    mar <- c(7,5,1,1)
    effects_barplot(aovs[[i]][,4],aovs[[i]][,1],1942,aovs[[i]][,5],predictor_names,mar,ylim=c(0,9))
    title(names(ind_lmc)[i])
    
  } else {
    mar <- c(1,5,1,1)
    effects_barplot(aovs[[i]][,4],aovs[[i]][,1],1942,aovs[[i]][,5],rep("",length(predictor_names)),mar,ylim=c(0,9))
    title(names(ind_lmc)[i])
  }
}
names(ind_lmc)
# dev.off()

# #### make some variables graphable
# ind_lmc$distance_FSA_rounded <- round(ind_lmc$distance_FSA/(300*1000))*300
# 
# #### food provision ####
# boxplot(FoodProvision~seafood_eat,data=ind_lmc,notch=TRUE)
# title(ylab='FoodProvision',xlab='Seafood eating frequency (yr^-1)')
# 
# boxplot(FoodProvision~demothree_age_num,data=ind_lmc,notch=TRUE)
# title(ylab='FoodProvision',xlab='age')
# 
# boxplot(FoodProvision~political_party2,data=ind_lmc,notch=TRUE)
# title(ylab='FoodProvision')
# 
# boxplot(FoodProvision~rural,data=ind_lmc,notch=TRUE)
# title(ylab='FoodProvision')
# 
# 
# #### aboriginal needs ####
# boxplot(AboriginalNeeds~demothree_age_num,ind_lmc,notch=TRUE)
# title(ylab='AboriginalNeeds',xlab='age')
# 
# boxplot(AboriginalNeeds~envr_org,ind_lmc,notch=TRUE)
# title(ylab='AboriginalNeeds',xlab='Enviro Org Membership')
# 
# boxplot(AboriginalNeeds~sample,ind_lmc,notch=TRUE)
# title(ylab='AboriginalNeeds')
# 
# boxplot(AboriginalNeeds~demothree_income,ind_lmc,notch=TRUE)
# title(ylab='AboriginalNeeds',xlab='income')
# 
# 
# 
# #### Natural Products ####
# boxplot(NaturalProducts~political_party2,ind_lmc,notch=TRUE)
# title(ylab='NaturalProducts')
# 
# boxplot(NaturalProducts~demoone_gender,ind_lmc,notch=TRUE)
# title(ylab='NaturalProducts',xlab='gender')
# 
# 
# 
# #### Carbon Storage ####
# boxplot(CarbonStorage~envr_org,ind_lmc,notch=TRUE)
# title(ylab='CarbonStorage',xlab='Enviro Org Membership')
# 
# boxplot(CarbonStorage~demothree_age_num,ind_lmc,notch=TRUE)
# title(ylab='CarbonStorage',xlab='age')
# 
# boxplot(CarbonStorage~demothree_income,ind_lmc,notch=TRUE)
# title(ylab='CarbonStorage',xlab='income')
# 
# 
# 
# #### Coastal Protection ####
# boxplot(CoastalProtection~seafood_eat,data=ind_lmc,notch=TRUE)
# title(ylab='CoastalProtection',xlab='Seafood eating frequency (yr^-1)')
# 
# boxplot(CoastalProtection~ocean_days,data=ind_lmc,notch=TRUE)
# title(ylab='CoastalProtection',xlab='ocean_days')
# 
# boxplot(CoastalProtection~distance_FSA_rounded,data=ind_lmc,notch=TRUE)
# title(ylab='CoastalProtection',xlab='Distance from shore (km)')
# 
# 
# 
# #### Coastal Livelihoods ####
# boxplot(CoastalLivelihoods~demothree_age_num,data=ind_lmc,notch=TRUE)
# title(ylab='CoastalLivelihoods',xlab='age')
# 
# boxplot(CoastalLivelihoods~demothree_age_num,data=ind_lmc,notch=TRUE)
# title(ylab='CoastalLivelihoods',xlab='age')
# 
# #### Tourism Recreation ####
# boxplot(TourismRecreation~rec,data=ind_lmc,notch=TRUE)
# title(ylab='TourismRecreation',xlab='Number of Rec. Act.')
# 
# boxplot(TourismRecreation~political_party2,ind_lmc,notch=TRUE)
# title(ylab='TourismRecreation')
# 
# 
# 
# #### Iconic Places SPecies ####
# boxplot(IconicPlacesSPecies~demothree_age_num,data=ind_lmc,notch=TRUE)
# title(ylab='IconicPlacesSPecies',xlab='age')
# 
# boxplot(IconicPlacesSPecies~rural,data=ind_lmc,notch=TRUE)
# title(ylab='IconicPlacesSPecies')
# 
# boxplot(IconicPlacesSPecies~demothree_householdsize,data=ind_lmc,notch=TRUE)
# title(ylab='IconicPlacesSPecies',xlab='household size')
# 
# 
# 
# #### CleanWaters ####
# boxplot(CleanWaters~seafood_eat,data=ind_lmc,notch=TRUE)
# title(ylab='CleanWaters',xlab='Seafood eating frequency (yr^-1)')
# 
# 
# 
# 
# names(ind_lmc)

