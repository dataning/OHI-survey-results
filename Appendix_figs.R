library(multcomp)

# boxplots(paste0('figures/fA',i,'_',alternativeNames[i],'.jpg'),var=alternativeNames[i],varlabel=GoalNames[i],max_h=10,width=full,ncol=3,res,qual)


boxplots <- function(filename,var,varlabel,max_h,width,ncol,res,qual){
  fitaov <- aov(as.formula(paste(var,"~",paste(predictors,collapse=" + "))),data=ind_lmc)
  fitglm <- summary(glm(as.formula(paste("FoodProvision~",paste(predictors,collapse=" + "))),data=ind_lmc))$coefficients
  
  p <- summary(fitaov)[[1]]$`Pr(>F)`
  significant <- p<0.05
  n_fig <- sum(significant,na.rm=TRUE)
  jpeg(paste(filename),height=max_h/4*ceiling(n_fig/3),width=width,units = "in",res=res,qual=qual)
  layout(matrix(c(1:(ncol*ceiling(n_fig/3))),ncol=ncol,byrow = T))
  par(mar=c(5,4,1,1))
  
  #province
  if(significant[1]){
    boxplot(get(var)~sample,data=ind_lmc)
    title(ylab=varlabel)
    x <- as.data.frame(cld(glht(fitaov,linfct=mcp(sample="Tukey")))$mcletters$Letters)
    axis(3,c(1:nrow(x)),labels=as.character(x[,1]),tick=FALSE,padj=1.5)
  }
  #ocean_days
  if(significant[2]){
    pvalue <- round(fitglm[row.names(fitglm)=="ocean_days",4],3) 
    if(pvalue==0) pvalue <- "<0.001" 
    if(pvalue<=0.05){
      boxplot(get(var)~ocean_days,data=ind_lmc)
      title(ylab=varlabel,xlab='Number of Ocean Days')
      mtext(paste("S = ",signif(fitglm[row.names(fitglm)=="ocean_days",1],2),
                  "SE =",signif(fitglm[row.names(fitglm)=="ocean_days",2],2),
                  "p =",pvalue),side=3,cex=0.6)
    }
  }
  #seafood_eat
  if(significant[3]){
    pvalue <- round(fitglm[row.names(fitglm)=="seafood_eat",4],3) 
    if(pvalue==0) pvalue <- "<0.001"
    if(pvalue<=0.05){
      boxplot(get(var)~seafood_eat,data=ind_lmc,las=3)
      title(ylab=varlabel,xlab='Seafood eating frequency (yr^-1)')
      mtext(paste("S = ",signif(fitglm[row.names(fitglm)=="seafood_eat",1],2),
                  "SE =",signif(fitglm[row.names(fitglm)=="seafood_eat",2],2),
                  "p =",pvalue),side=3,cex=0.6)
    }
  }
  #gender
  if(significant[4]){
    boxplot(get(var)~as.character(demoone_gender),data=ind_lmc[ind_lmc$demoone_gender!="",])
    title(ylab=varlabel)
    x <- as.data.frame(cld(glht(fitaov,linfct=mcp(demoone_gender="Tukey")))$mcletters$Letters)
    axis(3,c(1:nrow(x)),labels=as.character(x[,1]),tick=FALSE,padj=1.5)
  }
  #envr_org
  if(significant[5]){
    boxplot(get(var)~as.character(envr_org),data=ind_lmc,names=c('No','Yes'))
    title(ylab=varlabel,xlab='ENGO Membership')
    x <- as.data.frame(cld(glht(fitaov,linfct=mcp(envr_org="Tukey")))$mcletters$Letters)
    axis(3,c(1:nrow(x)),labels=as.character(x[,1]),tick=FALSE,padj=1.5)
  }
  #envr_protest
  if(significant[6]){
    boxplot(get(var)~as.character(envr_protest),data=ind_lmc,names=c('No','Yes'))
    title(ylab=varlabel,xlab='Enviromental Protest')
    x <- as.data.frame(cld(glht(fitaov,linfct=mcp(envr_protest="Tukey")))$mcletters$Letters)
    axis(3,c(1:nrow(x)),labels=as.character(x[,1]),tick=FALSE,padj=1.5)
  }
  #income
  if(significant[7]){
    pvalue <- round(fitglm[row.names(fitglm)=="demothree_income2",4],3) 
    if(pvalue==0) pvalue <- "<0.001" 
    if(pvalue<=0.05){
      boxplot(get(var)~demothree_income3,data=ind_lmc)
      title(ylab=varlabel,xlab='Income (Thousands CAD)')
      mtext(paste("S = ",signif(fitglm[row.names(fitglm)=="demothree_income2",1],2),
                  "SE =",signif(fitglm[row.names(fitglm)=="demothree_income2",2],2),
                  "p =",pvalue),side=3,cex=0.6)
    }
  }
  #demothree_householdsize
  if(significant[8]){
    pvalue <- round(fitglm[row.names(fitglm)=="demothree_householdsize",4],3) 
    if(pvalue==0) pvalue <- "<0.001" 
    if(pvalue<=0.05){
      boxplot(get(var)~demothree_householdsize,data=ind_lmc)
      title(ylab=varlabel,xlab='Household size')
      mtext(paste("S = ",signif(fitglm[row.names(fitglm)=="demothree_householdsize",1],2),
                  "SE =",signif(fitglm[row.names(fitglm)=="demothree_householdsize",2],2),
                  "p =",pvalue),side=3,cex=0.6)
    }
  }
  #rural
  if(significant[9]){
    boxplot(get(var)~rural,data=ind_lmc)
    title(ylab=varlabel)
    x <- as.data.frame(cld(glht(fitaov,linfct=mcp(rural="Tukey")))$mcletters$Letters)
    axis(3,c(1:nrow(x)),labels=as.character(x[,1]),tick=FALSE,padj=1.5)
  }
  #distance_FSA
  if(significant[10]){
    pvalue <- round(fitglm[row.names(fitglm)=="distance_FSA",4],3) 
    if(pvalue==0) pvalue <- "<0.001" 
    if(pvalue<=0.05){
      boxplot(get(var)~distance_FSA2,data=ind_lmc)
      title(ylab=varlabel,xlab='Distance to coast')
      mtext(paste("S = ",signif(fitglm[row.names(fitglm)=="distance_FSA",1],2),
                  "SE =",signif(fitglm[row.names(fitglm)=="distance_FSA",2],2),
                  "p =",pvalue),side=3,cex=0.6)
    }
  }
  #rec
  if(significant[11]){
    pvalue <- round(fitglm[row.names(fitglm)=="rec",4],3) 
    if(pvalue==0) pvalue <- "<0.001" 
    if(pvalue<=0.05){
      boxplot(get(var)~rec2,data=ind_lmc,names=c("0","1-4","5-8"))
      title(ylab=varlabel,xlab='Number of Rec. Act.')
      mtext(paste("S = ",signif(fitglm[row.names(fitglm)=="rec",1],2),
                  "SE =",signif(fitglm[row.names(fitglm)=="rec",2],2),
                  "p =",pvalue),side=3,cex=0.6)
    }
  }
  #job
  if(significant[12]){
    boxplot(get(var)~jobs2,data=ind_lmc,names=c('Conser.','Extract.','None'))
    title(ylab=varlabel,xlab='Type of job')
    x <- as.data.frame(cld(glht(fitaov,linfct=mcp(jobs2="Tukey")))$mcletters$Letters)
    axis(3,c(1:nrow(x)),labels=as.character(x[,1]),tick=FALSE,padj=1.5)
  }
  #education
  if(significant[13]){
    boxplot(get(var)~demoone_education2,data=ind_lmc,names=c('Unknown','K-12','PS'))
    title(ylab=varlabel,xlab='Education')
    x <- as.data.frame(cld(glht(fitaov,linfct=mcp(demoone_education2="Tukey")))$mcletters$Letters)
    axis(3,c(1:nrow(x)),labels=as.character(x[,1]),tick=FALSE,padj=1.5)
  }
  #age
  if(significant[14]){
    pvalue <- round(fitglm[row.names(fitglm)=="demothree_age_num",4],3) 
    if(pvalue==0) pvalue <- "<0.001"
    if(pvalue<=0.05){
      boxplot(get(var)~demothree_age_num,names=c("20-24","22-34","35-54","35-44","55-64","65+"),las=3,data=ind_lmc)
      title(ylab=varlabel)
      mtext(paste("S = ",signif(fitglm[row.names(fitglm)=="demothree_age_num",1],2),
                  "SE =",signif(fitglm[row.names(fitglm)=="demothree_age_num",2],2),
                  "p =",pvalue),side=3,cex=0.6)
    }
  }
  #Political party
  if(significant[15]){
    boxplot(get(var)~political_party2,data=ind_lmc,names=c('CPC','LIB','N/G','Other'))
    title(ylab=varlabel)
    x <- as.data.frame(cld(glht(fitaov,linfct=mcp(political_party2="Tukey")))$mcletters$Letters)
    axis(3,c(1:nrow(x)),labels=as.character(x[,1]),tick=FALSE,padj=1.5)
  }

  
  dev.off()
}
for(i in 1:10){
  boxplots(paste0('figures/fA',i,'_',alternativeNames[i],'.jpg'),alternativeNames[i],GoalNames[i],10,full,3,res,qual)
}
