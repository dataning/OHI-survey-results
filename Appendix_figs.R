boxplots <- function(filename,var,varlabel,max_h,width,ncol,res,qual,aovs){
  p <- aovs[,5]
  significant <- p<0.05
  n_fig <- sum(significant,na.rm=TRUE)
  jpeg(paste(filename),height=max_h/4*ceiling(n_fig/3),width=width,units = "in",res=res,qual=qual)
  layout(matrix(c(1:(ncol*ceiling(n_fig/3))),ncol=ncol,byrow = T))
  par(mar=c(5,4,0,1))
  
  #province
  if(significant[1]){
    boxplot(get(var)~sample,data=ind_lmc)
    title(ylab=varlabel)
  }
  #ocean_days
  if(significant[2]){
    boxplot(get(var)~ocean_days,data=ind_lmc)
    title(ylab=varlabel,xlab='Number of Ocean Days')
  }
  #seafood_eat
  if(significant[3]){
    boxplot(get(var)~seafood_eat,data=ind_lmc)
    title(ylab=varlabel,xlab='Seafood eating frequency (yr^-1)')
  }
  #gender
  if(significant[4]){
    boxplot(get(var)~demoone_gender,data=ind_lmc)
    title(ylab=varlabel)
  }
  #lived in Canada
  if(significant[5]){
    boxplot(get(var)~demotwo_livedCanada,data=ind_lmc)
    title(ylab=varlabel,xlab='Years live in Canada')
  }
  #envr_org
  if(significant[6]){
    boxplot(get(var)~as.character(envr_org),data=ind_lmc,names=c('Unknown','No','Yes'))
    title(ylab=varlabel,xlab='ENGO Membership')
  }
  #envr_protest
  if(significant[7]){
    boxplot(get(var)~as.character(envr_protest),data=ind_lmc,names=c('Unknown','No','Yes'))
    title(ylab=varlabel,xlab='Enviromental Protest')
  }
  #income
  if(significant[8]){
    boxplot(get(var)~demothree_income3,data=ind_lmc)
    title(ylab=varlabel,xlab='Income (Thousands CAD)')
  }
  #demothree_householdsize
  if(significant[9]){
    boxplot(get(var)~demothree_householdsize,data=ind_lmc)
    title(ylab=varlabel,xlab='Household size')
  }
  #rural
  if(significant[10]){
    boxplot(get(var)~rural,data=ind_lmc)
    title(ylab=varlabel)
  }
  #distance_FSA
  if(significant[11]){
    boxplot(get(var)~distance_FSA2,data=ind_lmc)
    title(ylab=varlabel,xlab='Distance to coast')
  }
  #rec
  if(significant[12]){
    boxplot(get(var)~rec2,data=ind_lmc,names=c("0","1-4","5-8"))
    title(ylab=varlabel,xlab='Number of Rec. Act.')
  }
  #job
  if(significant[13]){
    boxplot(get(var)~jobs2,data=ind_lmc,names=c('Conser.','Extract.','None'))
    title(ylab=varlabel,xlab='Type of job')
  }
  #education
  if(significant[14]){
    boxplot(get(var)~demoone_education2,data=ind_lmc,names=c('Unknown','K-12','PS'))
    title(ylab=varlabel,xlab='Education')
  }
  #age
  if(significant[15]){
    boxplot(get(var)~demothree_age_num,names=c("20-24","22-34","35-54","35-44","55-64","65+"),las=3,data=ind_lmc)
    title(ylab=varlabel)
  }
  #education
  if(significant[16]){
    boxplot(get(var)~political_party2,data=ind_lmc,names=c('CPC','LIB','N/G','Other'))
    title(ylab=varlabel,xlab='Education')
  }

  
  dev.off()
}
for(i in 1:10){
  boxplots(paste0('figures/fA',i,'_',alternativeNames[i],'.jpg'),alternativeNames[i],GoalNames[i],10,full,3,res,qual,aovs[[i]])
}
