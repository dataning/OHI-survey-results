userstats_BC_subsample <- read.csv("~/Documents/R/OHI survey results/userstats_BC_subsample.csv")
userstats_english <- read.csv("~/Documents/R/OHI survey results/userstats_english.csv")
userstats_French <- read.csv("~/Documents/R/OHI survey results/userstats_French.csv")

## identify un-usable records #######
#remove incompletes
userstats_BC_subsample=userstats_BC_subsample[userstats_BC_subsample$completed=="y",]
userstats_english=userstats_english[userstats_english$completed=="y",]
userstats_French=userstats_French[userstats_French$completed=="y",]

# remove 0's and NA's
i=which(names(userstats_BC_subsample)=="DCE_pg1"|names(userstats_BC_subsample)=="DCE_most10")

userstats_BC_subsample$zeros=0
userstats_BC_subsample$zeros[which(userstats_BC_subsample[i[1]:i[2]]==0,arr.ind=T)[,1]]=1

userstats_BC_subsample$NAs=0
userstats_BC_subsample$NAs[which(is.na(userstats_BC_subsample[i[1]:i[2]]),arr.ind=T)[,1]]=1

#userstats_BC_subsample=userstats_BC_subsample[-which(userstats_BC_subsample[i[1]:i[2]]==0,arr.ind=T)[,1],]
#userstats_BC_subsample=userstats_BC_subsample[-which(is.na(userstats_BC_subsample[i[1]:i[2]]),arr.ind=T)[,1],]
userstats_english=userstats_english[-which(userstats_english[i[1]:i[2]]==0,arr.ind=T)[,1],]
#userstats_english=userstats_english[-which(is.na(userstats_english[i[1]:i[2]]),arr.ind=T)[,1],]
userstats_French=userstats_French[-which(userstats_French[i[1]:i[2]]==0,arr.ind=T)[,1],]
#userstats_French=userstats_French[-which(is.na(userstats_French[i[1]:i[2]]),arr.ind=T)[,1],]
data=userstats_english
#survey completion time
library(chron)
find.time=function(data){
  data2=data
  data2$keep=0
  data2$time=(as.numeric(chron(times=data2$time_out))-as.numeric(chron(times=data2$time_in)))*24*60
  data2$time[is.na(data2$time)]=0
  data2$time[data2$time<0]=data2$time[data2$time<0]+1440
  data2$keep[data2$time<5|data2$time>60]=1
  print(sum(data2$keep))
  #return(data[data2$keep==0,])
  return(data2)
}

userstats_BC_subsample=find.time(userstats_BC_subsample)
userstats_english=find.time(userstats_english)
userstats_French=find.time(userstats_French)


# repeat answers
library(plyr)
card_key <- read.csv("~/Documents/R/OHI survey results/card_key.csv") 
card_key=arrange(card_key,card,goal)

find.rep=function(data){
  i=which(names(data)=="DCE_pg1"|names(data)=="DCE_most10")
  data2=data[,i[1]:i[2]]
  data2$repeated=0
  mosts=data2[,1:10*3]
  leasts=data2[,1:10*3-1]
  pgs=data2[,1:10*3-2]
  for(i in 1:dim(data2)[1]){
    m=rep(0,10)
    l=m
    if(sum(is.na(mosts[i,]))==0&sum(mosts[i,]==0)==0){
      for(j in 1:10){
        m[j]=which(card_key$goal[card_key$card==pgs[i,j]]==mosts[i,j])
        l[j]=which(card_key$goal[card_key$card==pgs[i,j]]==leasts[i,j])
      }
      if(max(hist(m,breaks=c(0:9))$counts)>=8|max(hist(l,breaks=c(0:9))$counts)>=8)
        data2$repeated[i]=1
    }
  }
  print(sum(data2$repeated))
  #return(data[data2$repeated==0,])
  return(cbind(data,data2$repeated))
}

userstats_BC_subsample=find.rep(userstats_BC_subsample)
userstats_english=find.rep(userstats_english)
userstats_French=find.rep(userstats_French)

write.csv(userstats_BC_subsample,"~/Documents/R/OHI survey results/userstats_BC_subsample_indexed.csv")


## demographics ################################################################################
En=table(userstats_english$sample[userstats_english$completed=="y"])
Fr=table(userstats_French$sample[userstats_French$completed=="y"])
BC=table(userstats_BC_subsample$sample[userstats_english$completed=="y"])
En=as.data.frame(En);names(En)[2]="Freq_En"
Fr=as.data.frame(Fr);names(Fr)[2]="Freq_Fr"
BC=as.data.frame(BC);names(BC)[2]="Freq_BC"
merge(as.data.frame(En),merge(as.data.frame(Fr),as.data.frame(BC),all=T),all=T)

En=table(userstats_english$demoone_gender[userstats_english$completed=="y"])
Fr=table(userstats_French$demoone_gender[userstats_French$completed=="y"])
BC=table(userstats_BC_subsample$demoone_gender[userstats_english$completed=="y"])
En=as.data.frame(En);names(En)[2]="Freq_En"
Fr=as.data.frame(Fr);names(Fr)[2]="Freq_Fr"
BC=as.data.frame(BC);names(BC)[2]="Freq_BC"
merge(as.data.frame(En),merge(as.data.frame(Fr),as.data.frame(BC),all=T),all=T)

En=table(userstats_english$demotwo_livedCanada[userstats_english$completed=="y"])
Fr=table(userstats_French$demotwo_livedCanada[userstats_French$completed=="y"])
BC=table(userstats_BC_subsample$demotwo_livedCanada[userstats_english$completed=="y"])
En=as.data.frame(En);names(En)[2]="Freq_En"
Fr=as.data.frame(Fr);names(Fr)[2]="Freq_Fr"
BC=as.data.frame(BC);names(BC)[2]="Freq_BC"
merge(as.data.frame(En),merge(as.data.frame(Fr),as.data.frame(BC),all=T),all=T)

En=table(userstats_english$demoone_education[userstats_english$completed=="y"])
Fr=table(userstats_French$demoone_education[userstats_French$completed=="y"])
BC=table(userstats_BC_subsample$demoone_education[userstats_english$completed=="y"])
En=as.data.frame(En);names(En)[2]="Freq_En"
Fr=as.data.frame(Fr);names(Fr)[2]="Freq_Fr"
BC=as.data.frame(BC);names(BC)[2]="Freq_BC"
merge(as.data.frame(En),merge(as.data.frame(Fr),as.data.frame(BC),all=T),all=T)

En=table(userstats_english$demothree_age[userstats_english$completed=="y"])
Fr=table(userstats_French$demothree_age[userstats_French$completed=="y"])
BC=table(userstats_BC_subsample$demothree_age[userstats_BC_subsample$completed=="y"])
En=as.data.frame(En);names(En)[2]="Freq_En"
Fr=as.data.frame(Fr);names(Fr)[2]="Freq_Fr"
BC=as.data.frame(BC);names(BC)[2]="Freq_BC"
merge(as.data.frame(En),merge(as.data.frame(Fr),as.data.frame(BC),all=T),all=T)

En=table(userstats_english$demoone_education[userstats_english$completed=="y"])
Fr=table(userstats_French$demoone_education[userstats_French$completed=="y"])
BC=table(userstats_BC_subsample$demoone_education[userstats_BC_subsample$completed=="y"])
En=as.data.frame(En);names(En)[2]="Freq_En"
Fr=as.data.frame(Fr);names(Fr)[2]="Freq_Fr"
BC=as.data.frame(BC);names(BC)[2]="Freq_BC"
merge(as.data.frame(En),merge(as.data.frame(Fr),as.data.frame(BC),all=T),all=T)

En=table(userstats_english$demothree_income[userstats_english$completed=="y"])
Fr=table(userstats_French$demothree_income[userstats_French$completed=="y"])
BC=table(userstats_BC_subsample$demothree_income[userstats_BC_subsample$completed=="y"])
En=as.data.frame(En);names(En)[2]="Freq_En"
Fr=as.data.frame(Fr);names(Fr)[2]="Freq_Fr"
BC=as.data.frame(BC);names(BC)[2]="Freq_BC"
merge(as.data.frame(En),merge(as.data.frame(Fr),as.data.frame(BC),all=T),all=T)

x=data.frame(table(substr(userstats_english$demotwo_fsa,2,2)[userstats_english$completed=="y"]));
En=x$Freq[x$Var1==0]/sum(x$Freq[x$Var1==1|x$Var1==2|x$Var1==3|x$Var1==4|x$Var1==5|x$Var1==6|x$Var1==7|x$Var1==8|x$Var1==9])
x=data.frame(table(substr(userstats_French$demotwo_fsa,2,2)[userstats_French$completed=="y"]));
Fr=x$Freq[x$Var1==0]/sum(x$Freq[x$Var1==1|x$Var1==2|x$Var1==3|x$Var1==4|x$Var1==5|x$Var1==6|x$Var1==7|x$Var1==8|x$Var1==9])
x=data.frame(table(substr(userstats_BC_subsample$demotwo_fsa,2,2)[userstats_BC_subsample$completed=="y"]));
BC=x$Freq[x$Var1==0]/sum(x$Freq[x$Var1==1|x$Var1==2|x$Var1==3|x$Var1==4|x$Var1==5|x$Var1==6|x$Var1==7|x$Var1==8|x$Var1==9])
En=as.data.frame(En)
Fr=as.data.frame(Fr)
BC=as.data.frame(BC)
merge(as.data.frame(En),merge(as.data.frame(Fr),as.data.frame(BC),all=T),all=T)

