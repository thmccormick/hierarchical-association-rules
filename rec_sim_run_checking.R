
##note this fucntion is designed to run on a mosix cluster where commandArgs are specified 
# for each run of the simulation

rm(list=ls(all=TRUE))
#############uncomment to use arguments on cluster 
args = commandArgs(trailing=TRUE)
#args[1] is set number
#args[2] is number of test individuals
#############uncomment to use arguments on cluster 
library(arules)
#library(multicore)

#set.seed(as.numeric(args[1]))
source("encounter.metro_s_meanonly_sim.R")

drug.data<-read.table("pres_covar.txt",header=T)

attach(drug.data)

#top 50 conditions
top50<-names(sort(table(REASON))[(length(unique(REASON))-49):length(unique(REASON))])
subset.ind<-rep(NA,nrow(drug.data))
subset.ind<-ifelse(is.na(match(drug.data$REASON,top50)),0,1)
drug.subset<-drug.data[subset.ind==1,]
drug.subset[,7]<-as.factor(as.character(drug.subset[,7]))


drug<-drug.subset[,c(1,7)]
users<-unique(drug[,1])

n.visit.all=rep(NA,length(unique(drug.subset[,1])))
for(i in 1:length(n.visit.all)){
n.visit.all[i]<-length(unique(drug.subset$VISITNUM[drug.subset[,1]==unique(drug.subset[,1])[i]]))}
#hist(a, main='histogram on number of visits')

#set up indicator variables
age.vec<-rep(NA,length(unique(drug[,1])))
gend.vec<-rep(NA,length(unique(drug[,1])))
race.vec<-rep(NA,length(unique(drug[,1])))
trt.vec<-rep(NA,length(unique(drug[,1])))
for(i in 1:length(unique(drug[,1]))){
age.vec[i]<-drug.subset[drug.subset[,1]==unique(drug[,1])[i],2][1]
gend.vec[i]<-drug.subset[drug.subset[,1]==unique(drug[,1])[i],4][1]
race.vec[i]<-drug.subset[drug.subset[,1]==unique(drug[,1])[i],5][1]
trt.vec[i]<-drug.subset[drug.subset[,1]==unique(drug[,1])[i],6][1]}



#set up x matrix
#gender
#control gp is men
g.ind<-rep(0,length(gend.vec))
g.ind<-ifelse(gend.vec<2,1,0)

#treatment
#control gp is placebo
#trt is either got 25mg or 50mg
trt.ind<-rep(0,length(trt.vec))
trt.ind<-ifelse(trt.vec<3,1,0)

#age
#youngest are control group
age5060<-ifelse(age.vec>50&age.vec<61,1,0)
age6070<-ifelse(age.vec>60&age.vec<71,1,0)
age70<-ifelse(age.vec>70,1,0)

#race
#white is control
racea<-ifelse(race.vec==1,1,0)
raceb<-ifelse(race.vec==2,1,0)
#combine hisp and other b/c of small samp size
raceh<-ifelse(race.vec>2&race.vec<9,1,0)

x.matrix.all=cbind(rep(1,length(g.ind)),g.ind,trt.ind,age5060,age6070,age70,racea,raceb,raceh)

########use functions for parallel processing#########
source("main_fcn_recsys2.R")
#function is rec.par


#mainargs is the line of the list of bad runs to use in the simulation
#same as set number.
recout<-rec.par(1,as.numeric(args[1]),utrainpar=as.numeric(args[2]),iter=8000,burn=3000,thin=50,mainargs=as.numeric(args[1]))
#two<-mcparallel(rec.par(2,as.numeric(args[1]),utrainpar=as.numeric(args[2]),iter=5000,burn=1000,thin=10),name='two')
#three<-mcparallel(rec.par(3,as.numeric(args[1]),utrainpar=as.numeric(args[2]),iter=5000,burn=1000,thin=10),name='three')
#four<-mcparallel(rec.par(4,as.numeric(args[1]),utrainpar=as.numeric(args[2]),iter=5000,burn=1000,thin=10),name='four')

#col<-collect(list(one=one,two=two,three=three,four=four))
save.image(paste("rrules_set_",args[1],".RData",sep=''))

