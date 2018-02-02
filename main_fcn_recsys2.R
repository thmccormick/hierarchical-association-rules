
rec.par<-function(runnum,setnum,utrainpar=200,
					utestpar=30,
					nvisitpar=n.visit.all,
					userspar=users,
					drug.subsetpar=drug.subset,
					x.matrix.allpar=x.matrix.all,
				iter=5000,chain=1,thin=10,burn=1000,mainargs=NULL){
#begin function

#set seed for different results
for(ddd in 1:(setnum*2)){
set.seed(abs(round(rnorm(1,runnum*(setnum+1500),400))))}

prep<<-testtrain(npartuser=utrainpar,nnewuser=utestpar,
		n.visit.tt=nvisitpar,
		users.tt=userspar,
		drug.subset.tt=drug.subsetpar,
		x.matrix.all.tt=x.matrix.allpar,args=mainargs)


rec.metro<<-f.metro(prep$y,prep$n,x.mat=prep$x.matrix.subset,n.iter=iter,n.chain=chain,n.thin=thin,n.burn=burn)

save.image(paste("rrules_",setnum,"_",runnum,".RData",sep='')) 

source("encounter_eval_newpt2.R")
source("encounter_eval_meanonly.R")

part.tmp<<-part.obs(y=prep$y,n=prep$n,n.test=prep$n.test,
					nItems=prep$nItems,
					training.tmp=prep$training.tmp,
					drug.subset=drug.subset,
					crosstab=prep$crosstab,
					testtrain.vec=prep$testtrain.vec)
					
no.tmp<<-no.obs(drug.subset=drug.subset,
				user.test=prep$user.test,
				crosstab=prep$crosstab,
				n.test=prep$n.test,
				x.matrix.subset=prep$x.matrix.subset,
				x.matrix.test=prep$x.matrix.test
				)
  
return(list(evalpart=part.tmp,
			evalno=no.tmp,
		   utestpar=prep$user.test,
		   usubpar=prep$user.subset,
		   nitempar=prep$nItems))}
 
#return(list(part=part.tmp,no=no.tmp))} 



testtrain<-function(npartuser=200,nnewuser=30,
		n.visit.tt=n.visit,
		users.tt=users,
		drug.subset.tt=drug.subset,
		x.matrix.all.tt=x.matrix.all,args=NULL){
###########################################################
#splits the data for testing and training
#prepares the needed matrices for the mcmc function
#output y, n or support and conf and matrix of covariates for both test and training(new) indiviudals
###########################################################
user.subset<-rep(NA,length(users.tt))
user.test<-rep(NA,length(users.tt))
for(i in 1:length(user.subset)){
user.subset[i]<-ifelse(rbinom(1,1,npartuser/length(users.tt)),1,0)
user.test[i]<-ifelse(rbinom(1,1,nnewuser/length(users.tt)),1,0)
}
setind<-args
user.subset<-read.csv("badruns.csv")
user.subset<-user.subset[-1,]
user.subset<-user.subset[setind,]
user.test[user.subset==1]<-0
n.visit<-n.visit.tt[user.subset==1]

testtrain.vec<-rep(NA,length(n.visit))
for(i in 1:length(testtrain.vec)){
testtrain.vec[i]<-round(runif(1,1,n.visit[i]),0)
}
n.test<-n.visit-testtrain.vec
nItems=length(levels(drug.subset.tt[,7]))
n.rules<-(nItems*(nItems-1))+nItems 
y.mat<-matrix(NA,sum(user.subset),n.rules)
n.mat<-matrix(NA,sum(user.subset),nItems)
#training #training #training
#training #training #training
#training #training #training
#training #training #training
#training #training #training
#training #training #training
training.tmp<-unique(drug.subset.tt[,1])[user.subset>0]
for(u in 1:length(n.visit)){
training.tmp.tmp<-training.tmp[u]
drug.tmp<-drug.subset.tt[which(is.na(match(drug.subset.tt[,1],training.tmp.tmp))==F),]
visit.ord.tmp<-order(unique(drug.tmp$VISITNUM))
visit.ind<-unique(drug.tmp$VISITNUM)[visit.ord.tmp]
visit.test<-visit.ind[1:testtrain.vec[u]]
drug.tmp<-drug.tmp[is.na(match(drug.tmp$VISITNUM,visit.test))==F,]
y.tmp<-rep(0,n.rules)
n.tmp<-rep(0,nItems)
for(v in 1:length(visit.test)){
hidden.tmp<-unique(drug.tmp[drug.tmp$VISITNUM==visit.test[v],7])
tmp.mat<-matrix(0,1,length(unique(drug.subset.tt[,7])))
colnames(tmp.mat)<-levels(drug.subset.tt[,7])
tmp.mat[1,match(hidden.tmp,colnames(tmp.mat))]<-1
crosstab<-crossTable(as(tmp.mat,"itemMatrix"))
y.tmp<-y.tmp+as.vector(crosstab)
n.vec<-diag(crosstab)
#n.vec<-rep(diag(crosstab)[1],nItems)
#for(j in 2:nItems){
#n.t<-rep(diag(crosstab)[j],nItems)
#n.vec<-c(n.vec,n.t)
#}
n.tmp<-n.tmp+n.vec
}
#n.tmp[n.tmp<1]<-1
y.mat[u,]<-y.tmp
n.mat[u,]<-n.tmp
}

n.mat.temp<-n.mat
for(i in 2:nItems){
	n.mat.temp<-cbind(n.mat.temp,n.mat)}

#for(i in 1:nrow(n.mat)){
#	n.mat[i,]<-rep(unique(n.mat[i]),nItems)}

y<-y.mat
n<-n.mat.temp

x.matrix.subset=x.matrix.all[user.subset>0,]
x.matrix.test=x.matrix.all[user.test>0,]

return(list(testtrain.vec=testtrain.vec,crosstab=crosstab,training.tmp=training.tmp,n.test=n.test,user.test=user.test,user.subset=user.subset,nItems=nItems,y=y,n=n,x.matrix.subset=x.matrix.subset,x.matrix.test=x.matrix.test))
}

	                
#save.image(paste("mean_sims_",args[2],".RData",sep=''))
