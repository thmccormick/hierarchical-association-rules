
#evaluation.  we use max of post on never before seen (user.test).  others pick randomly.


part.obs<-function(y,n,n.test,crosstab,nItems,training.tmp,drug.subset,testtrain.vec){
#
n.rules<-(nItems*(nItems-1))+nItems 
#
p.mat.post<-matrix(rec.metro$sims[1,1:(length(n.test)*n.rules)],length(n.test),n.rules)
gamma.post<-rec.metro$sims[1,((length(n.test)*n.rules)+1):((length(n.test)*n.rules)+length(n.test))]
pi.post<-matrix(rec.metro$sims[1,(1+(length(n.test)*n.rules)+length(n.test)):(2*(length(n.test)*n.rules)+length(n.test))],length(n.test),n.rules)

nobs<-(rec.metro$n.iter-rec.metro$n.burn)/rec.metro$n.thin
post.p.mat<-p.mat.post/nobs
gamma.post<-gamma.post/nobs
pi.post<-pi.post/nobs

#function parameters
sim.ind=1
k=9
n.rules=50
#function parameters



#sbst.lwr<-(((as.numeric(args[2])-1)*nItems)+1)
#sbst.upr<-(as.numeric(args[2])*nItems)
#ord<-"mc.v.p, mc.v.beta, mc.v.tbeta,mc.v.gamma,mc.v.tau, #mc.v.mu.beta,mc.v.mu.tbeta,mc.v.mu.gamma,mc.v.mu.tau,mc.v.sigma.beta,mc.v.sigma.tbeta,mc.v.sigma.gamma,mc.v.sigma.tau"
#
#library(arules)
num=length(n.test)
n.users=length(n.test)
n.rec=3
points.hm<-rep(NA,n.users)
points.conf<-rep(NA,n.users)
points.conf25<-rep(NA,n.users)
points.conf5<-rep(NA,n.users)
points.conf1<-rep(NA,n.users)
points.conf2<-rep(NA,n.users)
points.t1<-rep(NA,n.users)
points.t2<-rep(NA,n.users)
#
points.first.hm<-rep(NA,n.users)
points.first.conf<-rep(NA,n.users)
points.first.conf25<-rep(NA,n.users)
points.first.conf5<-rep(NA,n.users)
points.first.conf1<-rep(NA,n.users)
points.first.conf2<-rep(NA,n.users)
points.first.t1<-rep(NA,n.users)
points.first.t2<-rep(NA,n.users)
#
points.last.hm<-rep(NA,n.users)
points.last.conf<-rep(NA,n.users)
points.last.conf25<-rep(NA,n.users)
points.last.conf5<-rep(NA,n.users)
points.last.conf1<-rep(NA,n.users)
points.last.conf2<-rep(NA,n.users)
points.last.t1<-rep(NA,n.users)
points.last.t2<-rep(NA,n.users)
#
basket<-rep(colnames(crosstab)[1],ncol(crosstab))
for(i in 2:nItems){
basket.tmp<-rep(colnames(crosstab)[i],ncol(crosstab))
basket<-c(basket,basket.tmp)}
condit.basket<-rep(colnames(crosstab),ncol(crosstab))
########################
########################
#y and n are the data up to this point
for(tt in 1:n.users){
p.tmp<-post.p.mat[tt,]
pc.tmp<-y[tt,]/(n[tt,])
pc.tmp[is.na(pc.tmp)]<-0
pc25.tmp<-y[tt,]/(n[tt,]+.25)
pc5.tmp<-y[tt,]/(n[tt,]+.5)
pc1.tmp<-y[tt,]/(n[tt,]+1)
pc2.tmp<-y[tt,]/(n[tt,]+2)
t1.tmp<-ifelse(n[tt,]>1,y[tt,]/n[tt,],0)
t2.tmp<-ifelse(n[tt,]>2,y[tt,]/n[tt,],0)
points.tmp.user<-0
attempt.tmp.user<-0
points.conf.tmp<-0
points.conf25.tmp<-0
points.conf5.tmp<-0
points.conf1.tmp<-0
points.conf2.tmp<-0
points.t1.tmp<-0
points.t2.tmp<-0
y.tmp<-y[tt,]
n.tmp<-n[tt,]


first.hm=NA
first.conf=NA
first.conf1=NA
first.conf2=NA
first.conf5=NA
first.conf25=NA
first.t1=NA
first.t2=NA

last.hm=NA
last.conf=NA
last.conf1=NA
last.conf2=NA
last.conf5=NA
last.conf25=NA
last.t1=NA
last.t2=NA
#2 check order of basket and condit.basket
#3 make sure are suggesting both marginally most likely and one-conditionally (now just marginally mabye?)

if(n.test[tt]>0){
###################################
for(rrr in 1:n.test[tt]){
training.tmp.tmp<-training.tmp[tt]
drug.tmp<-drug.subset[which(is.na(match(drug.subset[,1],training.tmp.tmp))==F),]
visit.ord.tmp<-order(unique(drug.tmp$VISITNUM))
visit.ind<-unique(drug.tmp$VISITNUM)[visit.ord.tmp]
visit.test<-visit.ind[-c(1:testtrain.vec[tt])]
drug.tmp<-drug.tmp[is.na(match(drug.tmp$VISITNUM,visit.test))==F,]

hidden.tmp<-order(drug.tmp$REASON[drug.tmp$VISITNUM==unique(drug.tmp$VISITNUM)[order(unique(drug.tmp$VISITNUM))[rrr]]],
drug.tmp$START_DY[drug.tmp$VISITNUM==unique(drug.tmp$VISITNUM)[order(unique(drug.tmp$VISITNUM))[rrr]]])
hidden.tmp<-unique(drug.tmp$REASON[drug.tmp$VISITNUM==unique(drug.tmp$VISITNUM)[order(unique(drug.tmp$VISITNUM))[rrr]]][hidden.tmp])
probe='not there!'
p.tmp.pos<-p.tmp[which(is.na(match(basket,probe))&condit.basket==basket)]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
basket.pos<-basket.pos[order(p.tmp.pos,decreasing=T)]
rec<-unique(basket.pos)[1:n.rec]
points.tmp.user<-ifelse(is.na(match(hidden.tmp[1],rec)),points.tmp.user,points.tmp.user+1)
first.hm<-is.na(match(hidden.tmp[1],rec))
attempt.tmp.user<-1+attempt.tmp.user

#
p.tmp.pos<-pc.tmp[which(is.na(match(basket,probe))&condit.basket==basket)]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
tmp.or<-p.tmp.pos[order(p.tmp.pos,decreasing=T)]
samp<-sample(which(is.na(match(p.tmp.pos,unique(tmp.or[1:n.rec])))==F),n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.conf.tmp<-ifelse(is.na(match(hidden.tmp[1],rec)),points.conf.tmp,points.conf.tmp+1)
first.conf<-is.na(match(hidden.tmp[1],rec))
#
p.tmp.pos<-pc25.tmp[which(is.na(match(basket,probe))&condit.basket==basket)]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
tmp.or<-p.tmp.pos[order(p.tmp.pos,decreasing=T)]
samp<-sample(which(is.na(match(p.tmp.pos,unique(tmp.or[1:n.rec])))==F),n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.conf25.tmp<-ifelse(is.na(match(hidden.tmp[1],rec)),points.conf25.tmp,points.conf25.tmp+1)
first.conf25<-is.na(match(hidden.tmp[1],rec))
#
p.tmp.pos<-pc5.tmp[which(is.na(match(basket,probe))&condit.basket==basket)]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
tmp.or<-p.tmp.pos[order(p.tmp.pos,decreasing=T)]
samp<-sample(which(is.na(match(p.tmp.pos,unique(tmp.or[1:n.rec])))==F),n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.conf5.tmp<-ifelse(is.na(match(hidden.tmp[1],rec)),points.conf5.tmp,points.conf5.tmp+1)
first.conf5<-is.na(match(hidden.tmp[1],rec))
#
p.tmp.pos<-pc1.tmp[which(is.na(match(basket,probe))&condit.basket==basket)]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
tmp.or<-p.tmp.pos[order(p.tmp.pos,decreasing=T)]
samp<-sample(which(is.na(match(p.tmp.pos,unique(tmp.or[1:n.rec])))==F),n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.conf1.tmp<-ifelse(is.na(match(hidden.tmp[1],rec)),points.conf1.tmp,points.conf1.tmp+1)
first.conf1<-is.na(match(hidden.tmp[1],rec))
#
p.tmp.pos<-pc2.tmp[which(is.na(match(basket,probe))&condit.basket==basket)]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
tmp.or<-p.tmp.pos[order(p.tmp.pos,decreasing=T)]
samp<-sample(which(is.na(match(p.tmp.pos,unique(tmp.or[1:n.rec])))==F),n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.conf2.tmp<-ifelse(is.na(match(hidden.tmp[1],rec)),points.conf2.tmp,points.conf2.tmp+1)
first.conf2<-is.na(match(hidden.tmp[1],rec))
#
p.tmp.pos<-t1.tmp[which(is.na(match(basket,probe))&condit.basket==basket)]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
tmp.or<-p.tmp.pos[order(p.tmp.pos,decreasing=T)]
samp<-sample(which(is.na(match(p.tmp.pos,unique(tmp.or[1:n.rec])))==F),n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.t1.tmp<-ifelse(is.na(match(hidden.tmp[1],rec)),points.t1.tmp,points.t1.tmp+1)
first.t1<-points.t1.tmp
#
p.tmp.pos<-t2.tmp[which(is.na(match(basket,probe))&condit.basket==basket)]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
tmp.or<-p.tmp.pos[order(p.tmp.pos,decreasing=T)]
samp<-sample(which(is.na(match(p.tmp.pos,unique(tmp.or[1:n.rec])))==F),n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.t2.tmp<-ifelse(is.na(match(hidden.tmp[1],rec)),points.t2.tmp,points.t2.tmp+1)
first.t2<-is.na(match(hidden.tmp[1],rec))

probe<-c(probe,as.character(hidden.tmp[1]))

if(length(hidden.tmp)>1){
for(r in 2:length(hidden.tmp)){
p.tmp.pos<-p.tmp[which((is.na(match(basket,probe))&is.na(match(condit.basket,probe))==F)|(is.na(match(basket,probe))&condit.basket==basket))]
basket.pos<-basket[which((is.na(match(basket,probe))&is.na(match(condit.basket,probe))==F)|(is.na(match(basket,probe))&condit.basket==basket))]
basket.pos<-basket.pos[order(p.tmp.pos,decreasing=T)]
rec<-unique(basket.pos)[1:n.rec]
points.tmp.user<-ifelse(is.na(match(hidden.tmp[r],rec)),points.tmp.user,points.tmp.user+1)
attempt.tmp.user<-1+attempt.tmp.user
if(r==length(hidden.tmp)){
last.hm<-is.na(match(hidden.tmp[r],rec))}
#
p.tmp.pos<-pc.tmp[which((is.na(match(basket,probe))&is.na(match(condit.basket,probe))==F)|(is.na(match(basket,probe))&condit.basket==basket))]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
tmp.or<-p.tmp.pos[order(p.tmp.pos,decreasing=T)]
samp<-sample(which(is.na(match(p.tmp.pos,unique(tmp.or[1:n.rec])))==F),n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.conf.tmp<-ifelse(is.na(match(hidden.tmp[r],rec)),points.conf.tmp,points.conf.tmp+1)
if(r==length(hidden.tmp)){
last.conf<-is.na(match(hidden.tmp[r],rec))}
#
p.tmp.pos<-pc25.tmp[which((is.na(match(basket,probe))&is.na(match(condit.basket,probe))==F)|(is.na(match(basket,probe))&condit.basket==basket))]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
tmp.or<-p.tmp.pos[order(p.tmp.pos,decreasing=T)]
samp<-sample(which(is.na(match(p.tmp.pos,unique(tmp.or[1:n.rec])))==F),n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.conf25.tmp<-ifelse(is.na(match(hidden.tmp[r],rec)),points.conf25.tmp,points.conf25.tmp+1)
if(r==length(hidden.tmp)){
last.conf25<-is.na(match(hidden.tmp[r],rec))}
#
p.tmp.pos<-pc5.tmp[which((is.na(match(basket,probe))&is.na(match(condit.basket,probe))==F)|(is.na(match(basket,probe))&condit.basket==basket))]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
tmp.or<-p.tmp.pos[order(p.tmp.pos,decreasing=T)]
samp<-sample(which(is.na(match(p.tmp.pos,unique(tmp.or[1:n.rec])))==F),n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.conf5.tmp<-ifelse(is.na(match(hidden.tmp[r],rec)),points.conf5.tmp,points.conf5.tmp+1)
if(r==length(hidden.tmp)){
last.conf25<-is.na(match(hidden.tmp[r],rec))}
#
p.tmp.pos<-pc1.tmp[which((is.na(match(basket,probe))&is.na(match(condit.basket,probe))==F)|(is.na(match(basket,probe))&condit.basket==basket))]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
tmp.or<-p.tmp.pos[order(p.tmp.pos,decreasing=T)]
samp<-sample(which(is.na(match(p.tmp.pos,unique(tmp.or[1:n.rec])))==F),n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.conf1.tmp<-ifelse(is.na(match(hidden.tmp[r],rec)),points.conf1.tmp,points.conf1.tmp+1)
if(r==length(hidden.tmp)){
last.conf1<-is.na(match(hidden.tmp[r],rec))}
#
p.tmp.pos<-pc2.tmp[which((is.na(match(basket,probe))&is.na(match(condit.basket,probe))==F)|(is.na(match(basket,probe))&condit.basket==basket))]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
tmp.or<-p.tmp.pos[order(p.tmp.pos,decreasing=T)]
samp<-sample(which(is.na(match(p.tmp.pos,unique(tmp.or[1:n.rec])))==F),n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.conf2.tmp<-ifelse(is.na(match(hidden.tmp[r],rec)),points.conf2.tmp,points.conf2.tmp+1)
if(r==length(hidden.tmp)){
last.conf2<-is.na(match(hidden.tmp[r],rec))}
#
#
p.tmp.pos<-t1.tmp[which(is.na(match(basket,probe))&condit.basket==basket)]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
tmp.or<-p.tmp.pos[order(p.tmp.pos,decreasing=T)]
samp<-sample(which(is.na(match(p.tmp.pos,unique(tmp.or[1:n.rec])))==F),n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.t1.tmp<-ifelse(is.na(match(hidden.tmp[r],rec)),points.t1.tmp,points.t1.tmp+1)
if(r==length(hidden.tmp)){
last.t1<-is.na(match(hidden.tmp[r],rec))}
#
p.tmp.pos<-t2.tmp[which(is.na(match(basket,probe))&condit.basket==basket)]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
tmp.or<-p.tmp.pos[order(p.tmp.pos,decreasing=T)]
samp<-sample(which(is.na(match(p.tmp.pos,unique(tmp.or[1:n.rec])))==F),n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.t2.tmp<-ifelse(is.na(match(hidden.tmp[r],rec)),points.t2.tmp,points.t2.tmp+1)
if(r==length(hidden.tmp)){
last.t2<-is.na(match(hidden.tmp[r],rec))}
#
#
probe<-c(probe,as.character(hidden.tmp[r]))}}


tmp.mat<-matrix(0,1,length(unique(drug.subset[,7])))
colnames(tmp.mat)<-levels(drug.subset[,7])
tmp.mat[1,match(hidden.tmp,colnames(tmp.mat))]<-1
crosstab<-crossTable(as(tmp.mat,"itemMatrix"))
y.tmp<-y.tmp+as.vector(crosstab)

n.vec<-diag(crosstab)
n.mat.temp<-n.vec
for(i in 2:nItems){
    n.mat.temp<-c(n.mat.temp,n.vec)}
n.tmp<-n.tmp+n.mat.temp

#n.vec<-rep(diag(crosstab)[1],nItems)
#for(j in 2:nItems){
#n.t<-rep(diag(crosstab)[j],nItems)
#n.vec<-c(n.vec,n.t)
#}
#n.tmp<-n.tmp+n.vec

#print(dim(n.tmp))

p.tmp<-(y.tmp+log(pi.post[tt,]))/(n.tmp+log(pi.post[tt,])+gamma.post[tt])

pc.tmp<-y.tmp/(n.tmp)
pc.tmp[is.na(pc.tmp)]<-0
pc25.tmp<-y.tmp/(n.tmp+.25)
pc5.tmp<-y.tmp/(n.tmp+.5)
pc1.tmp<-y.tmp/(n.tmp+1)
pc2.tmp<-y.tmp/(n.tmp+2)
t1.tmp<-ifelse(n[tt,]>1,y[tt,]/n[tt,],0)
t2.tmp<-ifelse(n[tt,]>2,y[tt,]/n[tt,],0)}}

if(n.test[tt]<1){
    points.tmp.user=0
    points.conf.tmp=0
    points.conf25.tmp=0
    points.conf5.tmp=0
    points.conf1.tmp=0
    points.conf2.tmp=0
    points.t1.tmp=0
    points.t2.tmp=0
    }

#points.hm[tt]<-points.tmp.user/max(attempt.tmp.user,1)
#points.conf[tt]<-points.conf.tmp/max(attempt.tmp.user,1)
#points.conf25[tt]<-points.conf25.tmp/max(attempt.tmp.user,1)
#points.conf5[tt]<-points.conf5.tmp/max(attempt.tmp.user,1)
#points.conf1[tt]<-points.conf1.tmp/max(attempt.tmp.user,1)
#points.conf2[tt]<-points.conf2.tmp/max(attempt.tmp.user,1)

points.hm[tt]<-points.tmp.user
points.conf[tt]<-points.conf.tmp
points.conf25[tt]<-points.conf25.tmp
points.conf5[tt]<-points.conf5.tmp
points.conf1[tt]<-points.conf1.tmp
points.conf2[tt]<-points.conf2.tmp
points.t1[tt]<-points.t1.tmp
points.t2[tt]<-points.t2.tmp
points.first.hm[tt]<-first.hm
points.last.hm[tt]<-last.hm
points.first.conf[tt]<-first.conf
points.last.conf[tt]<-last.conf
points.first.conf1[tt]<-first.conf1
points.last.conf1[tt]<-last.conf1
points.first.conf2[tt]<-first.conf2
points.last.conf2[tt]<-last.conf2
points.first.conf25[tt]<-first.conf25
points.last.conf25[tt]<-last.conf25
points.first.conf5[tt]<-first.conf5
points.last.conf5[tt]<-last.conf5
points.first.t1[tt]<-first.t1
points.last.t1[tt]<-last.t1
points.first.t2[tt]<-first.t2
points.last.t2[tt]<-last.t2
}
score.hm<-sum(points.hm,na.rm=T)/sum(n.test)
score.conf<-sum(points.conf,na.rm=T)/sum(n.test)
score.conf25<-sum(points.conf25,na.rm=T)/sum(n.test)
score.conf1<-sum(points.conf1,na.rm=T)/sum(n.test)
score.conf5<-sum(points.conf5,na.rm=T)/sum(n.test)
score.conf2<-sum(points.conf2,na.rm=T)/sum(n.test)
score.t1<-sum(points.t1,na.rm=T)/sum(n.test)
score.t2<-sum(points.t2,na.rm=T)/sum(n.test)
score.fhm<-sum(points.first.hm,na.rm=T)/sum(n.test>0)
score.lhm<-sum(points.last.hm,na.rm=T)/sum(n.test>1)
score.fconf<-sum(points.first.conf,na.rm=T)/sum(n.test>0)
score.lconf<-sum(points.last.conf,na.rm=T)/sum(n.test>1)
score.fconf1<-sum(points.first.conf1,na.rm=T)/sum(n.test>0)
score.lconf1<-sum(points.last.conf1,na.rm=T)/sum(n.test>1)
score.fconf2<-sum(points.first.conf2,na.rm=T)/sum(n.test>0)
score.lconf2<-sum(points.last.conf2,na.rm=T)/sum(n.test>1)
score.fconf25<-sum(points.first.conf25,na.rm=T)/sum(n.test>0)
score.lconf25<-sum(points.last.conf25,na.rm=T)/sum(n.test>1)
score.fconf5<-sum(points.first.conf5,na.rm=T)/sum(n.test>0)
score.lconf5<-sum(points.last.conf5,na.rm=T)/sum(n.test>1)
score.ft1<-sum(points.first.t1,na.rm=T)/sum(n.test>0)
score.lt1<-sum(points.last.t1,na.rm=T)/sum(n.test>1)
score.ft2<-sum(points.first.t2,na.rm=T)/sum(n.test>0)
score.lt2<-sum(points.last.t2,na.rm=T)/sum(n.test>1)
num=n.test

return(list(score.hm=score.hm,score.conf=score.conf,score.conf25=score.conf25,score.conf1=score.conf1,score.conf5=score.conf5,score.conf2=score.conf2,score.t1=score.t1,score.t2=score.t2,
score.fhm=score.fhm,
score.lhm=score.lhm,
score.fconf=score.fconf,
score.lconf=score.lconf,
score.fconf1=score.fconf1,
score.lconf1=score.lconf1,
score.fconf2=score.fconf2,
score.lconf2=score.lconf2,
score.fconf5=score.fconf5,
score.lconf5=score.lconf5,
score.fconf25=score.fconf25,
score.lconf25=score.lconf25,
score.ft1=score.ft1,
score.lt1=score.lt1,
score.ft2=score.ft2,
score.lt2=score.lt2,
num=num))
}

