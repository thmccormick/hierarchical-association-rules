#evaluating how the model does on new patients

no.obs<-function(drug.subset,user.test,crosstab,n.test,x.matrix.subset,x.matrix.test){
n.visit=rep(NA,length(unique(drug.subset[,1])))
for(i in 1:length(n.visit)){
n.visit[i]<-length(unique(drug.subset$VISITNUM[drug.subset[,1]==unique(drug.subset[,1])[i]]))}
#hist(a, main='histogram on number of visits')
n.visit<-n.visit[user.test==1]
num=sum(user.test==1)
nItems=length(levels(drug.subset[,7]))
n.rules<-(nItems*(nItems-1))+nItems 

p.mat.post<-matrix(rec.metro$sims[1,1:(length(n.test)*n.rules)],length(n.test),n.rules)
gamma.post<-rec.metro$sims[1,((length(n.test)*n.rules)+1):((length(n.test)*n.rules)+length(n.test))]
pi.post<-matrix(rec.metro$sims[1,(1+(length(n.test)*n.rules)+length(n.test)):(2*(length(n.test)*n.rules)+length(n.test))],length(n.test),n.rules)

nobs<-(rec.metro$n.iter-rec.metro$n.burn)/rec.metro$n.thin
post.p.mat<-p.mat.post/nobs
gamma.post<-gamma.post/nobs
pi.post<-pi.post/nobs
testtrain.vec<-rep(0,length(n.visit))
n.test<-n.visit-testtrain.vec


x.matrix.test=cbind(rep(1,length(g.ind[user.test>0])),g.ind[user.test>0],trt.ind[user.test>0],age5060[user.test>0],age6070[user.test>0],age70[user.test>0],racea[user.test>0],raceb[user.test>0],raceh[user.test>0])
x.matrix.test=cbind(rep(1,length(g.ind[user.test>0])),g.ind[user.test>0],trt.ind[user.test>0],age5060[user.test>0],age6070[user.test>0],age70[user.test>0],racea[user.test>0],raceb[user.test>0],raceh[user.test>0])

training.tmp<-unique(drug.subset[,1])[user.test>0]

library(arules)
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
basket<-rep(colnames(crosstab)[1],ncol(crosstab))
for(i in 2:nItems){
basket.tmp<-rep(colnames(crosstab)[i],ncol(crosstab))
basket<-c(basket,basket.tmp)}
condit.basket<-rep(colnames(crosstab),ncol(crosstab))


for(tt in 1:sum(user.test)){
points.tmp.user<-0
attempt.tmp.user<-0
points.conf.tmp<-0
points.conf25.tmp<-0
points.conf5.tmp<-0
points.conf1.tmp<-0
points.conf2.tmp<-0
points.t1.tmp<-0
points.t2.tmp<-0


training.tmp.tmp<-training.tmp[tt]
drug.tmp<-drug.subset[which(is.na(match(drug.subset[,1],training.tmp.tmp))==F),]
visit.ord.tmp<-order(unique(drug.tmp$VISITNUM))
visit.ind<-unique(drug.tmp$VISITNUM)[visit.ord.tmp]
visit.test<-visit.ind[-c(1:testtrain.vec[tt])]
drug.tmp<-drug.tmp[is.na(match(drug.tmp$VISITNUM,visit.test))==F,]

match.ind=sum(x.matrix.subset[,1]==x.matrix.test[tt,1]&
x.matrix.subset[,2]==x.matrix.test[tt,2]&
x.matrix.subset[,3]==x.matrix.test[tt,3]&
x.matrix.subset[,4]==x.matrix.test[tt,4]&
x.matrix.subset[,5]==x.matrix.test[tt,5]&
x.matrix.subset[,6]==x.matrix.test[tt,6]&
x.matrix.subset[,7]==x.matrix.test[tt,7]&
x.matrix.subset[,8]==x.matrix.test[tt,8]&
x.matrix.subset[,9]==x.matrix.test[tt,9])

if(match.ind>1){
m.tmp<-colMeans((pi.post[x.matrix.subset[,1]==x.matrix.test[tt,1]&
x.matrix.subset[,2]==x.matrix.test[tt,2]&
x.matrix.subset[,3]==x.matrix.test[tt,3]&
x.matrix.subset[,4]==x.matrix.test[tt,4]&
x.matrix.subset[,5]==x.matrix.test[tt,5]&
x.matrix.subset[,6]==x.matrix.test[tt,6]&
x.matrix.subset[,7]==x.matrix.test[tt,7]&
x.matrix.subset[,8]==x.matrix.test[tt,8]&
x.matrix.subset[,9]==x.matrix.test[tt,9],]))

p.tmp<-log(m.tmp)/(log(m.tmp)+mean(gamma.post[x.matrix.subset[,1]==x.matrix.test[tt,1]&
x.matrix.subset[,2]==x.matrix.test[tt,2]&
x.matrix.subset[,3]==x.matrix.test[tt,3]&
x.matrix.subset[,4]==x.matrix.test[tt,4]&
x.matrix.subset[,5]==x.matrix.test[tt,5]&
x.matrix.subset[,6]==x.matrix.test[tt,6]&
x.matrix.subset[,7]==x.matrix.test[tt,7]&
x.matrix.subset[,8]==x.matrix.test[tt,8]&
x.matrix.subset[,9]==x.matrix.test[tt,9]]))}

if(match.ind==1){
m.tmp<-((pi.post[x.matrix.subset[,1]==x.matrix.test[tt,1]&
x.matrix.subset[,2]==x.matrix.test[tt,2]&
x.matrix.subset[,3]==x.matrix.test[tt,3]&
x.matrix.subset[,4]==x.matrix.test[tt,4]&
x.matrix.subset[,5]==x.matrix.test[tt,5]&
x.matrix.subset[,6]==x.matrix.test[tt,6]&
x.matrix.subset[,7]==x.matrix.test[tt,7]&
x.matrix.subset[,8]==x.matrix.test[tt,8]&
x.matrix.subset[,9]==x.matrix.test[tt,9],]))

p.tmp<-log(m.tmp)/(log(m.tmp)+(gamma.post[x.matrix.subset[,1]==x.matrix.test[tt,1]&
x.matrix.subset[,2]==x.matrix.test[tt,2]&
x.matrix.subset[,3]==x.matrix.test[tt,3]&
x.matrix.subset[,4]==x.matrix.test[tt,4]&
x.matrix.subset[,5]==x.matrix.test[tt,5]&
x.matrix.subset[,6]==x.matrix.test[tt,6]&
x.matrix.subset[,7]==x.matrix.test[tt,7]&
x.matrix.subset[,8]==x.matrix.test[tt,8]&
x.matrix.subset[,9]==x.matrix.test[tt,9]]))}

if(match.ind<1){
m.tmp<-colMeans(pi.post)
p.tmp<-log(m.tmp)/(log(m.tmp)+mean(gamma.post))}




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
attempt.tmp.user<-1+attempt.tmp.user

#
basket.pos<-colnames(crosstab)
samp<-sample(1:nItems,n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.conf.tmp<-ifelse(is.na(match(hidden.tmp[1],rec)),points.conf.tmp,points.conf.tmp+1)
#
basket.pos<-colnames(crosstab)
samp<-sample(1:nItems,n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pospoints.conf25.tmp<-ifelse(is.na(match(hidden.tmp[1],rec)),points.conf25.tmp,points.conf25.tmp+1)
#
basket.pos<-colnames(crosstab)
samp<-sample(1:nItems,n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pospoints.conf5.tmp<-ifelse(is.na(match(hidden.tmp[1],rec)),points.conf5.tmp,points.conf5.tmp+1)
#
basket.pos<-colnames(crosstab)
samp<-sample(1:nItems,n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.conf1.tmp<-ifelse(is.na(match(hidden.tmp[1],rec)),points.conf1.tmp,points.conf1.tmp+1)
#
basket.pos<-colnames(crosstab)
samp<-sample(1:nItems,n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.conf2.tmp<-ifelse(is.na(match(hidden.tmp[1],rec)),points.conf2.tmp,points.conf2.tmp+1)
#
basket.pos<-colnames(crosstab)
samp<-sample(1:nItems,n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.t1.tmp<-ifelse(is.na(match(hidden.tmp[1],rec)),points.t1.tmp,points.t1.tmp+1)
#
basket.pos<-colnames(crosstab)
samp<-sample(1:nItems,n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.t2.tmp<-ifelse(is.na(match(hidden.tmp[1],rec)),points.t2.tmp,points.t2.tmp+1)
#
probe<-c(probe,as.character(hidden.tmp[1]))


tmp.mat<-matrix(0,1,length(unique(drug.subset[,7])))
colnames(tmp.mat)<-levels(drug.subset[,7])
tmp.mat[1,match(hidden.tmp,colnames(tmp.mat))]<-1
crosstab<-crossTable(as(tmp.mat,"itemMatrix"))
y.tmp<-as.vector(crosstab)

n.vec<-diag(crosstab)
n.mat.temp<-n.vec
for(i in 2:nItems){n.mat.temp<-c(n.mat.temp,n.vec)}
n.tmp<-n.mat.temp

####################
if(match.ind>1){
m.tmp<-colMeans((pi.post[x.matrix.subset[,1]==x.matrix.test[tt,1]&
x.matrix.subset[,2]==x.matrix.test[tt,2]&
x.matrix.subset[,3]==x.matrix.test[tt,3]&
x.matrix.subset[,4]==x.matrix.test[tt,4]&
x.matrix.subset[,5]==x.matrix.test[tt,5]&
x.matrix.subset[,6]==x.matrix.test[tt,6]&
x.matrix.subset[,7]==x.matrix.test[tt,7]&
x.matrix.subset[,8]==x.matrix.test[tt,8]&
x.matrix.subset[,9]==x.matrix.test[tt,9],]))

p.tmp<-(y.tmp+log(m.tmp))/(n.tmp+log(m.tmp)+mean(gamma.post[x.matrix.subset[,1]==x.matrix.test[tt,1]&
x.matrix.subset[,2]==x.matrix.test[tt,2]&
x.matrix.subset[,3]==x.matrix.test[tt,3]&
x.matrix.subset[,4]==x.matrix.test[tt,4]&
x.matrix.subset[,5]==x.matrix.test[tt,5]&
x.matrix.subset[,6]==x.matrix.test[tt,6]&
x.matrix.subset[,7]==x.matrix.test[tt,7]&
x.matrix.subset[,8]==x.matrix.test[tt,8]&
x.matrix.subset[,9]==x.matrix.test[tt,9]]))}

if(match.ind==1){
m.tmp<-((pi.post[x.matrix.subset[,1]==x.matrix.test[tt,1]&
x.matrix.subset[,2]==x.matrix.test[tt,2]&
x.matrix.subset[,3]==x.matrix.test[tt,3]&
x.matrix.subset[,4]==x.matrix.test[tt,4]&
x.matrix.subset[,5]==x.matrix.test[tt,5]&
x.matrix.subset[,6]==x.matrix.test[tt,6]&
x.matrix.subset[,7]==x.matrix.test[tt,7]&
x.matrix.subset[,8]==x.matrix.test[tt,8]&
x.matrix.subset[,9]==x.matrix.test[tt,9],]))

p.tmp<-(y.tmp+log(m.tmp))/(n.tmp+log(m.tmp)+(gamma.post[x.matrix.subset[,1]==x.matrix.test[tt,1]&
x.matrix.subset[,2]==x.matrix.test[tt,2]&
x.matrix.subset[,3]==x.matrix.test[tt,3]&
x.matrix.subset[,4]==x.matrix.test[tt,4]&
x.matrix.subset[,5]==x.matrix.test[tt,5]&
x.matrix.subset[,6]==x.matrix.test[tt,6]&
x.matrix.subset[,7]==x.matrix.test[tt,7]&
x.matrix.subset[,8]==x.matrix.test[tt,8]&
x.matrix.subset[,9]==x.matrix.test[tt,9]]))}

if(match.ind<1){
m.tmp<-colMeans(pi.post)
p.tmp<-(y.tmp+log(m.tmp))/(n.tmp+log(m.tmp)+mean(gamma.post))}
#####################

pc.tmp<-y.tmp/(n.tmp)
pc.tmp[is.na(pc.tmp)]<-0
pc25.tmp<-y.tmp/(n.tmp+.25)
pc5.tmp<-y.tmp/(n.tmp+.5)
pc1.tmp<-y.tmp/(n.tmp+1)
pc2.tmp<-y.tmp/(n.tmp+2)
t1.tmp<-ifelse(n.tmp>1,y.tmp/n.tmp,0)
t2.tmp<-ifelse(n.tmp>2,y.tmp/n.tmp,0)

if(length(hidden.tmp)>1){
for(r in 2:length(hidden.tmp)){
p.tmp.pos<-p.tmp[which((is.na(match(basket,probe))&is.na(match(condit.basket,probe))==F)|(is.na(match(basket,probe))&condit.basket==basket))]
basket.pos<-basket[which((is.na(match(basket,probe))&is.na(match(condit.basket,probe))==F)|(is.na(match(basket,probe))&condit.basket==basket))]
basket.pos<-basket.pos[order(p.tmp.pos,decreasing=T)]
rec<-unique(basket.pos)[1:n.rec]
points.tmp.user<-ifelse(is.na(match(hidden.tmp[r],rec)),points.tmp.user,points.tmp.user+1)
attempt.tmp.user<-1+attempt.tmp.user
#
p.tmp.pos<-pc.tmp[which((is.na(match(basket,probe))&is.na(match(condit.basket,probe))==F)|(is.na(match(basket,probe))&condit.basket==basket))]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
tmp.or<-p.tmp.pos[order(p.tmp.pos,decreasing=T)]
samp<-sample(which(is.na(match(p.tmp.pos,unique(tmp.or[1:n.rec])))==F),n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.conf.tmp<-ifelse(is.na(match(hidden.tmp[r],rec)),points.conf.tmp,points.conf.tmp+1)
#
p.tmp.pos<-pc25.tmp[which((is.na(match(basket,probe))&is.na(match(condit.basket,probe))==F)|(is.na(match(basket,probe))&condit.basket==basket))]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
tmp.or<-p.tmp.pos[order(p.tmp.pos,decreasing=T)]
samp<-sample(which(is.na(match(p.tmp.pos,unique(tmp.or[1:n.rec])))==F),n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.conf25.tmp<-ifelse(is.na(match(hidden.tmp[r],rec)),points.conf25.tmp,points.conf25.tmp+1)
#
p.tmp.pos<-pc5.tmp[which((is.na(match(basket,probe))&is.na(match(condit.basket,probe))==F)|(is.na(match(basket,probe))&condit.basket==basket))]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
tmp.or<-p.tmp.pos[order(p.tmp.pos,decreasing=T)]
samp<-sample(which(is.na(match(p.tmp.pos,unique(tmp.or[1:n.rec])))==F),n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.conf5.tmp<-ifelse(is.na(match(hidden.tmp[r],rec)),points.conf5.tmp,points.conf5.tmp+1)
#
p.tmp.pos<-pc1.tmp[which((is.na(match(basket,probe))&is.na(match(condit.basket,probe))==F)|(is.na(match(basket,probe))&condit.basket==basket))]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
tmp.or<-p.tmp.pos[order(p.tmp.pos,decreasing=T)]
samp<-sample(which(is.na(match(p.tmp.pos,unique(tmp.or[1:n.rec])))==F),n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.conf1.tmp<-ifelse(is.na(match(hidden.tmp[r],rec)),points.conf1.tmp,points.conf1.tmp+1)
#
p.tmp.pos<-pc2.tmp[which((is.na(match(basket,probe))&is.na(match(condit.basket,probe))==F)|(is.na(match(basket,probe))&condit.basket==basket))]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
tmp.or<-p.tmp.pos[order(p.tmp.pos,decreasing=T)]
samp<-sample(which(is.na(match(p.tmp.pos,unique(tmp.or[1:n.rec])))==F),n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.conf2.tmp<-ifelse(is.na(match(hidden.tmp[r],rec)),points.conf2.tmp,points.conf2.tmp+1)
#
p.tmp.pos<-t1.tmp[which((is.na(match(basket,probe))&is.na(match(condit.basket,probe))==F)|(is.na(match(basket,probe))&condit.basket==basket))]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
tmp.or<-p.tmp.pos[order(p.tmp.pos,decreasing=T)]
samp<-sample(which(is.na(match(p.tmp.pos,unique(tmp.or[1:n.rec])))==F),n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.t1.tmp<-ifelse(is.na(match(hidden.tmp[r],rec)),points.t1.tmp,points.t1.tmp+1)
#
p.tmp.pos<-t2.tmp[which((is.na(match(basket,probe))&is.na(match(condit.basket,probe))==F)|(is.na(match(basket,probe))&condit.basket==basket))]
basket.pos<-basket[which(is.na(match(basket,probe))&condit.basket==basket)]
tmp.or<-p.tmp.pos[order(p.tmp.pos,decreasing=T)]
samp<-sample(which(is.na(match(p.tmp.pos,unique(tmp.or[1:n.rec])))==F),n.rec,replace=FALSE)
basket.pos<-basket.pos[samp]
rec<-basket.pos
points.t2.tmp<-ifelse(is.na(match(hidden.tmp[r],rec)),points.t2.tmp,points.t2.tmp+1)
#
probe<-c(probe,as.character(hidden.tmp[r]))}


tmp.mat<-matrix(0,1,length(unique(drug.subset[,7])))
colnames(tmp.mat)<-levels(drug.subset[,7])
tmp.mat[1,match(hidden.tmp,colnames(tmp.mat))]<-1
crosstab<-crossTable(as(tmp.mat,"itemMatrix"))
y.tmp<-y.tmp+as.vector(crosstab)

n.vec<-diag(crosstab)
n.mat.temp<-n.vec
for(i in 2:nItems){n.mat.temp<-c(n.mat.temp,n.vec)}
n.tmp<-n.tmp+n.mat.temp


####################
if(match.ind>1){
m.tmp<-colMeans((pi.post[x.matrix.subset[,1]==x.matrix.test[tt,1]&
x.matrix.subset[,2]==x.matrix.test[tt,2]&
x.matrix.subset[,3]==x.matrix.test[tt,3]&
x.matrix.subset[,4]==x.matrix.test[tt,4]&
x.matrix.subset[,5]==x.matrix.test[tt,5]&
x.matrix.subset[,6]==x.matrix.test[tt,6]&
x.matrix.subset[,7]==x.matrix.test[tt,7]&
x.matrix.subset[,8]==x.matrix.test[tt,8]&
x.matrix.subset[,9]==x.matrix.test[tt,9],]))

p.tmp<-(y.tmp+log(m.tmp))/(n.tmp+log(m.tmp)+mean(gamma.post[x.matrix.subset[,1]==x.matrix.test[tt,1]&
x.matrix.subset[,2]==x.matrix.test[tt,2]&
x.matrix.subset[,3]==x.matrix.test[tt,3]&
x.matrix.subset[,4]==x.matrix.test[tt,4]&
x.matrix.subset[,5]==x.matrix.test[tt,5]&
x.matrix.subset[,6]==x.matrix.test[tt,6]&
x.matrix.subset[,7]==x.matrix.test[tt,7]&
x.matrix.subset[,8]==x.matrix.test[tt,8]&
x.matrix.subset[,9]==x.matrix.test[tt,9]]))}

if(match.ind==1){
m.tmp<-((pi.post[x.matrix.subset[,1]==x.matrix.test[tt,1]&
x.matrix.subset[,2]==x.matrix.test[tt,2]&
x.matrix.subset[,3]==x.matrix.test[tt,3]&
x.matrix.subset[,4]==x.matrix.test[tt,4]&
x.matrix.subset[,5]==x.matrix.test[tt,5]&
x.matrix.subset[,6]==x.matrix.test[tt,6]&
x.matrix.subset[,7]==x.matrix.test[tt,7]&
x.matrix.subset[,8]==x.matrix.test[tt,8]&
x.matrix.subset[,9]==x.matrix.test[tt,9],]))

p.tmp<-(y.tmp+log(m.tmp))/(n.tmp+log(m.tmp)+(gamma.post[x.matrix.subset[,1]==x.matrix.test[tt,1]&
x.matrix.subset[,2]==x.matrix.test[tt,2]&
x.matrix.subset[,3]==x.matrix.test[tt,3]&
x.matrix.subset[,4]==x.matrix.test[tt,4]&
x.matrix.subset[,5]==x.matrix.test[tt,5]&
x.matrix.subset[,6]==x.matrix.test[tt,6]&
x.matrix.subset[,7]==x.matrix.test[tt,7]&
x.matrix.subset[,8]==x.matrix.test[tt,8]&
x.matrix.subset[,9]==x.matrix.test[tt,9]]))}

if(match.ind<1){
m.tmp<-colMeans(pi.post)
p.tmp<-(y.tmp+log(m.tmp))/(n.tmp+log(m.tmp)+mean(gamma.post))}
#####################


pc.tmp<-y.tmp/(n.tmp)
pc.tmp[is.na(pc.tmp)]<-0
pc25.tmp<-y.tmp/(n.tmp+.25)
pc5.tmp<-y.tmp/(n.tmp+.5)
pc1.tmp<-y.tmp/(n.tmp+1)
pc2.tmp<-y.tmp/(n.tmp+2)
t1.tmp<-ifelse(n.tmp>1,y.tmp/n.tmp,0)
t2.tmp<-ifelse(n.tmp>2,y.tmp/n.tmp,0)}}

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
}
score.hm<-sum(points.hm,na.rm=T)/sum(n.test)
score.conf<-sum(points.conf,na.rm=T)/sum(n.test)
score.conf25<-sum(points.conf25,na.rm=T)/sum(n.test)
score.conf1<-sum(points.conf1,na.rm=T)/sum(n.test)
score.conf5<-sum(points.conf5,na.rm=T)/sum(n.test)
score.conf2<-sum(points.conf2,na.rm=T)/sum(n.test)
score.t1<-sum(points.t1,na.rm=T)/sum(n.test)
score.t2<-sum(points.t2,na.rm=T)/sum(n.test)
num=n.test

return(list(num=num,score.hm=score.hm,score.conf=score.conf,score.conf25=score.conf25,score.conf1=score.conf1,score.conf5=score.conf5,score.conf2=score.conf2,score.t1=score.t1,score.t2=score.t2))
}
