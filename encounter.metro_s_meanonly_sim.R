#only keep the mean to try to use all rules
#######this version only keeps probabilities.  

pt.tmp.fcn<-function(beta.tmp,g,x.matrix=x.mat){
    out=exp((x.matrix%*%beta.tmp)+g)
    return(out)}

f.metro<-function(y,n, x.mat=NULL, n.iter=5000,n.thin=10,n.burn=1000,n.chain=3){

#beta is n.rules*ncol(x)+1
#gamma is n.users long
#tau is n.rules*n.users

print(date())
if(is.null(x.mat)){stop("havent coded intercept only mode yet!  Need at least one covariate")}
#n.iter=5000
#n.thin=10
#n.burn=1000
#n.chain=3 
#x.mat=x.true

#dimensions
n.rules<-ncol(y)
n.users<-nrow(y)
k=ncol(x.mat)
n.par<-(n.users*n.rules)+n.users+(n.users*n.rules)
n.keep<-(n.iter-n.burn)/n.thin
sims<-array(0, c(n.chain,n.par))

for(ccc in 1:n.chain){     
 ## initialization
    mc.v.p<-abs(y/ifelse(n>0,n,1)+runif(length(y),-.1,.1))
    mc.v.beta<-matrix(rnorm((n.rules*(k)),0,sqrt(8)),n.rules,k)
    mc.v.tbeta<-matrix(rnorm((n.rules*(k)),0,sqrt(8)),n.rules,k)
    mc.v.gamma<-rnorm(n.users,0,sqrt(8))
    mc.v.tau<-rnorm(n.users,0,sqrt(4))
    mc.v.mu.beta<-colMeans(mc.v.beta)
    mc.v.mu.tbeta<-colMeans(mc.v.tbeta)
    mc.v.mu.beta<-runif(k,-2,2)
    mc.v.mu.tbeta<-rep(0,k)
    mc.v.mu.gamma<-mean(mc.v.gamma)
    #mc.v.mu.tau<-mean(mc.v.tau)
    #mc.v.mu.gamma=0
    mc.v.mu.tau=0
    mc.v.sigma.beta<-apply(mc.v.beta,2,'var')
    mc.v.sigma.tbeta<-apply(mc.v.tbeta,2,'var')
    mc.v.sigma.gamma<-var(mc.v.gamma)
    mc.v.sigma.tau<-var(mc.v.tau)
    
    
    
     bd.jump<-matrix(.1,n.rules,k)
     tbd.jump<-matrix(.1,n.rules,k)
     gd.jump<-rep(.1,n.users)
     td.jump<-rep(.1,n.users)
     ### jump's are the variance of the jump distribution.
     last.20p<-array(NA, c((n.users)+n.users+(2*n.rules*(k)),50))
     p.ct<-0
     for(t in 1:n.iter){
       temp<-f.update(y=y,n=n, x.mat=x.mat, v.p=mc.v.p, v.tbeta=mc.v.tbeta,v.beta=mc.v.beta, v.gamma=mc.v.gamma,v.tau=mc.v.tau, v.mu.tbeta=mc.v.mu.tbeta,v.mu.beta=mc.v.mu.beta,v.mu.gamma=mc.v.mu.gamma,v.mu.tau=mc.v.mu.tau,v.sigma.beta=mc.v.sigma.beta,v.sigma.gamma=mc.v.sigma.gamma,v.sigma.tbeta=mc.v.sigma.tbeta,v.sigma.tau=mc.v.sigma.tau,bd.jump=bd.jump,td.jump=td.jump,gd.jump=gd.jump,tbd.jump=tbd.jump)
       mc.v.p<-temp$v.p
       mc.v.beta<-temp$v.beta
       mc.v.gamma<-temp$v.gamma
       mc.v.tau<-temp$v.tau
       mc.v.tbeta<-temp$v.tbeta
       mc.v.mu.beta<-temp$v.mu.beta
       mc.v.mu.tbeta<-temp$v.mu.tbeta
       mc.v.mu.gamma<-temp$v.mu.gamma
       mc.v.mu.tau<-temp$v.mu.tau
       mc.v.sigma.beta<-temp$v.sigma.beta
       mc.v.sigma.tbeta<-temp$v.sigma.tbeta
       mc.v.sigma.gamma<-temp$v.sigma.gamma
       mc.v.sigma.tau<-temp$v.sigma.tau
       
       #if(t<5001){
        p.ct<-p.ct+1
       last.20p[,p.ct]<-temp$jump.p
    #order for jumping probabilities 
    #c(t.p,g.p,b.p)
       if(p.ct==50){  
       p.ct<-0
        jmp.t.mean<-apply(last.20p[1:(n.users),],1,mean)
        jmp.g.mean<-apply(last.20p[((n.users)+1):(n.users+n.users),],1,mean)
        jmp.b.mean<-apply(last.20p[(n.users+n.users+1):(n.users+n.users+((k)*n.rules)),],1,mean)
        jmp.tb.mean<-apply(last.20p[(n.users+n.users+((k)*n.rules)+1):(n.users+n.users+(((k)*n.rules)+((k)*n.rules))),],1,mean)
        td.jump<-pmax(pmin(log(0.23)*td.jump/log(jmp.t.mean), pmin(10*td.jump,2)), 0.2)
        gd.jump<-pmax(pmin(log(0.23)*gd.jump/log(jmp.g.mean), pmin(10*gd.jump,2)), 0.2)
        bd.jump<-pmax(pmin(log(0.23)*bd.jump/log(jmp.b.mean), pmin(10*bd.jump,2)), .01)
        tbd.jump<-pmax(pmin(log(0.23)*tbd.jump/log(jmp.tb.mean), pmin(10*tbd.jump,2)), .01)}
        #}
        
         
       if (t%%500==0){
       #print("gamma 0 mean")
        print(c(t,date()))}
          
        if(t%%n.thin==0 & t>n.burn){      	
          sims[ccc,(1:(n.users*n.rules))] <-sims[ccc,(1:(n.users*n.rules))]+mc.v.p        
		  sims[ccc,((n.users*n.rules)+1):(((n.users*n.rules)+n.users))] <-sims[ccc,((n.users*n.rules)+1):(((n.users*n.rules)+n.users))]+exp(mc.v.tau)        
          b.tmp<-exp((x.mat%*%mc.v.beta[1,])+mc.v.gamma)
          for(u in 2:n.rules){
          b.tmp<-cbind(b.tmp,exp((x.mat%*%mc.v.beta[u,])+mc.v.gamma))}
          sims[ccc,(((n.users*n.rules)+n.users+1):((n.users*n.rules)+n.users+(n.users*n.rules)))]<-sims[ccc,(((n.users*n.rules)+n.users+1):((n.users*n.rules)+n.users+(n.users*n.rules)))]+as.vector(b.tmp)        
        #  c(mc.v.p, mc.v.beta, mc.v.tbeta,mc.v.gamma,mc.v.tau, #mc.v.mu.beta,mc.v.mu.tbeta,mc.v.mu.gamma,mc.v.mu.tau,mc.v.sigma.beta,mc.v.sigma.tbeta,mc.v.sigma.gamma,mc.v.sigma.tau)
        }
        }
        }
     ord<-"mc.v.p, gamma, pi"
    #return(list(sims=sims,ord=ord,b.out=b.out,p.out=p.out,g.out=g.out,t.out=t.out))
        return(list(sims=sims,ord=ord,n.iter=n.iter,n.thin=n.thin,n.burn=n.burn))

}


  
f.update<-function(y,n, x.mat, v.p, v.beta, v.gamma,v.tau,v.tbeta,v.mu.tbeta, v.mu.beta,v.mu.gamma,v.mu.tau,v.sigma.tbeta,v.sigma.beta,v.sigma.gamma,v.sigma.tau,bd.jump,td.jump,gd.jump,tbd.jump){
      
    n.users<-nrow(y)
    n.rules<-ncol(y)
    k=ncol(x.mat)
    
#  #  #####comment this out unless debugging###
#    v.gamma=mc.v.gamma
#    v.beta=mc.v.beta
#    v.tau=mc.v.tau
#    v.mu.tau=mc.v.mu.tau
#    v.sigma.tau=mc.v.sigma.tau
#    td.jump=td.jump
#    gd.jump=gd.jump
#    bd.jump=bd.jump
#    v.mu.gamma=mc.v.mu.gamma
#    v.sigma.gamma=mc.v.sigma.gamma
#    v.mu.beta=mc.v.mu.beta
#    v.sigma.beta=mc.v.sigma.beta
    ###
#    
### updating p 
    v.p<-matrix(NA,n.users,n.rules)
    for(r in 1:n.rules){
    pi.tmp<-exp((x.mat%*%v.beta[r,])+v.gamma)
    tau.tmp<-exp((x.mat%*%v.tbeta[r,])+v.tau)
    v.p[,r]<-rbeta(n.users,(y[,r]+pi.tmp),(n[,r]-y[,r]+tau.tmp))
    v.p[v.p[,r]==1,r]<-v.p[v.p[,r]==1,r]-runif(1,1e-16,1e-15)
    v.p[v.p[,r]<1e-15,r]<-1e-15}

###updating gamma
    g.old<-(v.gamma)
    pi.tmp<-apply(v.beta,1,pt.tmp.fcn,g=g.old,x.matrix=x.mat)
    tau.tmp<-apply(v.tbeta,1,pt.tmp.fcn,g=v.tau,x.matrix=x.mat)
    lik.old<-rowSums((-log(beta(pi.tmp,tau.tmp)+1e-320))+((y+pi.tmp-1)*log(v.p))+(log(1-v.p)*(n-y+tau.tmp-1)))+log(dnorm(g.old,v.mu.gamma,sqrt(v.sigma.gamma)))
    g.star<-g.old+rnorm(n.users,0,sqrt(gd.jump))
    pi.tmp<-apply(v.beta,1,pt.tmp.fcn,g=g.star,x.matrix=x.mat)
    lik.star<-rowSums((-log(beta(pi.tmp,tau.tmp)+1e-320))+((y+pi.tmp-1)*log(v.p))+(log(1-v.p)*(n-y+tau.tmp-1)))+log(dnorm(g.star,v.mu.gamma,sqrt(v.sigma.gamma)))
    prob.diff<-lik.star-lik.old
    g.valid<-(!is.infinite(prob.diff))&(!(is.na(prob.diff)))
    jump.new<-rep(0,length(v.gamma))
    jump.new[g.valid]<-rbinom(sum(g.valid),1,exp(pmin(prob.diff[g.valid],0)))  
    v.gamma[jump.new==1]<-g.star[jump.new==1]
    v.gamma[jump.new<1]<-g.old[jump.new<1]  
    g.p<-rep(0,length(v.gamma))
    g.p[g.valid]<-exp(pmin(prob.diff[g.valid],0))

    ### updating v.mu.gamma
    v.mu.gamma<-rnorm(1, mean(v.gamma, na.rm=T), sqrt(v.sigma.gamma/n.users))
    ###updating v.sigma.gamma
    v.sigma.gamma<-sum((v.gamma-v.mu.gamma)^2, na.rm=T)/rchisq(1, n.users-1)
   
###updating tau
    t.old<-(v.tau)
    pi.tmp<-apply(v.beta,1,pt.tmp.fcn,g=v.gamma,x.matrix=x.mat)
    tau.tmp<-apply(v.tbeta,1,pt.tmp.fcn,g=t.old,x.matrix=x.mat)
    lik.old<-rowSums((-log(beta(pi.tmp,tau.tmp)+1e-320))+((y+pi.tmp-1)*log(v.p))+(log(1-v.p)*(n-y+tau.tmp-1)))+(log(dnorm(t.old,0,sqrt(v.sigma.tau))))
    t.star<-t.old+rnorm(n.users,0,sqrt(td.jump))
    tau.tmp<-apply(v.tbeta,1,pt.tmp.fcn,g=t.star,x.matrix=x.mat)
    lik.star<-rowSums((-log(beta(pi.tmp,tau.tmp)+1e-320))+((y+pi.tmp-1)*log(v.p))+(log(1-v.p)*(n-y+tau.tmp-1)))+(log(dnorm(t.star,0,sqrt(v.sigma.tau))))
    prob.diff<-t(lik.star-lik.old)
    t.valid<-(!is.infinite(prob.diff))&(!(is.na(prob.diff)))
    jump.new<-rep(0,length(v.tau))
    jump.new[t.valid]<-rbinom(sum(t.valid),1,exp(pmin(prob.diff[t.valid],0)))    
    v.tau[jump.new==1]<-t.star[jump.new==1]
    v.tau[jump.new<1]<-t.old[jump.new<1]
    t.p<-rep(0,length(v.tau))
    t.p[t.valid]<-exp(pmin(prob.diff[t.valid],0))

      ### updating v.mu.tau
    #v.mu.tau<-rnorm(1, mean(v.tau, na.rm=T), sqrt(v.sigma.tau/n.users))
    v.mu.tau=0
    ###updating v.sigma.tau
    v.sigma.tau<-sum((v.tau-v.mu.tau)^2, na.rm=T)/rchisq(1, n.users-1)


    tb.p<-matrix(0,nrow(v.beta),ncol(v.beta))
    v.tbeta<-matrix(0,nrow(v.beta),ncol(v.beta))
 
    
    
    v.mu.tbeta<-rep(0,k)
    v.sigma.tbeta<-rep(0,k)


###updating beta    
    b.p<-matrix(0,nrow(v.beta),ncol(v.beta))
    for(a in 1:n.rules){
    for(kkk in 1:k){
    b.old<-v.beta[a,]
    pi.tmp<-exp((x.mat%*%b.old)+v.gamma)
    tau.tmp<-exp((x.mat%*%v.tbeta[a,])+v.tau)
    lik.old<-sum((-log(beta(pi.tmp,tau.tmp)+1e-320))+((y[,a]+pi.tmp-1)*log(v.p[,a]))+(log(1-v.p[,a])*(n[,a]-y[,a]+tau.tmp-1)))+((log(dnorm(b.old[kkk],v.mu.beta[kkk],sqrt(v.sigma.beta[kkk])))))
    b.star<-b.old
    b.star[kkk]<-b.old[kkk]+rnorm(1,0,sqrt(matrix(bd.jump,n.rules,(k))[a,kkk]))
    pi.tmp<-exp((x.mat%*%b.star)+v.gamma)
    lik.star<-sum((-log(beta(pi.tmp,tau.tmp)+1e-320))+((y[,a]+pi.tmp-1)*log(v.p[,a]))+(log(1-v.p[,a])*(n[,a]-y[,a]+tau.tmp-1)))+((log(dnorm(b.star[kkk],v.mu.beta[kkk],sqrt(v.sigma.beta[kkk])))))
    prob.diff<-lik.star-lik.old
    #print(prob.diff)
    b.valid<-(!is.infinite(prob.diff))&(!(is.na(prob.diff)))
    if(b.valid>0){
    jump.new<-rbinom(1,1,exp(pmin(prob.diff,0))) 
    if(jump.new>0){
    v.beta[a,kkk]<-b.star[kkk]
    b.p[a,kkk]<-exp(pmin(prob.diff,0))}
    if(jump.new<1){
    v.beta[a,kkk]<-b.old[kkk]
    b.p[a,kkk]<-exp(pmin(prob.diff,0))}}
    if(b.valid==0){
    #print('drrrr')
    ind<-rbinom(1,1,0)
    v.beta[a,kkk]<-(1-ind)*b.old[kkk]+ind*b.star[kkk]
    b.p[a,kkk]<-0
    }}}
    b.p<-as.vector(b.p)
    
    for(aa in 1:k){
    ###updating v.mu.beta
    v.mu.beta[aa]<-rnorm(1, mean(v.beta, na.rm=T), sqrt(v.sigma.beta[aa]/(k*n.rules)))
    #v.mu.beta[aa]<-0
    ###updating v.sigma.beta
    v.sigma.beta[aa]<-sum((v.beta-v.mu.beta)^2, na.rm=T)/rchisq(1, (k*n.rules)-1)}

return(list(v.p=v.p, v.tau=v.tau,v.gamma=v.gamma,v.beta=v.beta,v.tbeta=v.tbeta,v.mu.tbeta=v.mu.tbeta,v.mu.tau=v.mu.tau,v.mu.gamma=v.mu.gamma,v.mu.beta=v.mu.beta,v.sigma.tau=v.sigma.tau,v.sigma.gamma=v.sigma.gamma,v.sigma.beta=v.sigma.beta, v.sigma.tbeta=v.sigma.tbeta,jump.p=c(t.p,g.p,b.p,tb.p)))
}
