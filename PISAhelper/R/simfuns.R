simdata<-function(speed.offset,th.offset,N=1000,rho=0,b.time,nitems=45) {
  library(MASS) 
  z1<-mvrnorm(N,c(0,0),matrix(c(1,rho,rho,1),2,2))
  z2<-mvrnorm(N,c(th.offset,speed.offset),matrix(c(1,rho,rho,1),2,2))
  z<-rbind(cbind(z1,1),cbind(z2,2))
  th<-z[,1]
  tau<-z[,2]
  gr<-z[,3]
  diff<-rnorm(nitems,mean=0,sd=.5)
  L<-list()
  for (i in 1:length(diff)) L[[i]]<-cbind(1:length(th),i,th,tau,diff[i],gr)
  x<-data.frame(do.call("rbind",L))
  names(x)<-c("id","item","th","tau","diff","group")
  #x$delta<-rnorm(nrow(x),mean=0,sd=.5)
  ##
  #f1<-function(tau,delta) 2/(1+exp(-1*(tau-delta)))
  f1<-function(del) exp(rnorm(nrow(x),mean=.1+.3*(del),sd=.5))
  #f<-Vectorize(f1)
  x$rt<-f1(-1*x$tau)
  #x$rt<-ifelse(x$rt>5,5,x$rt)
  #plot(density(x$rt))
  #by(x$rt,x$group,mean)
  #plot(density(x$rt[x$group==1])); lines(density(x$rt[x$group==2]),col='red')
  ##
  f2<-function(th,t,diff,b.time) 1/(1+exp(-(th+b.time*t-diff)))
  f<-Vectorize(f2,vectorize.args=c("th","t","diff"))
  x$pv<-f(th=x$th,t=x$rt-mean(x$rt),diff=x$diff,b.time=b.time)
  #plot(density(x$pv[x$group==1])); lines(density(x$pv[x$group==2]),col='red')
  #by(x$pv,x$group,mean)
  x$resp<-rbinom(nrow(x),1,x$pv)
  #cor(x$pv,x$resp)
  #by(x$resp,x$group,mean)
  #print(summary(lm(resp~rt+group,x)))
  x
}



simdata.rho<-function(speed.offset,th.offset,N=1000,rho1=0,rho2=0,b.time,nitems=45) {
    library(MASS) 
    z1<-mvrnorm(N,c(0,0),matrix(c(1,rho1,rho1,1),2,2))
    z2<-mvrnorm(N,c(th.offset,speed.offset),matrix(c(1,rho2,rho2,1),2,2))
    z<-rbind(cbind(z1,1),cbind(z2,2))
    th<-z[,1]
    tau<-z[,2]
    gr<-z[,3]
    diff<-rnorm(nitems,mean=0,sd=.5)
    L<-list()
    for (i in 1:length(diff)) L[[i]]<-cbind(1:length(th),i,th,tau,diff[i],gr)
    x<-data.frame(do.call("rbind",L))
    names(x)<-c("id","item","th","tau","diff","group")
    #x$delta<-rnorm(nrow(x),mean=0,sd=.5)
    ##
                                        #f1<-function(tau,delta) 2/(1+exp(-1*(tau-delta)))
    f1<-function(del) exp(rnorm(nrow(x),mean=.1+.3*(del),sd=.5))
    #f<-Vectorize(f1)
    x$rt<-f1(-1*x$tau)
                                        #x$rt<-ifelse(x$rt>5,5,x$rt)
                                        #plot(density(x$rt))
                                        #by(x$rt,x$group,mean)
                                        #plot(density(x$rt[x$group==1])); lines(density(x$rt[x$group==2]),col='red')
    ##
    f2<-function(th,t,diff,b.time) 1/(1+exp(-(th+b.time*t-diff)))
    f<-Vectorize(f2,vectorize.args=c("th","t","diff"))
    x$pv<-f(th=x$th,t=x$rt-mean(x$rt),diff=x$diff,b.time=b.time)
    #plot(density(x$pv[x$group==1])); lines(density(x$pv[x$group==2]),col='red')
    #by(x$pv,x$group,mean)
    x$resp<-rbinom(nrow(x),1,x$pv)
    #cor(x$pv,x$resp)
                                        #by(x$resp,x$group,mean)
    #print(summary(lm(resp~rt+group,x)))
    x
}

# getcaf<-function(x) {
#     library(splines)
#     tmp.bs<-bs(x$rt)
#      
#     for (i in 1:ncol(tmp.bs)) x[[paste("bs",i,sep='')]]<-tmp.bs[,i]
#     caf<-list()
#     library(fixest) 
#     ran<-quantile(x$rt,c(.01,.99),na.rm=TRUE) #range(x$rt)
#     for (i in 1:2) {
#         xx<-x[x$group==i,]
#         mod<-feols(resp~bs1+bs2+bs3|item+id,xx)
#                                         #print(mod)
#                                         #p0<-mean(xx$pv.est)
#         t<-seq(ran[1],ran[2],length.out=1000)
#         z<-predict(tmp.bs,t)
#         fe<-fixest::fixef(mod)
#         #if (i==1) {
#         #    ii<-which.min(abs(fe$item))
#         #    item<-names(fe$item)[ii]
#         #}
#         #ii<-which.min(abs(fe$id))
#         #id<-names(fe$id)[ii]
#         z<-data.frame(bs1=z[,1],bs2=z[,2],bs3=z[,3])# ,item=item,id=id)
#         co<-coef(mod)
#         yhat<-0
#         for (j in 1:length(co)) yhat<-yhat+co[j]*z[,j]
#         yhat<-yhat+mean(fe$item)+mean(fe$id)
#         #y0<-predict(mod,z)
#         caf[[i]]<-data.frame(t=t,z,yhat=yhat) #yhat=y0-mean(y0))
#     }
#     caf[[1]]<-cbind(caf[[1]]$t,caf[[1]]$yhat)
#     caf[[2]]<-cbind(caf[[2]]$t,caf[[2]]$yhat)
#     caf
# }

integrate<-function(rt,caf) {
    ## library(kdensity)
    ## if (length(rt)>5000) rt<-sample(rt,5000)
    ## kd<-kdensity(rt)
    ## den<-kd(rt)

    den<-density(rt)
    rt0<-seq(min(rt),max(rt),length.out=1000)
    den<-approx(den$x,den$y,xout=rt0)$y
        
    del<-mean(diff(rt0))
    y<-numeric()
    ##
    ff<-function(t,caf) {
        ii<-which.min(abs(t-caf$t))
        caf$yhat[ii]
    }
    ff<-Vectorize(ff,"t")
    y<-ff(rt0,caf)
    sum(den*del*y)
}
# 
# plotcaf = function(caf, heading_caf) {
# caf.tmp<-data.frame(t=caf[[2]][,1],yhat=caf[[2]][,2]-caf[[1]][,2])
# plot(caf[[1]],type='l',ylim=c(0,1),xlab="time", ylab = "predicted accuracy", main=heading_caf)
# lines(caf[[2]],col='red') 
# legend("top",legend = c("female", "male"), lty=1:1, col = c("black", "red"), cex=0.7)
# ##
# d<-density(x$rt[x$group==1])
# d$y<-.2*d$y/max(d$y)
# lines(d$x,d$y)
# d<-density(x$rt[x$group==2])
# d$y<-.2*d$y/max(d$y)
# lines(d$x,d$y,col='red')
# ##
# M<-by(x$resp,x$group,mean)
# ##integrate
# caf.tmp<-data.frame(t=caf[[2]][,1],yhat=caf[[2]][,2]-caf[[1]][,2])
# intdiff<-integrate(x$rt,caf.tmp)
# ##
# legend("center",bty='n',legend=c(paste("obs",round(M[[2]]-M[[1]],2)),paste("int",round(intdiff,2))),cex=0.8)
# 
# }
