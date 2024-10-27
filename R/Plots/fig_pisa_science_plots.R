setwd("/Users/radhika/Library/CloudStorage/GoogleDrive-rkap786@stanford.edu/My Drive/0. Projects - Stanford/PISA gender/Final code files revision/")

library(readr)
library(PISAhelper)
library(tidyverse)
library(fs)
library(ggpubr)
library(dplyr)
library(ggplot2)



source("Code/PISAgender/PISAhelper/R/scorediff.R")
source("Code/PISAgender/PISAhelper/R/getcaf_deg.R")
source("Code/PISAgender/PISAhelper/R/getcaf0_deg.R")
source("Code/PISAgender/PISA_dataprep_funcs.R")
source("Code//PISAgender/analysis_fncs.R")

##########################################
nm=""
load("Data/Final data/science.Rdata")


######################################################################

pdf("Figures/country_resultsB_science.pdf",width=10,height=3.5)
par(mfrow=c(1,4),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
L=read_csv("Data/Final data/science_book13to18_results_q99q1.csv")
# L<-list()
# z=lapply(read_rf,ff)
# L= data.frame(do.call("rbind",z))


####################################
##densities of obs accuracy
# p1<-function(L) {
#     z<-L
#     den<-density(-z$obs.acc)
#     plot(den,main='',sub='',xlab="Acc diff: females-males",xlim=c(-.15,.15),ylab='',yaxt='n',col='blue')
#     cc<-col2rgb("blue")
#     col<-rgb(cc[1],cc[2],cc[3],max=255,alpha=55)
#     polygon(c(den$x,rev(den$x)),c(den$y,rep(0,length(den$y))),col=col)
#     mtext(side=3,line=0,"Reading fluency")
#     abline(v=0,lwd=2)
#     legend("topleft",bty='n',paste("E(y)=",round(mean(z$acc),2),sep=''))
#     #NULL
# }
#pdf("Figures/country_resultsB.pdf",width=8,height=2.7)
#par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
## Plot 1
zden<-L
den<-density(-zden$actualdiff)
plot(den,main='',sub='',xlab="Accuracy diff: females-males",xlim=c(-.15,.15),ylab='',yaxt='n',col='lightblue')
cc<-col2rgb("lightblue")
col<-rgb(cc[1],cc[2],cc[3],max=255,alpha=55)
polygon(c(den$x,rev(den$x)),c(den$y,rep(0,length(den$y))),col=col)
mtext(side=3,line=0,"Accuracy difference", cex=1)
abline(v=0,lwd=2)
legend("topleft",bty='n',paste("E(y)=",round(mean(-zden$actualdiff, na.rm=T),2),sep=''))

#p1(L)
#dev.off()

### Plot 2
L=science
# summary(do.call(rbind, lapply(L, function(x) quantile(x$rt,.1))))
# summary(do.call(rbind, lapply(L, function(x) quantile(x$rt,.3))))

for (i in 1:length(L)) {
  
  upper=quantile(L[[i]]$rt, 0.99)
  lower=quantile(L[[i]]$rt, 0.01)
  L[[i]]= L[[i]] |>
    filter(rt>=(lower) & rt<=(upper))  |>
    mutate(rt= log(rt)) 

}

med_f= as.numeric(do.call(rbind, lapply(L, function(x) median(x$rt[x$gender==1], na.rm=T))))
med_m= as.numeric(do.call(rbind, lapply(L, function(x) median(x$rt[x$gender==2], na.rm=T))))
med= do.call(rbind, lapply(L, function(x) median(x$rt, na.rm=T)))
M= median(med)
# 
# mlist=c()
# for (i in 1:length(L)) {
#   m=median(L[[i]]$rt, na.rm = T)
#   mlist=c(mlist, m)
# }
# M=exp(median(mlist))


plot(med_f, med_m, pch=16,xlab="females", ylab="males",
     xlim=c(3,4.5),ylim=c(3,4.5))
abline(0,1)
mtext(side=3,line=0,"Median log response time", cex=1)
legend("topleft",bty='n',paste("Median=",round(M,2),
                               "log secs;\n            =", round(exp(M),2), "secs"))

###

#Plot 3
####################################
##qqplots of time
    #rt<-lapply(L,function(x) x$rt)


L=science
# for (i in 1:length(science)) {
#   L[[i]]= filterdata(L[[i]], qu=0.99, ql=0.01) #drop 1% and log rt
# }
# 

for (i in 1:length(L)) {
  
  upper=quantile(L[[i]]$rt, 0.99)
  lower=quantile(L[[i]]$rt, 0.01)
  L[[i]]= L[[i]] |>
    filter(rt>=(lower) & rt<=(upper))  |>
    mutate(rt= log(rt)) 
  
}
##########
  rt<-lapply(L,function(x) x$rt)
    #M<-median(unlist(rt),na.rm=TRUE)
    qf<-function(z) {
        qq<-qqplot(z$rt[z$gender==1],z$rt[z$gender==2],plot.it=FALSE)
        qq
    }
    Lq<-lapply(L,qf)
    plot(NULL,xlim=c(-4,4),ylim=c(-4,4),xlab="females",ylab="males")
    mtext(side=3,line=0,"Q-Q plot: log time", cex=1)
    #legend("topleft",bty='n',paste("Median=",round(exp(M),2), " secs"))
    for (i in 1:length(Lq)) lines(Lq[[i]]$x,Lq[[i]]$y,col='lightblue',type='l')
    abline(0,1,lwd=2)
    #NULL

#pdf("/home/bd/Dropbox/Apps/Overleaf/PISA gender NCME/country_resultsA.pdf",width=8,height=2.7)
#par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
#for (i in 1:length(dfL)) p2(dfL[[i]],nm=names(dfL)[i] )
#dev.off()



####################################
##CAFs
p3<-function(ll) {
    #ll<-ll[[nm]]
  #ll=L
  #ll=read_rf
    x<-data.frame(do.call("rbind",ll))
    ca<-getcaf0_deg(x, deg=2)
    plot(NULL,xlim=c(-4,4),ylim=c(0,1),xlab="Response time (log seconds)",ylab='Accuracy offset')
    mtext(side=3,line=0,"CAFs", cex=1)
    abline(h=0,col='gray')
    for (i in 1:length(ll)) {
        ca2<-getcaf0_deg(ll[[i]])
        lines(ca2,col='lightblue')
    }
    lines(ca[,1],ca[,2],type='l',lwd=2)
    qu<-quantile(x$rt,c(.05,.25,.5,.75,.95),na.rm=TRUE)
    for (ii in 1:length(qu)) {
        jj<-which.min(abs(qu[ii]-ca[,1]))
        if (ca[,2][jj]<ca[,2][jj+1]) pos<-1 else pos<-3
        if (qu[ii]==quantile(x$rt,0.25) | qu[ii]==quantile(x$rt,0.75)) pos=3
        points(ca[,1][jj],ca[,2][jj],col='red',pch=19)
        text(ca[,1][jj],ca[,2][jj],names(qu)[ii],cex=.7,pos=pos,col='red')
    }
    NULL
}
#pdf("/home/bd/Dropbox/Apps/Overleaf/PISA gender NCME/country_resultsC.pdf",width=8,height=2.7)
#par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
#lapply(names(dfL),p3,dfL)
#lapply(names(read_rf),p3,read_rf)
L= science
for (i in 1:length(science)) {
  L[[i]]= filterdata(L[[i]], qu=0.99, ql=0.01) #drop 1% and log rt
}

p3(L)
dev.off()


####################################
##A versus A'
# 
# p4<-function(nm,L) {
#     z<-L[[nm]]
#     mm<-min(c(z$obs.acc,z$int))
#     MM<-max(c(z$obs.acc,z$int))
#     del<-abs(z$obs.acc-z$int)
#     pch<-ifelse(del<.01,1,19)
#     col<-ifelse(del<.01,'gray','blue')
#                                         #plot(z$obs.acc,z$int,pch=pch,col=col,xlim=c(mm,MM),ylim=c(mm,MM))
#     plot(z$obs.acc,z$int,pch=pch,col=col,xlim=c(mm,MM),ylim=c(mm,MM))
#     mtext(side=3,line=0,nm)
#     abline(0,1); abline(.01,1,col='blue'); abline(-.01,1,col='blue')
#     m<-mean(z$obs.acc-z$int)
#     legend("bottomright",bty="n",paste("Mean(A-A')=",round(m,3),sep=''))
#     NULL
# }
# #pdf("/home/bd/Dropbox/Apps/Overleaf/PISA gender NCME/country_resultsD.pdf",width=8,height=2.7)
# par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
# lapply(names(L),p4,L=L)
# dev.off()
