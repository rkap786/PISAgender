setwd("/Users/radhika/Library/CloudStorage/GoogleDrive-rkap786@stanford.edu/My Drive/0. Projects - Stanford/PISA gender/Final code files revision/")
pdf("Figures/Fig1_concept.pdf",width=8,height=3)
layout(matrix(c(1,1,2,3),2,2,byrow=FALSE))
par(mgp=c(2,1,0),oma=rep(0,4),mar=c(2,2,1,1))
#
x<-sort(rnorm(10000))
p<-1/(1+exp(-x))
y<-rbinom(length(x),1,p)
plot(x,p,type='l',ylab="",xlab="",xaxt='n',yaxt='n',lwd=3,col="black",ylim=0:1)
mtext(side=2,line=.5,"Accuracy")
mtext(side=1,line=.5,"Response time (t)")
text(2,.8,"CAF",cex=.7)
#
plot(x,p+.075,type='l',ylab="",xlab="",xaxt='n',yaxt='n',lwd=3,col="blue",ylim=0:1)
lines(x,p-.075,lwd=3,col="red")
mtext(side=2,line=.5,"Accuracy")
mtext(side=1,line=.5,"Response time (t)")
x0<-x
den<-density(x0)
M<-max(p)
M<-M/5
dy<-M*den$y/max(den$y)
lines(den$x,dy,col='black',lwd=3)
legend("topleft",bty="n",fill=c("red","blue"),c("Group 1","Group 2"),title="Pure Capacity",cex=1)
text(2,.67,"CAFs",cex=1)
text(2,.3,"Response time\ndistribution",cex=1)
#
plot(x,p,type='l',ylab="",xlab="",xaxt='n',yaxt='n',lwd=3,col="black",ylim=0:1)
mtext(side=2,line=.5,"Accuracy")
mtext(side=1,line=.5,"Response time (t)")
x0<-x-sd(x)/5
den<-density(x0)
M<-max(p)
M<-M/5
dy<-M*den$y/max(den$y)
lines(den$x,dy,col='red',lwd=3)
x0<-x+sd(x)/5
den<-density(x0)
M<-max(p)
M<-M/5
dy<-M*den$y/max(den$y)
lines(den$x,dy,col='blue',lwd=3)
#legend("topleft",bty="n",fill=c("red","blue"),c("Group 1","Group 2"),title="Pure Speed")
text(2,.75,"CAF",cex=1)
text(2,.3,"Response time\ndistributions",cex=1)
dev.off()
