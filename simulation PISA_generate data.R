


## simulate dataset
#install.packages("remotes")
#remotes::install_github("ben-domingue/PISAhelper")
#library(PISAhelper)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
setwd("/Users/radhika/Library/CloudStorage/GoogleDrive-rkap786@stanford.edu/My Drive/0. Projects - Stanford/PISA gender/Final code files revision/")
source("Code/PISAhelper/R/scorediff.R")
source("Code/PISAhelper/R/simfuns.R")

result= data.frame()
speed.offset_r<-c(-.5,0,.5)
th.offset_r<-runif(50,min=0,max=.25)
rho<-c(0,.25,0.5)
N=1000

# caf=getcaf(x)
# plotcaf(caf,"th= 0.2, tau=0.5, b=0.5")
cl<-makeCluster(parallel::detectCores())
registerDoSNOW(cl)

for (speed.offset in speed.offset_r) {
  for (th.offset in th.offset_r) {
    for (nitems in c(20,45,100)) {
    for(b.time in c(-0.5,0, 0.5)) {
      for (r in rho) {
        

      # print(N)
      # print(speed.offset)
      # print(th.offset)
      # print(b.time) 
    x<-simdata(speed.offset=speed.offset,th.offset=th.offset,
               N=N,rho=r, b.time=b.time, nitems=nitems)
    #x$gender= x$group
    ##### estimate same CAF, density for group 1
    x1= x |>
      filter(group==1)
    
    x2= x |>
      filter(group==2)
    # summary(x1$rt)
    # summary(x2$rt)
    M<-by(x$resp,x$group,mean)
    ##### hold time density the same, different group CAFs
    # caf.tmp<-data.frame(t=caf[[2]][,1],yhat=caf[[2]][,2])
    # intdiff<-integrate(x$rt,caf.tmp)
    #print(M)
    #print(intdiff) #0.4766381
    
    caf = getcaf_deg(x, deg=2)
    caf0= getcaf0_deg(x, deg=2)
    
    ##### time density of group 1, CAF for group 2 (t1h2) 
    caf.tmp<-data.frame(t=caf[[2]][,1],yhat=caf[[2]][,2])
    intdiff1<-integrate(x1$rt,caf.tmp)
    
    
    ##### time density of group 2, CAF for group 2 (t2h2)
    # caf.tmp<-data.frame(t=caf[[2]][,1],yhat=caf[[2]][,2])
    # intdiff2<-integrate(x2$rt,caf.tmp)
    
    
    ##### merged time density, CAF for group 2 (th2)  
    caf.tmp<-data.frame(t=caf[[2]][,1],yhat=caf[[2]][,2])
    intdiff3<-integrate(x$rt,caf.tmp)

    
    ##### merged time density, CAF for group 1 (th1)
    caf.tmp<-data.frame(t=caf[[1]][,1],yhat=caf[[1]][,2])
    intdiff4<-integrate(x$rt,caf.tmp)
    
    
    ##### time density of group 1, CAF for group 1 (t1h1)
    caf.tmp<-data.frame(t=caf[[1]][,1],yhat=caf[[1]][,2])
    intdiff5<-integrate(x1$rt,caf.tmp)
    # 
    
    ##### time density of group 2, CAF for group 1 (t2h1)
    caf.tmp<-data.frame(t=caf[[1]][,1],yhat=caf[[1]][,2])
    intdiff6<-integrate(x2$rt,caf.tmp)
    
    #### time density of group 1, merged CAF (t1h)
    caf.tmp<-data.frame(t=caf0[,1],yhat=caf0[,2])
    intdiff7<-integrate(x1$rt,caf.tmp)
    
    #### time density of group 2, merged CAF (t2h)
    caf.tmp<-data.frame(t=caf0[,1],yhat=caf0[,2])
    intdiff8<-integrate(x2$rt,caf.tmp)
    
    out = c(N, speed.offset, b.time, th.offset, r, nitems, M[[1]], M[[2]],
            intdiff1, intdiff3, intdiff4, intdiff5, intdiff6, intdiff7, intdiff8
            )
    #print(out)
    result= rbind(result,out)
      }
    }
    } 
  }
}

stopCluster(cl)

result = data.frame(result)
names(result) = c("N", "speed","b.time", "ability","rho","nitems","group1", "group2", 
                  "intdiff_t1h2", "intdiff_th2", "intdiff_th1", "intdiff_t1h1", 
                  "intdiff_t2h1","intdiff_t1h","intdiff_t2h")
#names(result) = c("N", "speed","b.time", "ability","group1", "group2", "intdiff_t1h2", "intdiff_th2", "intdiff_th1", "intdiff_t1h1", "intdiff_t2h1", "intdiff_t1h", "intdiff_t2h")


result = result |>
  mutate_if(is.character, as.numeric) |> 
  mutate(
         score_diff = group2-group1, 
         a1 = intdiff_t1h2 - intdiff_t1h1,
         a2 = intdiff_t2h1 - intdiff_t1h1,
         a3c= intdiff_t2h - intdiff_t1h,
         a4s = intdiff_th2 - intdiff_th1)


result= write_csv(result,"Data/Sim data/result_v3.csv")

# r.theta1= result |>
#   filter(N==10000) |>
#   with(cor(ability,score_diff))
# 
# r.speed= result |>
#   filter(N==1000) |>
#   with(cor(speed,score_diff))

#r_th_a=cor(result$ability, result$score_diff)

#txt1<-bquote("r("~theta~", A)="~.(round(r.theta,2)))#txt1 <- expression(paste0("r(", theta, ",A)=",round(r.theta,2)))
#txt2<-bquote("r("~mu~", A)="~.(round(r.speed,2)))#txt1 <- expression(paste0("r(", theta, ",A)=",round(r.theta,2)))
# 
# ### Simulation figure: 3
# #r.theta=numeric(r.theta)
# txt1 = expression(paste("N=1000: r(",theta,",A)=",r_th_a)) #,";","N=10,000: r(",theta,",A)=",0.72)
# #txt2 = expression(paste("N=10,000: r(",theta,",A)=",0.72))
# #txt= paste(txt1,";", txt2)
# lab1=expression(paste("A ( ",b[t]," = 0.5)"))  
# lab2=expression(paste("A ( ",b[t]," = -0.5)"))  
#   
# plot_left1 = result |> 
#   filter(N==1000, b.time==-0.5) |>
#   ggplot(aes(x=ability, y=score_diff)) + geom_point(aes(color=factor(speed))) +
#   scale_color_manual(name = expression(mu[tau]), values=c("red", "black", "blue")) +theme_classic() +
#   xlab(expression(mu[theta])) + ylab (lab1)  +
#   facet_wrap(~N,
#              strip.position = "left",
#              labeller = labeller(
#                N=c("1000"= "N=1000",
#                     "10000"="N=10,000")),
#              ncol=1
#              ) +  
#   theme(axis.line.x  = element_line(),
#     legend.position="bottom",
#         strip.placement = "outside",
#         strip.background = element_blank()
# ) 
# 
# 
# plot_right1 = result |>
#   filter(b.time==-0.5,
#          N==1000) |>
#   ggplot(aes(x=ability, y=score_diff)) + geom_point(aes(color=factor(speed))) +
#   scale_color_manual(name = expression(mu[tau]), values=c("red", "black", "blue")) +theme_classic() +
#   xlab(expression(mu[theta])) + ylab (lab2)  +
#   facet_wrap(~N,
#              strip.position = "left",
#              labeller = labeller(
#                N=c("1000"= "N=1000",
#                    "10000"="N=10,000")),
#              ncol=1
#   ) +  
#   theme(axis.line.x  = element_line(),
#         legend.position="bottom",
#         strip.placement = "outside",
#         strip.background = element_blank()
#   ) 
# 
# 
# 
# ########################################################
# #lab1=expression(paste("A ( ",b[t]," =-0.5)"))  
# 
# plot_right1 = result |>
#   filter(b.time==0.5,
#          N==1000) |>
#   ggplot(aes(x=ability, y=score_diff)) + geom_point(aes(color=factor(speed))) +
#   scale_color_manual(name = expression(mu[tau]), values=c("red", "black", "blue")) +theme_classic() +
#   xlab(expression(mu[theta])) + ylab (lab1)  +
#   facet_wrap(~N,
#              strip.position = "left",
#              labeller = labeller(
#                N=c("1000"= "N=1000",
#                    "10000"="N=10,000")
#                ),
#              ncol=1
#   ) +  
#   theme(axis.line.x  = element_line(),
#         legend.position="bottom",
#         strip.placement = "outside",
#         strip.background = element_blank()
#         )
# 
# plot_right2 = result |>
#   filter(b.time==-0.5,
#          N==1000) |>
#   ggplot(aes(x=ability, y=score_diff)) + geom_point(aes(color=factor(speed))) +
#   scale_color_manual(name = expression(mu[tau]), values=c("red", "black", "blue")) +theme_classic() +
#   xlab(expression(mu[theta])) + ylab (lab1)  +
#   facet_wrap(~N,
#              strip.position = "left",
#              labeller = labeller(
#                N=c("1000"= "N=1000",
#                    "10000"="N=10,000")
#              ),
#              ncol=1
#   ) + labs(caption = txt1) + 
#   theme(axis.line.x  = element_line(),
#         legend.position="bottom",
#         strip.placement = "outside",
#         strip.background = element_blank()
#   )
# 
# ggarrange(plot_left1 + ggtitle(lab1), plot_left2, plot_right1, plot_right2, 
#           common.legend = T,
#           align = "h", widths = c(2,2,2,2))
# 
# 
# 
# ########################################################
# 
# caf= getcaf(x)
# caf_group1= data.frame(caf[[1]])
# names(caf_group1)= c("rt", "yhat")
# caf_group1$group= "group1"
# caf_group2= data.frame(caf[[2]])
# names(caf_group2)= c("rt", "yhat")
# caf_group2$group= "group2"
# 
# caf_merged= rbind(caf_group1, caf_group2)
# table(caf_merged$group)
# 
# plot1= caf_merged |>
#   ggplot(aes(x= rt, y= yhat, color= group)) + geom_line() +theme(legend.position = "bottom")
# 
# plot2= x |>
#   ggplot(aes(x= rt, color= factor(group), group=factor(group))) + geom_density() +theme(legend.position = "none")
# ggarrange(plot1,plot2)
# 
# 
# ### 
# M<-by(x$resp,x$group,mean)
# 
# length(x$rt) #very big
# #nrow(caf.tmp) #1000
# nrow(caf_merged) #2000
# 
# #plot(caf_group1$rt, caf_group1$yhat,col="blue")
# #lines(caf_group2$rt, caf_group2$yhat, col="black")
# 
# ##### estimate same CAF, density for group 1
# x1= x |>
#   filter(group==1)
# 
# x2= x |>
#   filter(group==2)
# summary(x1$rt)
# summary(x2$rt)
# 
# ##integrate
# ##### hold time density the same, difference in CAFs
# caf.tmp<-data.frame(t=caf[[2]][,1],yhat=caf[[2]][,2]-caf[[1]][,2])
# intdiff<-integrate(x$rt,caf.tmp)
# print(M)
# print(intdiff) #-0.006748815
# 
# 
# ##### hold time density the same, different group CAFs
# caf.tmp<-data.frame(t=caf[[2]][,1],yhat=caf[[2]][,2])
# intdiff<-integrate(x$rt,caf.tmp)
# print(M)
# print(intdiff) #0.4766381
# 
# ##### time density of group 1, CAF for group 2
# caf.tmp<-data.frame(t=caf[[2]][,1],yhat=caf[[2]][,2])
# intdiff<-integrate(x1$rt,caf.tmp)
# print(M) #group 1 accuracy  0.5245244, group 2 accuracy 0.4228089
# #ability offset group 1 0.52552 group 2 0.9888178
# print(intdiff) #speed offset:0.5333175, ability offset: 0.9893992
# 
# ##### time density of group 2, CAF for group 2
# caf.tmp<-data.frame(t=caf[[2]][,1],yhat=caf[[2]][,2])
# intdiff<-integrate(x2$rt,caf.tmp)
# print(M) #group 1 accuracy  0.5245244, group 2 accuracy 0.4228089
# print(intdiff) # accuracy 0.4227697
# 
# ##### merged time density, CAF for group 2
# caf.tmp<-data.frame(t=caf[[2]][,1],yhat=caf[[2]][,2])
# intdiff<-integrate(x$rt,caf.tmp)
# print(M) #group 1 accuracy  0.5245244, group 2 accuracy 0.4228089
# print(intdiff) #  0.4766381
# 
# 
# ##### merged time density, CAF for group 1
# caf.tmp<-data.frame(t=caf[[1]][,1],yhat=caf[[1]][,2])
# intdiff<-integrate(x$rt,caf.tmp)
# print(M) #group 1 accuracy  0.5245244, group 2 accuracy 0.4228089
# print(intdiff) #  
# 
# 
# ##### time density of group 1, CAF for group 1
# caf.tmp<-data.frame(t=caf[[1]][,1],yhat=caf[[1]][,2])
# intdiff<-integrate(x1$rt,caf.tmp)
# print(M) #group 1 accuracy  0.5245244, group 2 accuracy 0.4228089
# print(intdiff) #0.523112
# 
# 
# 
# ##### time density of group 2, CAF for group 1
# caf.tmp<-data.frame(t=caf[[1]][,1],yhat=caf[[1]][,2])
# intdiff<-integrate(x2$rt,caf.tmp)
# print(M) #group 1 accuracy  0.5245244, group 2 accuracy 0.4228089
# print(intdiff) #
