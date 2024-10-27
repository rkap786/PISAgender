library(PISAhelper)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(latex2exp)
setwd("/Users/radhika/Library/CloudStorage/GoogleDrive-rkap786@stanford.edu/My Drive/0. Projects - Stanford/PISA gender/Final code files revision/")
result=read.csv("Data/Sim data/result_v2.csv")
result = data.frame(result)
names(result)
# 
result = result |>
  mutate_if(is.character, as.numeric) |>
  mutate(
    score_diff = group2-group1,
    a1 = intdiff_t1h2 - intdiff_t1h1,
    a2 = intdiff_t2h1 - intdiff_t1h1,
    a3c = intdiff_th2 - intdiff_th1,
  a4s= intdiff_t2h - intdiff_t1h)


r.theta1= result |>
  filter(N==1000) |>
  with(cor(ability,score_diff))

r.speed= result |>
  filter(N==1000) |>
  with(cor(speed,score_diff))

txt1<-bquote("r("~theta~", A)="~.(round(r.theta1,2)))#txt1 <- expression(paste0("r(", theta, ",A)=",round(r.theta,2)))
txt2<-bquote("r("~mu~", A)="~.(round(r.speed,2)))#txt1 <- expression(paste0("r(", theta, ",A)=",round(r.theta,2)))
###############################################################
###############################################################
### Simulation figure: 3 [PART 1]

# r.theta=numeric(r.theta)
# txt1 = expression(paste("N=1000: r(theta, A)=",round(r.theta1,2))) #;","N=10,000: r(",theta,",A)=",0.72)
#txt2 = expression(paste("N=10,000: r(",theta,",A)=",0.72))
#txt= paste(txt1,";", txt2)
#lab1=expression(bold(paste("A ( ",b[t]," = 0.5)")))  

################

plotfig1 = function(result, bfilter, Nfilter) {
  labtitle1=expression(paste(b[t]," =0.5"))  
  labtitle2=expression(paste(b[t]," =0"))  
  labtitle3=expression(paste(b[t]," =-0.5"))  
  
  if(bfilter==0.5) {labb=labtitle1}
  if(bfilter==0) {labb=labtitle2}
  if(bfilter==-0.5) {labb=labtitle3}
  
  #labb= expression(paste("'b[t]', ", "= ",bfilter))
  #labb= bquote(list(b[t] == .(.bfilter)))
  
  #lab1=expression(bold("A"))  
  lab1= "Group score differences (A)"
  plot1 = result |>
    filter(b.time==bfilter,
           N==Nfilter) |>
    ggplot(aes(x=ability, y=score_diff)) + geom_point(aes(color=factor(speed))) +
    scale_color_manual(name = expression(mu[tau]), values=c("red", "black", "blue")) +theme_classic() +
    xlab(expression(mu[theta])) + ylab (lab1)  +  
    scale_x_continuous(limits = c(0,0.25)) +
    scale_y_continuous(limits = c(-0.05,0.1)) +
    theme(axis.line.x  = element_line(),
          legend.position="bottom",
          strip.placement = "outside",
          strip.background = element_blank(),
          text = element_text(size=15),
          axis.title = element_text(size=15)
    ) +annotate(geom="text", x=0.05, y=0.08, label= labb, size=6)
  
  return(plot1)
}
#lab1=expression(bold(paste("A ( ",b[t]," =0)")))


plot_left1 = plotfig1(result, bfilter=0.5, N=1000)
plot_middle1 =plotfig1(result, bfilter=0, N=1000)
plot_right1 =plotfig1(result, bfilter=-0.5, N=1000)

### Arrange

plot_demo1_v2= ggarrange(plot_left1, plot_middle1, plot_right1,
                      common.legend = T,
                      nrow=1,
                      align = "hv")

plot_demo1_v2= annotate_figure(plot_demo2,
                            bottom = text_grob("N=1000", 
                                               hjust =1.2, vjust=-2,x = 1, face = "italic", size = 10)) #expression("N=1000, "* b[t]*"=0.5")


ggsave("Figures/fig3_sim_N1000.jpeg", plot_demo1_v2, height = 8, units = "cm")



plot_left1 = plotfig1(result, bfilter=0.5, N=10000)
plot_middle1 =plotfig1(result, bfilter=0, N=10000)
plot_right1 =plotfig1(result, bfilter=-0.5, N=10000)

### Arrange

plot_demo1_v2= ggarrange(plot_left1, plot_middle1, plot_right1,
                         common.legend = T,
                         nrow=1,
                         align = "hv")

plot_demo1_v2= annotate_figure(plot_demo1_v2,
                            bottom = text_grob("N=10,000", 
                                               hjust =1.2, vjust=-2,x = 1, 
                                               face = "italic", size = 10)) 


ggsave("Figures/figA2_demo1_v2_N10000.jpeg", plot_demo1_v2, height = 8, units = "cm")
#ggsave("Figures/fig3_demo1_v2_N10000.jpeg", plot_demo1_v2)
################################################
################################################
################################################
# ### Simulation figure: 3 [PART 2]
# ### Y axis is the adjusted difference
# #r.theta=numeric(r.theta)
# #lab1=expression(paste('A'[c]*minute, "( ",b[t]," = 0.5)") )  
# 
# 
# plotfig2_top = function(result, bfilter, Nfilter=10000, afnc="a3c") {
#   labtitle1=expression(paste(b[t]," =0.5"))  
#   labtitle2=expression(paste(b[t]," =0"))  
#   labtitle3=expression(paste(b[t]," =-0.5"))  
#   
#   if(bfilter==0.5) {labb=labtitle1}
#   if(bfilter==0) {labb=labtitle2}
#   if(bfilter==-0.5) {labb=labtitle3}
#   if(afnc=="a3c") {ylabb= expression('A'[c]*hat)}
#   if(afnc=="a4s") {ylabb= expression('A'[s]*hat)}
#   
#   plot1 = result |>
#     filter(b.time==0.5,
#            N==1000) |>
#     ggplot(aes(x=ability, y=!!sym(afnc))) + geom_point(aes(color=factor(speed))) +
#     scale_x_continuous(limits = c(0,0.25)) +
#     scale_y_continuous(limits = c(-0.05,0.1)) +
#     scale_color_manual(name = expression(mu[tau]), values=c("red", "black", "blue")) +theme_classic() +
#     xlab(expression(mu[theta])) + ylab(ylabb) +
#     theme(axis.line.x  = element_line(),
#           legend.position="bottom",
#           strip.placement = "outside",
#           strip.background = element_blank(),
#           text = element_text(size=14)
#     ) +annotate(geom="text", x=0.05, y=0.08, label= labb, parse=T, size=3)
#   return(plot1)
# }
# 
# 
# 
# plotfig2_top = function(result, bfilter=0.5, Nfilter=1000, afnc="a3c") {
#   labtitle1=expression(paste(b[t]," =0.5"))  
#   labtitle2=expression(paste(b[t]," =0"))  
#   labtitle3=expression(paste(b[t]," =-0.5"))  
#   
#   
#   if(bfilter==0.5) {labb=labtitle1}
#   if(bfilter==0) {labb=labtitle2}
#   if(bfilter==-0.5) {labb=labtitle3}
#   
#   
#   if(afnc=="a3c") {ylabb= TeX("$\\widehat{A_c}$")}
#   if(afnc=="a4s") {ylabb= TeX("$\\widehat{A_s}$")}
#   #if(afnc=="a4s") {ylabb= expression('A'[s]*hat)}
#   
#   plot2 = result |>
#     filter(b.time==bfilter,
#            N==Nfilter) |>
#     ggplot(aes(x=score_diff, y=a4s)) + geom_point(aes(color=factor(speed))) +
#     scale_color_manual(name = expression(mu[tau]), values=c("red", "black", "blue")) +theme_classic() +
#     scale_x_continuous(limits = c(-0.1,0.1)) +
#     scale_y_continuous(limits = c(-0.05,0.1)) +
#     xlab("Group score difference (A)") + ylab(ylabb) +
#     theme(axis.line.x  = element_line(),
#           legend.position="bottom",
#           strip.placement = "outside",
#           strip.background = element_blank(),
#           text = element_text(size=14)
#     ) +annotate(geom="text", x=-0.05, y=0.08, label= labb, parse=T, size=6)
#   return(plot2)
#   
# }
# 
# 
# plot_left1_B= plotfig2_top(result, 0.5, 10000, "a3c")
# plot_middle1_B= plotfig2_top(result, 0, 10000, "a3c")
# plot_right1_B= plotfig2_top(result, -0.5, 10000, "a3c")
# plot_demo2_row1= ggarrange(plot_left1_B, plot_middle1_B, plot_right1_B, 
#                            common.legend = T,legend="bottom",
#                            align = "h",  nrow=1) 
# plot_left1_B_bottom=plotfig2_top(result,0.5,1000, "a3c")
# plot_middle1_B_bottom=plotfig2_top(result,0,1000, "a3c")
# plot_right1_B_bottom=plotfig2_top(result,-0.5,1000, "a3c")
# 
# plot_demo2_row2= ggarrange(plot_left1_B_bottom, plot_middle1_B_bottom, plot_right1_B_bottom, 
#                            common.legend = T,legend="bottom",
#                            align = "h",  nrow=1) 
# 
# 
# plot_demo2= ggarrange(plot_demo2_row1, plot_demo2_row2, ncol = 1)
# 
# plot_demo2= annotate_figure(plot_demo2,
#                             bottom = text_grob("N=1000", 
#                                                hjust =1.2, vjust=-2,x = 1, face = "italic", size = 10)) #expression("N=1000, "* b[t]*"=0.5")
# 
# ggsave("Figures/fig3_demo2_results_aac.jpeg", plot_demo2)
# 
# ############################################################################################################
# ################################### As
# ############################################################################################################
# plot_left1_B= plotfig2_top(result, 0.5, 10000, "a4s")
# plot_middle1_B= plotfig2_top(result, 0, 10000, "a4s")
# plot_right1_B= plotfig2_top(result, -0.5, 10000, "a4s")
# plot_demo2_row1= ggarrange(plot_left1_B, plot_middle1_B, plot_right1_B, 
#                            common.legend = T,legend="bottom",
#                            align = "h",  nrow=1) 
# plot_left1_B_bottom=plotfig2_top(result,0.5,1000, "a4s")
# plot_middle1_B_bottom=plotfig2_top(result,0,1000, "a4s")
# plot_right1_B_bottom=plotfig2_top(result,-0.5,1000, "a4s")
# 
# plot_demo2_row2= ggarrange(plot_left1_B_bottom, plot_middle1_B_bottom, plot_right1_B_bottom, 
#                            common.legend = T,legend="bottom",
#                            align = "h",  nrow=1) 
# 
# 
# plot_demo2= ggarrange(plot_demo2_row1, plot_demo2_row2, ncol = 1)
# 
# plot_demo2= annotate_figure(plot_demo2,
#                             bottom = text_grob("N=1000", 
#                                                hjust =1.2, vjust=-2,x = 1, face = "italic", size = 10)) #expression("N=1000, "* b[t]*"=0.5")
# 
# ggsave("Figures/fig3_demo2_results_aas.jpeg", plot_demo2)
# 
# #ggsave("Figures/fig3_demo2v2.jpeg", plot_demo2)
# 
# # 
# # plot_left2_B = result |>
# #   filter(b.time==0.5,
# #          N==10000) |>
# #   ggplot(aes(x=ability, y=a1)) + geom_point(aes(color=factor(speed))) +
# #   scale_color_manual(name = expression(mu[tau]), values=c("red", "black", "blue")) +theme_classic() +
# #   xlab(expression(mu[theta])) + ylab (lab1)  +
# #   facet_wrap(~N,
# #              strip.position = "left",
# #              labeller = labeller(
# #                N=c("1000"= "N=1000",
# #                    "10000"="N=10,000")),
# #              ncol=1
# #   ) +  
# #   theme(axis.line.x  = element_line(),
# #         legend.position="bottom",
# #         strip.placement = "outside",
# #         strip.background = element_blank(),
# #         text = element_text(size=10)
# #   ) 
# 
# 
# ################
# 
# # 
# # 
# # plot_middle2_B = result |>
# #   filter(b.time==0.5,
# #          N==10000) |>
# #   ggplot(aes(x=ability, y=a2)) + geom_point(aes(color=factor(speed))) +
# #   scale_color_manual(name = expression(mu[tau]), values=c("red", "black", "blue")) +theme_classic() +
# #   xlab(expression(mu[theta])) + ylab (lab1)  +
# #   facet_wrap(~N,
# #              strip.position = "left",
# #              labeller = labeller(
# #                N=c("1000"= "N=1000",
# #                    "10000"="N=10,000")
# #              ),
# #              ncol=1
# #   ) + #labs(caption = txt1) + 
# #   theme(axis.line.x  = element_line(),
# #         legend.position="bottom",
# #         strip.placement = "outside",
# #         strip.background = element_blank(),
# #         text = element_text(size=10)
# #   )
# 
# 
# ########################################################
# #lab1=expression(paste("A ( ",b[t]," =-0.5)"))  
# 
# # 
# # 
# # plot_right2_B = result |>
# #   filter(b.time==0.5,
# #          N==10000) |>
# #   ggplot(aes(x=score_diff, y=a1)) + geom_point(aes(color=factor(speed))) +
# #   scale_color_manual(name = expression(mu[tau]), values=c("red", "black", "blue")) +theme_classic() +
# #    xlab(lab1)  +ylab(lab2)  +
# #   facet_wrap(~N,
# #              strip.position = "left",
# #              labeller = labeller(
# #                N=c("1000"= "N=1000",
# #                    "10000"="N=10,000")
# #              ),
# #              ncol=1
# #   )  + 
# #   theme(axis.line.x  = element_line(),
# #         legend.position="bottom",
# #         strip.placement = "outside",
# #         strip.background = element_blank(),
# #         text = element_text(size=10)
# #   )
# 
# ####################################
# 
# # 
# # plot_vright2 = result |>
# #   filter(b.time==0.5,
# #          N==10000) |>
# #   ggplot(aes(x=score_diff, y=a2)) + geom_point(aes(color=factor(speed))) +
# #   scale_color_manual(name = expression(mu[tau]), values=c("red", "black", "blue")) +theme_classic() +
# #    xlab(lab1)  +ylab(lab2)  +
# #   facet_wrap(~N,
# #              strip.position = "left",
# #              labeller = labeller(
# #                N=c("1000"= "N=1000",
# #                    "10000"="N=10,000")
# #              ),
# #              ncol=1
# #   )  + 
# #   theme(axis.line.x  = element_line(),
# #         legend.position="bottom",
# #         strip.placement = "outside",
# #         strip.background = element_blank(),
# #         text = element_text(size=10)
# #   )
# 
# 
# ### Arrange
# 
