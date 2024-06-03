library(PISAhelper)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(latex2exp)
setwd("/Users/radhika/Library/CloudStorage/GoogleDrive-rkap786@stanford.edu/My Drive/0. Projects - Stanford/PISA gender/Final code files revision/")
result=read.csv("Data/Sim data/result_v2.csv")
result = data.frame(result)
#names(result) = c("sno","N", "speed","b.time", "ability","group1", "group2", "intdiff_t1h2", "intdiff_t2h2", "intdiff_th2", "intdiff_th1", "intdiff_t1h1", "intdiff_t2h1")
#names(result) = c("N", "speed","b.time", "ability","group1", "group2", "intdiff_t1h2", "intdiff_th2", "intdiff_th1", "intdiff_t1h1", "intdiff_t2h1", "intdiff_t1h", "intdiff_t2h")
names(result)
###FIGURE 4



result = result |>
  mutate_if(is.character, as.numeric) |> 
  mutate(
    score_diff = group2-group1, 
    a3c = intdiff_th2 - intdiff_th1,
    a4s= intdiff_t2h - intdiff_t1h)


#lab1=expression(paste('A'[c]*minute, "( ",b[t]," = 0.5)") )  
# lab1= TeX("$\\widehat{A_s}$")
# labtitle2=expression(paste(b[t]," =0.5"))  
# labb=expression(paste(b[t]," =0.5")) 

plot_ability_adj= function(result, n=1000, bfilter=0.5) {
  lab1= TeX("$\\widehat{A_c}$")
  labb=expression(paste(b[t]," =0.5")) 
  
  labtitle1=expression(paste(b[t]," =0.5"))  
  labtitle2=expression(paste(b[t]," =0"))  
  labtitle3=expression(paste(b[t]," =-0.5"))  
  
  if(bfilter==0.5) {labb=labtitle1}
  if(bfilter==0) {labb=labtitle2}
  if(bfilter==-0.5) {labb=labtitle3}
  
plot1 = result |>
  filter(b.time==bfilter,
         N==n
         ) |>
  ggplot(aes(x=ability, y=a3c)) + geom_point(aes(color=factor(speed))) +
  scale_color_manual(name = expression(mu[tau]), values=c("red", "black", "blue")) +theme_classic() +
  scale_x_continuous(limits = c(0,0.25)) +
  scale_y_continuous(limits = c(-0.05,0.1)) +
  xlab(expression(mu[theta])) + ylab (lab1) +
  annotate(geom="text", x=0.05, y=0.08, label= labb, parse=T, size=4) +
  theme(axis.line.x  = element_line(),
        legend.position="bottom",
        strip.placement = "outside",
        strip.background = element_blank(),
        text = element_text(size=15)
  ) 
return(plot1)
}


plot_diff_adj= function(result, n=1000, bfilter=0.5) {
  lab1= TeX("$\\widehat{A_c}$")
  labb=expression(paste(b[t]," =0.5")) 
  
  labtitle1=expression(paste(b[t]," =0.5"))  
  labtitle2=expression(paste(b[t]," =0"))  
  labtitle3=expression(paste(b[t]," =-0.5"))  
  
  if(bfilter==0.5) {labb=labtitle1}
  if(bfilter==0) {labb=labtitle2}
  if(bfilter==-0.5) {labb=labtitle3}
  
  plot2 = result |>
    filter(b.time==bfilter,
           N==n) |>
    ggplot(aes(x=score_diff, y=a3c)) + geom_point(aes(color=factor(speed))) +
    scale_color_manual(name = expression(mu[tau]), values=c("red", "black", "blue")) +theme_classic() +
    xlab("Group score difference (A)") + ylab (lab1) +
    scale_x_continuous(limits = c(-0.1,0.1)) +
    scale_y_continuous(limits = c(-0.05,0.1)) +
    annotate(geom="text", x=-0.05, y=0.08, label= labb, parse=T, size=5) +
    theme(axis.line.x  = element_line(),
          legend.position="bottom",
          strip.placement = "outside",
          strip.background = element_blank(),
          text = element_text(size=15))
  return(plot2)
}

## Plot together
top_left= plot_ability_adj(result, n=1000, bfilter = 0.5)
top_mid= plot_ability_adj(result, n=1000, bfilter = 0)
top_rt= plot_ability_adj(result, n=1000, bfilter = -0.5)


bottom_left= plot_diff_adj(result, n=1000, bfilter = 0.5)
bottom_mid= plot_diff_adj(result, n=1000, bfilter = 0)
bottom_rt= plot_diff_adj(result, n=1000, bfilter = -0.5)

plot_demo2_row1= ggarrange(top_left, top_mid, top_rt, 
                           common.legend = T,legend="bottom",
                           align = "h",  nrow=1) 

plot_demo2_row2= ggarrange(bottom_left, bottom_mid, bottom_rt, 
                           common.legend = T,legend="bottom",
                           align = "h",  nrow=1) 


plot_demo2= ggarrange(plot_demo2_row1, plot_demo2_row2, ncol = 1)

plot_demo2= annotate_figure(plot_demo2,
                            bottom = text_grob("N=1000", 
                                               hjust =1.2, vjust=-2,x = 1, face = "italic", size = 10)) #expression("N=1000, "* b[t]*"=0.5")

ggsave("Figures/fig4_demo2_results_aac.jpeg", plot_demo2)

############ N=10000
top_left= plot_ability_adj(result, n=10000, bfilter = 0.5)
top_mid= plot_ability_adj(result, n=10000, bfilter = 0)
top_rt= plot_ability_adj(result, n=10000, bfilter = -0.5)


bottom_left= plot_diff_adj(result, n=10000, bfilter = 0.5)
bottom_mid= plot_diff_adj(result, n=10000, bfilter = 0)
bottom_rt= plot_diff_adj(result, n=10000, bfilter = -0.5)

plot_demo2_row1= ggarrange(top_left, top_mid, top_rt, 
                           common.legend = T,legend="bottom",
                           align = "h",  nrow=1) 

plot_demo2_row2= ggarrange(bottom_left, bottom_mid, bottom_rt, 
                           common.legend = T,legend="bottom",
                           align = "h",  nrow=1) 


#plot_demo2= ggarrange(plot_demo2_row1, plot_demo2_row2, ncol = 1)

plot_demo2= annotate_figure(plot_demo2_row2,
                            bottom = text_grob("N=10,000", 
                                               hjust =1.2, vjust=-2,x = 1, face = "italic", size = 10)) #expression("N=1000, "* b[t]*"=0.5")

ggsave("Figures/figA3a_demo2_results_aac_N10000.jpeg", plot_demo2, height=8,  width = 25, unit="cm")


# labtitle2=expression(paste(b[t],"=0"))  
# labb=expression(paste(b[t]," =0")) 
# 
# 
# plot_middle1_B = result |>
#   filter(b.time==0,
#          N==1000) |>
#   ggplot(aes(x=ability, y=a4s)) + geom_point(aes(color=factor(speed))) +
#   scale_color_manual(name = expression(mu[tau]), values=c("red", "black", "blue")) +theme_classic() +
#   scale_x_continuous(limits = c(0,0.25)) +
#   scale_y_continuous(limits = c(-0.05,0.1)) +
#   xlab(expression(mu[theta])) + ylab (lab1) +
#   annotate(geom="text", x=0.05, y=0.08, label= labb, parse=T, size=6) +
#   theme(axis.line.x  = element_line(),
#         legend.position="bottom",
#         strip.placement = "outside",
#         strip.background = element_blank(),
#         text = element_text(size=12)
#   ) 
# 
# plot_middle1_B_bottom = result |>
#   filter(b.time==0,
#          N==1000) |>
#   ggplot(aes(x=score_diff, y=a4s)) + geom_point(aes(color=factor(speed))) +
#   scale_color_manual(name = expression(mu[tau]), values=c("red", "black", "blue")) +theme_classic() +
#   xlab("Group score difference (A)") + ylab (lab1) +
#   scale_x_continuous(limits = c(-0.1,0.1)) +
#   scale_y_continuous(limits = c(-0.05,0.1)) +
#   theme(axis.line.x  = element_line(),
#         legend.position="bottom",
#         strip.placement = "outside",
#         strip.background = element_blank(),
#         text = element_text(size=12) 
#   ) #+scale_x_continuous(n.breaks=4) 
# 
# lab1=expression(paste('A'[s]*minute, "( ",b[t]," = -0.5)") )  
# plot_right1_B = result |>
#   filter(b.time==-0.5,
#          N==1000) |>
#   ggplot(aes(x=ability, y=a4s)) + geom_point(aes(color=factor(speed))) +
#   scale_color_manual(name = expression(mu[tau]), values=c("red", "black", "blue")) +theme_classic() +
#   xlab(expression(mu[theta])) + ylab (lab1) +
#   theme(axis.line.x  = element_line(),
#         legend.position="bottom",
#         strip.placement = "outside",
#         strip.background = element_blank(),
#         text = element_text(size=12)
#   ) 
# 
# plot_right1_B_bottom = result |>
#   filter(b.time==-0.5,
#          N==1000) |>
#   ggplot(aes(x=score_diff, y=a4s)) + geom_point(aes(color=factor(speed))) +
#   scale_color_manual(name = expression(mu[tau]), values=c("red", "black", "blue")) +theme_classic() +
#   xlab("Group score difference (A)") + ylab (lab1) +
#   theme(axis.line.x  = element_line(),
#         legend.position="bottom",
#         strip.placement = "outside",
#         strip.background = element_blank(),
#         text = element_text(size=12)
#   )  #+scale_x_continuous(n.breaks=4) 

