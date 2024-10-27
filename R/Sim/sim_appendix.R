library(PISAhelper)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(latex2exp)
setwd("/Users/radhika/Library/CloudStorage/GoogleDrive-rkap786@stanford.edu/My Drive/0. Projects - Stanford/PISA gender/Final code files revision/")
result=read.csv("Data/Sim data/result_v3.csv")
result = data.frame(result)
#names(result) = c("sno","N", "speed","b.time", "ability","group1", "group2", "intdiff_t1h2", "intdiff_t2h2", "intdiff_th2", "intdiff_th1", "intdiff_t1h1", "intdiff_t2h1")
#names(result) = c("N", "speed","b.time", "ability","group1", "group2", "intdiff_t1h2", "intdiff_th2", "intdiff_th1", "intdiff_t1h1", "intdiff_t2h1", "intdiff_t1h", "intdiff_t2h")
names(result)
###FIGURE 4

result = result |>
  filter(N==1000) |>
  mutate_if(is.character, as.numeric) |> 
  mutate(
    score_diff = group2-group1, 
    a3c = intdiff_th2 - intdiff_th1,
    a4s= intdiff_t2h - intdiff_t1h)


#lab1=expression(paste('A'[c]*minute, "( ",b[t]," = 0.5)") )  
# lab1= TeX("$\\widehat{A_s}$")
# labtitle2=expression(paste(b[t]," =0.5"))  
# labb=expression(paste(b[t]," =0.5")) 

### Change with rho
  lab1= TeX("$\\widehat{A_c}$")
  

  plot1 = result |>
    filter(b.time==0.5,
           nitems==45,
           speed==0.5
    ) |>
    mutate(speed.label= paste0("speed = ", speed)) |>
    ggplot(aes(x=ability, y=a3c)) + geom_point(aes(color=factor(rho))) +
    scale_color_manual(name = expression(rho), values=c("red", "black", "blue")) +theme_classic() +
    scale_x_continuous(limits = c(0,0.25)) +
    scale_y_continuous(limits = c(-0.05,0.1)) +
    xlab(expression(mu[theta])) + ylab (lab1) +
    theme(axis.line.x  = element_line(),
          legend.position="bottom",
          strip.placement = "outside",
          strip.background = element_blank(),
          text = element_text(size=15)
    ) #annotate(geom="text", x=0.05, y=0.08, label= labb, parse=T, size=4) +

  plot2 = result |>
    filter(b.time==0.5,
           nitems==45,
           speed==0.5
    ) |>
    mutate(speed.label= paste0("speed = ", speed)) |>
    ggplot(aes(x=score_diff, y=a3c)) + geom_point(aes(color=factor(rho))) +
    scale_color_manual(name = expression(rho), values=c("red", "black", "blue")) +theme_classic() +
    scale_x_continuous(limits = c(-0.1,0.1)) +
    scale_y_continuous(limits = c(-0.05,0.1)) +
    xlab("Group score difference (A)") + ylab (lab1) +
    theme(axis.line.x  = element_line(),
          legend.position="bottom",
          strip.placement = "outside",
          strip.background = element_blank(),
          text = element_text(size=15)) #annotate(geom="text", x=-0.07, y=0.07, label= labb, parse=T, size=4) +
  
  
plot_row= ggarrange(plot1, plot2, 
                             common.legend = T,legend="bottom",
                             align = "h",  nrow=1) 
  
labb=expression(paste("N=1000; ",b[t]," =0.5", "; ", mu[t], " =0.5; ", "N items= 45")) 
plot_row= annotate_figure(plot_row,
                              bottom = text_grob(labb, 
                                                 hjust =1.2, vjust=0,x = 1, face = "italic", size = 10)) #expression("N=1000, "* b[t]*"=0.5")
  
ggsave("Figures/fig_appdx_rho_var.jpeg", plot_row, height=7, unit="cm")


##########################

  
result=read.csv("Data/Sim data/result_v3.csv")
result = data.frame(result)
#names(result) = c("sno","N", "speed","b.time", "ability","group1", "group2", "intdiff_t1h2", "intdiff_t2h2", "intdiff_th2", "intdiff_th1", "intdiff_t1h1", "intdiff_t2h1")
#names(result) = c("N", "speed","b.time", "ability","group1", "group2", "intdiff_t1h2", "intdiff_th2", "intdiff_th1", "intdiff_t1h1", "intdiff_t2h1", "intdiff_t1h", "intdiff_t2h")
names(result)
###FIGURE 4

result = result |>
  filter(N==1000) |>
  mutate_if(is.character, as.numeric) |> 
  mutate(
    score_diff = group2-group1, 
    a3c = intdiff_th2 - intdiff_th1,
    a4s= intdiff_t2h - intdiff_t1h)


#lab1=expression(paste('A'[c]*minute, "( ",b[t]," = 0.5)") )  
# lab1= TeX("$\\widehat{A_s}$")
# labtitle2=expression(paste(b[t]," =0.5"))  
# labb=expression(paste(b[t]," =0.5")) 

### Change with rho
lab1= TeX("$\\widehat{A_c}$")


plot1 = result |>
  filter(b.time==0.5,
         speed==0.5,
         rho==0.5
  ) |>
  ggplot(aes(x=ability, y=a3c, group=factor(nitems))) + geom_point(aes(color=factor(nitems))) +
  scale_color_manual(name = "Number of items", values=c("red", "black", "blue")) +theme_classic() +
  scale_x_continuous(limits = c(0,0.25)) +
  scale_y_continuous(limits = c(-0.05,0.1)) +
  xlab(expression(mu[theta])) + ylab (lab1) +
  theme(axis.line.x  = element_line(),
        legend.position="bottom",
        strip.placement = "outside",
        strip.background = element_blank(),
        text = element_text(size=15)
  ) #annotate(geom="text", x=0.05, y=0.08, label= labb, parse=T, size=4) +

plot2 = result |>
  filter(b.time==0.5,
         speed==0.5,
         rho==0.5
  ) |>
  ggplot(aes(x=score_diff, y=a3c, group=factor(nitems))) + geom_point(aes(color=factor(nitems))) +
  scale_color_manual(name = "Number of items", values=c("red", "black", "blue")) +theme_classic() +
  scale_x_continuous(limits = c(-0.1,0.1)) +
  scale_y_continuous(limits = c(-0.05,0.1)) +
  xlab("Group score difference (A)") + ylab (lab1) +
  theme(axis.line.x  = element_line(),
        legend.position="bottom",
        strip.placement = "outside",
        strip.background = element_blank(),
        text = element_text(size=15)
  ) #annotate(geom="text", x=0.05, y=0.08, label= labb, parse=T, size=4) +


plot_row= ggarrange(plot1, plot2, 
                    common.legend = T,legend="bottom",
                    align = "h",  nrow=1) 

labb=expression(paste("N=1000; ",b[t]," =0.5", "; ", mu[t], " =0.5; ", rho, "=0.5")) 
plot_row= annotate_figure(plot_row,
                          bottom = text_grob(labb, 
                                             hjust =1.2, vjust=0,x = 1, face = "italic", size = 10)) #expression("N=1000, "* b[t]*"=0.5")

ggsave("Figures/fig_appdx_nitems_var.jpeg", plot_row,height=7, unit="cm")

########### rho variation
result=read.csv("Data/Sim data/result_v3_rho.csv")
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

### Change with rho
lab1= TeX("$\\widehat{A_c}$")
lab2=TeX("$\\rho_1$")
lab3=TeX("$\\rho_2$")

plot1 = result |>
  filter(rho2==0) |>
  ggplot(aes(x=ability, y=a3c, group=factor(rho1))) + geom_point(aes(color=factor(rho1))) +
  scale_color_manual(name = lab2, values=c("red", "black", "blue")) +theme_classic() +
  scale_x_continuous(limits = c(0,0.25)) +
  scale_y_continuous(limits = c(-0.05,0.1)) +
  xlab(expression(mu[theta])) + ylab (lab1) +
  theme(axis.line.x  = element_line(),
        legend.position="bottom",
        strip.placement = "outside",
        strip.background = element_blank(),
        text = element_text(size=15)
  ) #annotate(geom="text", x=0.05, y=0.08, label= labb, parse=T, size=4) +

plot2 = result |>
  filter(rho2==0) |>
  ggplot(aes(x=score_diff, y=a3c, group=factor(rho1))) + geom_point(aes(color=factor(rho1))) +
  scale_color_manual(name = lab2, values=c("red", "black", "blue")) +theme_classic() +
  scale_x_continuous(limits = c(-0.1,0.1)) +
  scale_y_continuous(limits = c(-0.05,0.1)) +
  xlab("Group score difference (A)") + ylab (lab1) +
  theme(axis.line.x  = element_line(),
        legend.position="bottom",
        strip.placement = "outside",
        strip.background = element_blank(),
        text = element_text(size=15)
  ) #annotate(geom="text", x=0.05, y=0.08, label= labb, parse=T, size=4) +


plot_row= ggarrange(plot1, plot2, 
                    common.legend = T,legend="bottom",
                    align = "h",  nrow=1) 

labb=expression(paste("N=1000; ",b[t]," =0.5", "; ", mu[t], " =0.5; ", rho[2], "=0")) 
plot_row= annotate_figure(plot_row,
                          bottom = text_grob(labb, 
                                             hjust =1.2, vjust=0,x = 1, face = "italic", size = 10)) #expression("N=1000, "* b[t]*"=0.5")

ggsave("Figures/fig_appdx_rho_var.jpeg", plot_row,height=7, unit="cm")

