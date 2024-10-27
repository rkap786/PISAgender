## function for running pisahelper
library(readr)
library(PISAhelper)
library(tidyverse)
library(fs)
library(ggpubr)
library(dplyr)
library(ggplot2)
library(ggrepel)
setwd("/Users/radhika/Library/CloudStorage/GoogleDrive-rkap786@stanford.edu/My Drive/0. Projects - Stanford/PISA gender/Final code files revision/")

source("Code/PISAgender/PISAhelper/R/scorediff.R")
source("Code/PISAgender/PISAhelper/R/getcaf_deg.R")
source("Code/PISAgender/PISAhelper/R/getcaf0_deg.R")
source("Code/PISAgender/PISA_dataprep_funcs.R")
source("Code/PISAgender/PISAhelper/R/integrate.R")
source("Code/PISAgender/analysis_fncs.R")


##########################################
load("Data/Final data/read_fluency.Rdata")
#load("/Users/radhika/Google Drive Stanford/PISA gender/Data subsets and Code/country datasets/read_comprehension.Rdata")
#load("/Users/radhika/Google Drive Stanford/PISA gender/Data subsets and Code/country datasets/math.Rdata")


## Statistics for each country
nstudents=as.data.frame(do.call(rbind, lapply(read_rf, function(x) n_distinct(x$id))))
meangender =as.data.frame(do.call(rbind, lapply(read_rf, function(x) mean(x$gender,na.rm=T))))
mean(as.vector(meangender)$V1)
meanresp =as.data.frame(do.call(rbind, lapply(read_rf, function(x) mean(x$resp,na.rm=T))))
mean(as.vector(meanresp)$V1)

nitems=as.data.frame(do.call(rbind, lapply(read_rf, function(x) n_distinct(x$item))))


rtdist=do.call(rbind, lapply(read_rf, function(x) summary(x$rt)))
mean(rtdist[,3]) #mean of median
log(mean(rtdist[,3])) #mean of median

summary(do.call(rbind, lapply(read_rf, function(x) quantile(x$rt,0.01))))
summary(exp(do.call(rbind, lapply(read_rf, function(x) quantile(x$rt,0.99)))))




#### estimate CAFs and adjusted difference
  
rtdist=c()  
out=data.frame()

for (i in 1:length(read_rf)){
  xfull= read_rf[[i]]
  xfull= filterdata(xfull, qu=0.99, ql=0.01) #drop 1% and log rt
  tmp=runpisahelper(xfull, "fluency")
  
  ## Time difference
  rt1= xfull |> filter(gender==1) |> pull(rt)
  rt2= xfull |> filter(gender==2) |> pull(rt)
  rtdist=c(rtdist,mean(kld_base(rt1,rt2), kld_base(rt2,rt1)))
  
  out=bind_rows(out, unlist(tmp))
}


out[,c(1:4)] <- sapply(out[,c(1:4)],as.numeric)
out=out |>  mutate(obvdir= (actualdiff-a_c >=0), truedir= (`est diff`-a_c >=0), match=obvdir==truedir)
out$err= out$actualdiff-out$`est diff`
out$rtdist= rtdist
summary(out$actualdiff)
summary(out$rtdist)


cntcode= read_csv("Data/Final data/pisa_country_codes.csv")
out= merge(out, cntcode)
write_csv(out,"Data/Final data/readrf_results_q99q1.csv")

out= read_csv("Data/Final data/readrf_results_q99q1.csv")


lab1= TeX("$\\widehat{A_c}$")

g1= out |>
  filter(subject=="fluency") |>
  mutate(adjusted = -as.numeric(a_c),
         actual=-as.numeric(actualdiff),
         As= -as.numeric(a_s)
  ) |> #col= if_else(actual<adjusted, "blue", if_else(actual==adjusted,"black","red")),  #subject=factor(subject, levels=c("reading fluency", "reading other", "math")
  mutate(cntlab = ifelse(cnt=="NLD"| cnt=="QMR" | cnt=="ARE" | cnt=="BRA", cnt, "")) |>
  ggplot(aes(x=actual, y= adjusted,col=cntlab, label=cntlab)) +
  geom_point() + 
  scale_color_manual(values =c("black","blue", "blue", "blue", "blue"), guide="none") +
  geom_abline(intercept = 0, slope = 1,linetype="dotted") + 
  geom_hline(yintercept = 0, linetype="dotted") +
  theme_classic(base_size = 15) + #geom_text(hjust=1.5, vjust=0) + 
  geom_label_repel(box.padding = 0.5, max.overlaps = Inf) + 
  labs(x= "A (females-males)", y=lab1) 


figures="/Users/radhika/Library/CloudStorage/GoogleDrive-rkap786@stanford.edu/My Drive/0. Projects - Stanford/PISA gender/Final code files revision/Figures/"
ggsave(paste0(figures,"CAF_gminusb_fluency",".png"), plot=g1, height = 8, unit="cm")


lab1= TeX("$\\widehat{A_s}$")
g2= out |>
  filter(subject=="fluency") |>
  mutate(adjusted = -as.numeric(a_c),
         actual=-as.numeric(actualdiff),
         As= -as.numeric(a_s)
  ) |> #col= if_else(actual<adjusted, "blue", if_else(actual==adjusted,"black","red")),  #subject=factor(subject, levels=c("reading fluency", "reading other", "math")
  mutate(cntlab = ifelse(cnt=="NLD"| cnt=="QMR" | cnt=="ARE" | cnt=="BRA", cnt, "")) |>
  ggplot(aes(x=actual, y= As, col=cntlab, label=cntlab)) +
  geom_point() + 
  scale_color_manual(values =c("black","blue", "blue", "blue", "blue"), guide="none") +
  geom_abline(intercept = 0, slope = 1,linetype="dotted") + 
  geom_hline(yintercept = 0, linetype="dotted") +
  theme_classic(base_size = 14) + #geom_text(hjust=1.5, vjust=0) +
  geom_label_repel(box.padding = 0.5, max.overlaps = Inf) +
  labs(x= "A (females-males)", y=lab1) 


ggsave(paste0(figures,"CAF_gminusb_fluency_As",".png"), plot=g2, height = 8, unit="cm")



g3= out |>
  filter(subject=="fluency") |>
  mutate(adjusted = -as.numeric(a_c),
         actual=-as.numeric(actualdiff),
         As= -as.numeric(a_s)
  ) |> #col= if_else(actual<adjusted, "blue", if_else(actual==adjusted,"black","red")),  #subject=factor(subject, levels=c("reading fluency", "reading other", "math")
  mutate(cntlab = ifelse(cnt=="NLD"| cnt=="QMR" | cnt=="ARE" | cnt=="BRA", cnt, "")) |>
  ggplot(aes(x=actual, y= As,col=cntlab, label=cntlab)) +
  geom_point() + 
  scale_color_manual(values =c("black","blue", "blue","blue","blue"), guide="none") +
  geom_hline(yintercept = 0, linetype="dotted") + #geom_text(hjust=1.5, vjust=0) +
  theme_classic(base_size = 14) + 
  geom_label_repel(box.padding = 0.5, max.overlaps = Inf) +
  labs(x= "Mean score difference (females- males)", y="A_s (Girls-Boys)") 


ggsave(paste0(figures,"CAF_gminusb_fluency_As",".png"), plot=g3)


### Summary stats
mean(out$actualdiff)
median(out$actualdiff)
sum(out$N)
mean(out$actualdiff)*100
median(out$actualdiff)*100
quantile(out$actualdiff,.75)*100
quantile(out$actualdiff,.25)*100


################################################
# Graphs on full data
################################################
# library(readr)
# # out = read_csv("/Users/radhika/Google Drive Stanford/PISA gender/Data subsets and Code/group_diff_adjusted.csv")
# names(out)
# #p1= plotpisa(out)
# # p2= plotpisa(out, "reading other")
# # p3= plotpisa(out, "math")
# # ggarrange(p1,p2,p3,
# #           common.legend = T,legend="none",
# #           align = "h", nrow=3) 
# 
# 
# ###### Plot  caf
# plots=list()
# for (i in 1:length(read_rf)) {
#   x=read_rf[[1]]
#   x$group= x$gender
#   caf= getcaf(x, deg=2)
#   plots=plotcaf(caf, unique(x$CNT))
# }




### Code to save country CAF plots
# out |>
#   filter(subject=="reading fluency") |>
#   mutate(adjusted = -as.numeric(Ac),
#          actual=-as.numeric(A),
#          As= -as.numeric(As)
#   ) |> #col= if_else(actual<adjusted, "blue", if_else(actual==adjusted,"black","red")),  #subject=factor(subject, levels=c("reading fluency", "reading other", "math")
#   ggplot(aes(x=actual, y= adjusted, label=cnt)) +
#   geom_point() +
#   facet_wrap(.~subject, nrow = 1) +
#   geom_abline(intercept = 0, slope =1,linetype="dotted") +
#   labs(x= "Mean score difference (Girls- Boys)", y="Adjusted difference (Girls-Boys)") +
#   theme_minimal()
# 
# out |>
#   filter(subject=="reading fluency") |>
#   mutate(adjusted = -as.numeric(adjc.a_c),
#          actual=-as.numeric(obs),
#          estimated= -as.numeric(`estdiff.est diff`)
#   ) |> #col= if_else(actual<adjusted, "blue", if_else(actual==adjusted,"black","red")),  #subject=factor(subject, levels=c("reading fluency", "reading other", "math")
#   ggplot(aes(x=actual, y= adjusted, label=cnt)) +
#   geom_point() +
#   facet_wrap(.~subject, nrow = 1) +
#   geom_abline(intercept = 0, slope =1,linetype="dotted") +
#   labs(x= "Mean score difference (Girls- Boys)", y="Adjusted difference (Girls-Boys)") +
#   theme_minimal()
# 
# 
# g1= out |>
#   filter(subject=="reading fluency") |>
#   mutate(adjusted = as.numeric(adjc.a_c),
#          actual=as.numeric(obs),
#          ) |> #col= if_else(actual<adjusted, "blue", if_else(actual==adjusted,"black","red")),  #subject=factor(subject, levels=c("reading fluency", "reading other", "math")
#   ggplot(aes(x=actual, y= adjusted, label=cnt)) +
#   geom_point() +
#   facet_wrap(.~subject, nrow = 1) +
#   ylim(-0.1,0.1) +xlim(-0.1,0.08) +
#   geom_abline(intercept = 0, slope = 1,linetype="dotted") +
#   geom_vline(xintercept = 0, linetype="dotted") + theme_classic(base_size = 8) +
#   theme(legend.position = "none") +
#   labs(x ="Observed difference A (female-male)", y = "Adjusted difference A' (female-male)")  +
#   scale_color_manual(values=c("blue", "red")) +
#   ggtitle("Adjusted vs observed score")
# 
# 
# summary(country_desc$resp_diff)
# 
# g4= country_desc %>% 
#   mutate(resp_diff=as.numeric(resp_diff),
#          subject=factor(subject, levels=c("reading fluency", "reading other", "math"))
#          ) %>%
#   ggplot(aes(x=resp_diff)) +
#   geom_density() +
#   facet_wrap(.~subject) +
#   geom_vline(xintercept =0, linetype="dashed") + theme_classic(base_size = 8) +
#   theme(legend.position="bottom", axis.title = element_text(size=8),
#         axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
#   ggtitle("Observed score difference") + theme_classic(base_size = 8) +
#   xlab("A (female-male)") + ylab("Density")
# 
# 
# 
# g5= country_desc %>%
#   mutate(rt_diff= as.numeric(rt_diff),
#          subject=factor(subject, levels=c("reading fluency", "reading other", "math"))
#          ) %>%
#   ggplot(aes(x=rt_diff)) +
#   geom_density() +
#   facet_wrap(.~subject) +
#   geom_vline(xintercept =0, linetype="dashed") + theme_classic(base_size = 8) +
#   theme(legend.position="bottom", axis.title = element_text(size=8),
#         axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
#   ggtitle("Log median response time") +
#   xlab("Log response time difference (female-male)") + ylab("Density")
# 
# ggarrange(g5,g4,g1, nrow=3, align = "h",widths = c(1.5,2))
# 

################################################
# Graphs on sample data
################################################


############# Code for setting up pisa data
# 
# 
# f <- dir_ls('/Users/radhika/Google Drive Stanford/PISA gender/Data subsets and Code/country datasets/')
# d <- list()
# n=length(f)
# for (i in 1:n){
#   d[[i]] <- read_csv(f[i])
# }
# 
# 
# #gender datafile
# gen<-read_csv("pisa_stu_gender.csv")
# colnamesgen= names(gen)
# colnamesgen[4] = "gender"
# names(gen)  = colnamesgen

###########
## Pull data we want to use from country level PISA files + add gender

# read_rf= list()
# read_rc= list()
# math= list()
# counter=1
# n=length(f)
# 
# for (i in 2:n){
#   pisa_sample <- read_csv(f[i])
#   pisa_math = extract_math_country(pisa_sample)
#   pisa_math= addgender(pisa_math, gen)
#   pisa_read= extract_read_country(pisa_sample) ##first dataset is reading fluency, second is other items called reading comprehension here
#   rf= addgender(pisa_read[[1]], gen)
#   rc= addgender(pisa_read[[2]], gen)
# 
#   if(!(identical(pisa_math$itemno, character(0)) | is.null(pisa_math$itemno) | identical(rf$itemno, character(0)) | is.null(rf$itemno) | identical(rc$itemno, character(0)) | is.null(rc$itemno))) {
#   pisa_math= clean(pisa_math)
#   rf=clean(rf)
#   rc=clean(rc)
#   read_rf[[counter]]= rf
#   read_rc[[counter]]=rc
#   math[[counter]] =pisa_math
#   counter=counter+1
#   }
# }


# 
# save(read_rf, file="country datasets/read_fluency.Rdata")
# save(read_rc, file="country datasets/read_comprehension.Rdata")
# save(math, file="country datasets/math.Rdata")
# 
# 
# pisa_math_gender = read_csv("Final datasets/pisa_math_gender.csv")
# pisa_read_gender = read_csv("Final datasets/pisa_read_gender.csv")
# names(pisa_read_gender)
# 
# pisa_read_rf= pisa_read_gender |>
#   filter(str_detect(item, "CR590"))
# 
# pisa_read_rc= pisa_read_gender |>
#   filter(!str_detect(item, "CR590"))  
# 
# ### append into one
# pisa_read_rf$subject= "reading fluency"
# pisa_read_rc$subject= "reading other"
# pisa_math_gender$subject= "math"
# pisa_math_gender= pisa_math_gender |>
#   select(-F) 
# names(pisa_math_gender)==names(pisa_read_rf)
# names(pisa_math_gender)==names(pisa_read_rc)
# 
# pisa_gender=rbind(pisa_read_rf, pisa_read_rc, pisa_math_gender)
# n_distinct(pisa_gender$id) #45413 students
# n_distinct(pisa_gender$CNT) #71 countries 
# ### aggregate graphs
# # x=pisa_gender %>%
# #   filter(!is.na(rt)) %>%
# #   filter(!is.na(gender))  %>%
# #   mutate(gender=if_else(gender==1, "female", "male")) %>%
# #   group_by(subject, gender) %>%
# #   summarise(median_time=median(rt), mean_score= mean(resp, na.rm=T)) 
# 
# # g1_1= pisa_gender %>%
# #   filter(!is.na(rt), !is.na(gender)) %>%
# #   mutate(gender_str= if_else(gender==1, "Female", "Male"),
# #          subject=factor(subject, levels=c("reading fluency", "reading other", "math"))) %>%
# #   ggplot(
# #     aes(x=rt, group=as.factor(gender_str), color=as.factor(gender_str))
# #   ) +
# #   geom_density() +
# #   facet_wrap(.~subject)  + theme_classic() +
# #   theme(legend.position="bottom", axis.title = element_text(size=8)) +
# #   labs(color="Gender") +
# #   xlab("Log response time") + ylab("Density")  
# # 
# # g1_2= pisa_gender %>%
# #   filter(!is.na(rt)) %>%
# #   filter(!is.na(gender))  %>%
# #   mutate(gender=if_else(gender==1, "Female", "Male")) %>%
# #   group_by(subject, item, gender) %>%
# #   summarise(mean_score= mean(resp)) %>%
#   pivot_wider(names_from = gender,
#               values_from= c(mean_score)) %>%
#   mutate(
#     diff_score= Female-Male,
#     subject=factor(subject, levels=c("reading fluency", "reading other", "math"))
#   ) %>%
#   ggplot(aes(x=diff_score, color=subject)) +
#   geom_density()  +
#   geom_vline(xintercept =0, linetype="dashed") + theme_classic(base_size = 8) +
#   theme(legend.position="bottom", axis.title = element_text(size=8),
#         axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
#   xlab("Observed score difference A (female-male)") + ylab("Density") 
# 
# g1_3= pisa_gender %>%
#   filter(!is.na(rt)) %>%
#   filter(!is.na(gender))  %>%
#   mutate(gender=if_else(gender==1, "Female", "Male")) %>%
#   group_by(subject, item, gender) %>%
#   summarise(median_rt= median(rt)) %>%
#   pivot_wider(names_from = gender,
#               values_from= c(median_rt)) %>%
#   mutate(
#     diff_time= Female-Male,
#     subject=factor(subject, levels=c("reading fluency", "reading other", "math"))
#   ) %>%
#   ggplot(aes(x=diff_time, color=subject)) +
#   geom_density()  +
#   geom_vline(xintercept =0, linetype="dashed") + theme_classic(base_size = 8) +
#   theme(legend.position="bottom", axis.title = element_text(size=8),
#         axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
#   xlab("Median time spent on item difference (female-male)") + ylab("Density") 
# 
# ggarrange(g1_1, ggarrange(g1_2, g1_3, nrow=1), nrow=2)
