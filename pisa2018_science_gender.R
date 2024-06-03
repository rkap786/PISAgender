## function for running pisahelper
library(readr)
library(PISAhelper)
library(tidyverse)
library(fs)
library(ggpubr)
library(dplyr)
library(ggplot2)
setwd("/Users/radhika/Library/CloudStorage/GoogleDrive-rkap786@stanford.edu/My Drive/0. Projects - Stanford/PISA gender/Final code files revision/")

source("Code/PISAhelper/R/scorediff.R")
source("Code/PISAhelper/R/getcaf_deg.R")
source("Code/PISAhelper/R/getcaf0_deg.R")
source("Code/PISA_dataprep_funcs.R")
source("Code/PISAhelper/R/integrate.R")
source("Code/analysis_fncs.R")
#source("pisa scorediff plot oct22.R")
#source("Updated code/caf estimation.R")
#source("Updated code/plot_caftime.R")

##########################################
#load("Data/Final data/read_fluency.Rdata")
load("Data/Final data/science.Rdata")
#load("/Users/radhika/Google Drive Stanford/PISA gender/Data subsets and Code/country datasets/read_comprehension.Rdata")
#load("/Users/radhika/Google Drive Stanford/PISA gender/Data subsets and Code/country datasets/math.Rdata")
#load("/Users/radhika/Google Drive Stanford/PISA gender/Data subsets and Code/country datasets/science.Rdata")


## Statistics for each country
# science_clean=list()
# counter=1
# for (i in 1:length(science)) {
#   if(!is.null(science[[i]])){
#     science_clean[[counter]]= science[[i]]
#     counter=counter+1
#   } 
# }
# rm(science)

nstudents=as.data.frame(do.call(rbind, lapply(science, function(x) n_distinct(x$id))))
sum(nstudents)
nitems=as.data.frame(do.call(rbind, lapply(science, function(x) n_distinct(x$item))))

meangender =as.data.frame(do.call(rbind, lapply(science, function(x) mean(x$gender,na.rm=T))))
mean(as.vector(meangender)$V1)
meanresp =as.data.frame(do.call(rbind, lapply(science, function(x) mean(x$resp,na.rm=T))))
mean(as.vector(meanresp)$V1)

nitems=as.data.frame(do.call(rbind, lapply(science, function(x) n_distinct(x$item))))


rtdist=do.call(rbind, lapply(science, function(x) summary(x$rt)))
mean(rtdist[,3])
summary(do.call(rbind, lapply(science, function(x) quantile(x$rt,0.01))))
summary(do.call(rbind, lapply(science, function(x) quantile(x$rt,0.99))))

for (i in 1:length(science)) {
  x=science[[i]]
  x|> group_by(gender) |>
    summarise(mean(rt, na.rm=T), quantile(rt,0.25))
  
}

rtdist=do.call(rbind, lapply(science, function(x) summary(x$rt)))




#######Sensititvity check
#quset= c(quantile(x$rt, 0.90), quantile(x$rt, 0.95), 190, quantile(x$rt, 0.99), 200, max(x$rt))
#qlset= c(1, quantile(x$rt, 0.01), 3, quantile(x$rt, 0.05), quantile(x$rt, 0.1))

rtdist=c()  
out=data.frame()
for (i in 1:length(science)){
  print(i)
  xfull= science[[i]]
  xfull= xfull |>
    filter(BOOKID>=13 & BOOKID<=18) 
  
  xfull= filterdata(xfull, qu=0.99, ql=0.01) #drop 1% and log rt
  tmp=runpisahelper(xfull, "science")
  out=bind_rows(out, unlist(tmp))
  
  ## Time difference
  rt1= xfull |> filter(gender==1) |> pull(rt)
  rt2= xfull |> filter(gender==2) |> pull(rt)
  rtdist=c(rtdist,mean(kld_base(rt1,rt2), kld_base(rt2,rt1)))
}

out[,c(1:4)] <- sapply(out[,c(1:4)],as.numeric)
out=out |>  mutate(obvdir= (actualdiff-a_c >=0), truedir= (`est diff`-a_c >=0), match=obvdir==truedir)
out$err= out$actualdiff-out$`est diff`
out$rtdist= rtdist
cntcode= read_csv("Data/Final data/pisa_country_codes.csv")
out= merge(out, cntcode)
write_csv(out,"Data/Final data/science_book13to18_results_q99q1.csv")
#out2=read_csv("Data/Final data/science_book13to18_results_q99q1.csv") #quantile here was dropped before log rt
# rtdist=c()  
# out=data.frame()
# for (i in 1:length(science)){
#   print(i)
#   xfull= science[[i]]
#   xfull= xfull |>
#     filter(BOOKID>=13 & BOOKID<=18)
#   xfull= filterdata(xfull, qu=0.9, ql=0.1)
#   tmp=runpisahelper(xfull, "science")
#   out=bind_rows(out, unlist(tmp))
#   
#   ## Time difference
#   rt1= xfull |> filter(gender==1) |> pull(rt)
#   rt2= xfull |> filter(gender==2) |> pull(rt)
#   rtdist=c(rtdist,mean(kld_base(rt1,rt2), kld_base(rt2,rt1)))
# }
# out[,c(1:4)] <- sapply(out[,c(1:4)],as.numeric)
# out=out |>  mutate(obvdir= (actualdiff-a_c >=0), truedir= (`est diff`-a_c >=0), match=obvdir==truedir)
# out$err= out$actualdiff-out$`est diff`
# out$rtdist= rtdist
# write_csv(out,"Data/Final data/science_book13to18_results_q90q10.csv")

#### Load results
out= read_csv("Data/Final data/science_book13to18_results_q99q1.csv")
#out= out |> select(-cnt)


out = out |> 
  mutate(across(actualdiff:a_s, ~as.numeric(.x))) |>
  mutate(across(actualdiff:a_s, ~round(.x,4))) |>
  mutate(across(err:rtdist, ~round(.x,4))) 

out |> group_by(match) |> summarise(mean(rtdist))

out |>
  mutate(across(c("actualdiff", "est diff","a_c","a_s"), as.numeric)) |>
  mutate(adj_size = (actualdiff-a_c)/actualdiff) |>
  pull(adj_size) |>
  summary()

out |>
  mutate(across(c("actualdiff", "est diff","a_c","a_s"), as.numeric)) |>
  mutate(adj_size_s = (actualdiff-a_s)/actualdiff) |>
  pull(adj_size_s) |>
  summary()

## Difference
out |> mutate(diff=actualdiff-a_c) |> group_by(match) |> summarise(mean(as.numeric(diff)))

round(summary(as.numeric(out$actualdiff)),4)
out$diff_per=((out$actualdiff-out$a_c)/out$actualdiff)*100
round(summary(abs(out$err)),3)
summary(out$rtdist)

sum(out$N)
mean(out$actualdiff)*100
median(out$actualdiff)*100
quantile(out$actualdiff,.75)*100
quantile(out$actualdiff,.25)*100


figures="/Users/radhika/Library/CloudStorage/GoogleDrive-rkap786@stanford.edu/My Drive/0. Projects - Stanford/PISA gender/Final code files revision/Figures/"
lab1= TeX("$\\widehat{A_c}$")

g1= out |> #filter(subject=="science", err<0.02,rtdist>0.01) |>
  filter(subject=="science") |>
  filter(actualdiff>=0) |> 
  mutate(cntlab = ifelse(cnt=="QMR" | cnt=="BRA", cnt, "")) |>   #mutate(cntlab=cnt) |>mutate(cntlab= ifelse(match, "match", "notMatch")) |>
  mutate(adjusted = as.numeric(a_c),
         actual=as.numeric(actualdiff),
         As= as.numeric(a_s)
  ) |> #col= if_else(actual<adjusted, "blue", if_else(actual==adjusted,"black","red")),  #subject=factor(subject, levels=c("reading fluency", "reading other", "math")
  ggplot(aes(x=actual, y= adjusted, col=cntlab,label=cntlab)) +
  geom_point() + 
  scale_color_manual(values =c("black","blue", "blue"), guide="none") +
  geom_abline(intercept = 0, slope = 1,linetype="dotted") + 
  geom_hline(yintercept = 0, linetype="dotted") +
  theme_classic(base_size = 24) + geom_text(hjust=0.5, vjust=-0.5, size=8) + 
  labs(x= "A (Boys- Girls)", y=lab1) 

ggsave(paste0(figures,"CAF_bminusg_science",".png"), plot=g1)

g2= out |>
  filter(subject=="science") |> #, abs(err)<0.003,rtdist>0.01
  filter(actualdiff<0) |>
  mutate(cntlab = ifelse(cnt=="NLD"| cnt=="ARE", cnt, "")) |> #mutate(cntlab= ifelse(match, "match", "nM")) |>
  mutate(adjusted = -as.numeric(a_c),
         actual=-as.numeric(actualdiff),
         As= -as.numeric(a_s)
  ) |> #col= if_else(actual<adjusted, "blue", if_else(actual==adjusted,"black","red")),  #subject=factor(subject, levels=c("reading fluency", "reading other", "math")
  ggplot(aes(x=actual, y= adjusted, col=cntlab, label=cntlab)) +
  geom_point() + geom_text(hjust=-0.5, vjust=-0.2, size=8) + 
  scale_color_manual(values =c("black","blue","blue"), guide="none") +
  geom_abline(intercept = 0, slope = 1,linetype="dotted") + 
  geom_hline(yintercept = 0, linetype="dotted") +
  theme_classic(base_size = 24) +
  labs(x= "A (Girls- Boys)", y=lab1) 
  #theme_minimal()  #facet_wrap(.~subject, nrow = 1) +

ggsave(paste0(figures,"CAF_gminusb_science",".png"), plot=g2)

out |> filter(subject=="science", abs(err)<0.003,rtdist>0.01) |>nrow()

lab1= TeX("$\\widehat{A_s}$")
g3= out |> #filter(subject=="science", err<0.02,rtdist>0.01) |>
  filter(subject=="science") |>
  filter(actualdiff>=0) |> 
  mutate(cntlab = ifelse(cnt=="QMR" | cnt=="BRA", cnt, "")) |>   #mutate(cntlab=cnt) |>mutate(cntlab= ifelse(match, "match", "notMatch")) |>
  mutate(adjusted = as.numeric(a_c),
         actual=as.numeric(actualdiff),
         As= as.numeric(a_s)
  ) |> #col= if_else(actual<adjusted, "blue", if_else(actual==adjusted,"black","red")),  #subject=factor(subject, levels=c("reading fluency", "reading other", "math")
  ggplot(aes(x=actual, y= As, col=cntlab,label=cntlab)) +
  geom_point() + 
  scale_color_manual(values =c("black","blue", "blue"), guide="none") +
  geom_hline(yintercept = 0, linetype="dotted") +
  theme_classic(base_size = 24) + geom_text(hjust=1, vjust=1.3, size=8) + 
  labs(x= "A (Boys-Girls)", y=lab1
       ) #facet_wrap(.~subject, nrow = 1) +
ggsave(paste0(figures,"CAF_bminusg_science_As",".png"), plot=g3)

g4= out |>
  filter(subject=="science") |> #, abs(err)<0.003,rtdist>0.01
  filter(actualdiff<0) |>
  mutate(cntlab = ifelse(cnt=="NLD" | cnt=="ARE", cnt, "")) |>
  mutate(adjusted = -as.numeric(a_c),
         actual=-as.numeric(actualdiff),
         As= -as.numeric(a_s)
  ) |> #col= if_else(actual<adjusted, "blue", if_else(actual==adjusted,"black","red")),  #subject=factor(subject, levels=c("reading fluency", "reading other", "math")
  ggplot(aes(x=actual, y= As,col=cntlab,label=cntlab)) +
  geom_point() + #geom_text(hjust=-0.5, vjust=-0.2) + scale_color_manual(values =c("black","blue","blue"), guide="none") +
    scale_color_manual(values =c("black","blue", "blue"), guide="none") +
  geom_hline(yintercept = 0, linetype="dotted") + geom_text(hjust=0.5, vjust=-0.5, size=8) +
  theme_classic(base_size = 24) +
  labs(x= "A (Girls- Boys)", y=lab1) 
#theme_minimal()  #facet_wrap(.~subject, nrow = 1) +

ggsave(paste0(figures,"CAF_gminusb_science_As",".png"), plot=g4)

### Which countries have small time differences
# g3= out |>
#   filter(subject=="science", abs(err)<0.003) |>
#   filter(actualdiff<0) |>
#   mutate(timelab = ifelse(rtdist<0.01,"Similar rt", "Different rt")) |>
#   mutate(adjusted = -as.numeric(a_c),
#          actual=-as.numeric(actualdiff),
#          As= as.numeric(a_s)
#   ) |> #col= if_else(actual<adjusted, "blue", if_else(actual==adjusted,"black","red")),  #subject=factor(subject, levels=c("reading fluency", "reading other", "math")
#   ggplot(aes(x=actual, y= adjusted, col=timelab)) +
#   geom_point() +
#   scale_color_manual(values =c("blue","black"), guide="none") +
#   geom_abline(intercept = 0, slope = 1,linetype="dotted") + 
#   geom_hline(yintercept = 0, linetype="dotted") +
#   theme_classic(base_size = 10) +
#   labs(x= "Mean score difference (Girls- Boys)", y="Adjusted difference (Girls-Boys)") +
#   theme_minimal() 

#p1= plotpisa(out)
# p2= plotpisa(out, "reading other")
# p3= plotpisa(out, "math")
# ggarrange(p1,p2,p3,
#           common.legend = T,legend="none",
#           align = "h", nrow=3) 




# g4= country_desc %>% 
#   mutate(resp_diff=as.numeric(resp_diff),
#          subject=factor(subject, levels=c("reading fluency", "reading other", "math"))
#   ) %>%
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
#   ) %>%
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


# gender = 1 is female,2 is male
## run PISAhelper

## Each booklet
# booklet_nos= bind_rows(lapply(read_rf, function(x) x |> pull(BOOKID) |> table() ), .id="country")
# booklet_nos = booklet_nos |>
#   pivot_longer(cols= -c("country"),
#                names_to = "booklet",
#                values_to = "count")
# 
# unique(booklet_nos$booklet)
# 
# booklet_nos |>
#   group_by(booklet) |>
#   summarise(medcount= min(count)) |>
#   arrange(medcount)


# 
# plotCAF = function(out_caf,x, tmp) {
#   library(ggplot2)
#   library(cowplot)
#   x$group= x$gender
#   gcaf= out_caf  |> 
#     ggplot(aes(rt, prob, color=group)) + geom_line() +
#     scale_color_manual(values=c("red", "blue", "black"))  +xlab("log t (seconds)") + ylab("Accuracy offset") +
#     theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 2)) +
#     scale_x_continuous(breaks = c(seq(0, 7, 1)), limits = c(0,7)) 
#   image0= paste0(unique(out_caf$cnt), " ", unique(out_caf$subject))
#   caption0= paste0(unique(out_caf$cnt), " ", unique(out_caf$subject),"\n A:", 
#                    round(as.numeric(tmp[5]),4), "; A_c:", round(as.numeric(tmp[7]),4))
#   gtime= x |> ggplot(aes(rt, group=factor(group), color= factor(group))) + geom_density() + 
#     scale_color_manual(values=c("red", "blue")) +
#     theme(legend.position = "bottom")  + labs(caption = caption0) +
#     scale_x_continuous(breaks = c(seq(0, 7, 1)), limits = c(0,7)) 
#   
#   plot0= ggarrange(gcaf,gtime, ncol=1,common.legend = TRUE, legend = "bottom",
#                    align = "hv")
#   ggsave(paste0("/Users/radhika/Google Drive Stanford/PISA gender/Graphs/Country CAFs/",image0,".png"), plot=plot0)
#   #aligned= align_plots(gcaf, gtime, align = "v")
#   #plot_grid(aligned[[1]],aligned[[2]], ncol = 1, align = "h", ) 
#   
#   return(plot)
# }


#### Save CAFs images

# 
# out=data.frame()
# out_select=data.frame()
# for (i in 1:length(read_rf)){
#   tmp=runpisahelper(read_rf[[i]], "reading fluency")
#   out=rbind(out, unlist(tmp))
# }
# names(out) = names(unlist(tmp))

#for (i in 1:length(read_rf)){
#   bookletids = sort(unique(math[[i]]$BOOKID))
#   for (book in bookletids) {
#     if(n_distinct(df$id)>20) {
#     df=math[[i]] |> filter(BOOKID ==book)
#     tmp=runpisahelper(df, "math")
#     out=rbind(out, c(bookid=book,tmp))  
#     }
#   }
# 
#   bookletids = sort(unique(read_rf[[i]]$BOOKID))
#   for (book in bookletids) {
#   df=read_rf[[i]] |> filter(BOOKID ==book)
#   if(n_distinct(df$id)>20) {
#   tmp=runpisahelper(df, "reading fluency")
#   out=rbind(out, c(bookid=book,tmp))  
#   }
#     }
#   
#     
# bookletids = sort(unique(read_rc[[i]]$BOOKID))
# for (book in bookletids) {
#   if(n_distinct(df$id)>20) {
#         df=read_rc[[i]] |> filter(BOOKID ==book)
#         
#         tmp=runpisahelper(df, "reading other")
#         out=rbind(out, c(bookid=book,tmp))  
#   }
#   }
# }
  
  #tmp=runpisahelper(math[[i]], "math")
  #out=rbind(out, tmp)
  #chk= (as.numeric(tmp[5]) - as.numeric(tmp[6]))/as.numeric(tmp[5])
  # if(abs(chk)<=0.5) {
  # out_caf = cafcomplied(math[[i]])
  # out_caf$subject = rep("Math",3000)
  # out_caf$cnt = rep(unique(math[[i]]$CNT),3000)
  # plotCAF(out_caf, math[[i]], tmp)
  # out_select=rbind(out_select, tmp)
  # }
  
  
  # tmp=runpisahelper(read_rf[[i]], "reading fluency")
  # out=rbind(out, unlist(tmp))
  # chk= (as.numeric(tmp[5]) - as.numeric(tmp[6]))/as.numeric(tmp[5])
  # if(abs(chk)<=0.5) {
  # out_caf = cafcomplied(read_rf[[i]])
  # out_caf$subject = rep("Fluency",3000)
  # out_caf$cnt = rep(unique(read_rf[[i]]$CNT),3000)
  # plotCAF(out_caf, read_rf[[i]], tmp)
  # out_select=rbind(out_select, tmp)
  #}
  
  # tmp=runpisahelper(read_rc[[i]], "reading other")
  # out=rbind(out, unlist(tmp))
  # chk= (as.numeric(tmp[5]) - as.numeric(tmp[6]))/as.numeric(tmp[5])
  # if(abs(chk)<=0.5) {
  # out_caf = cafcomplied(read_rc[[i]])
  # out_caf$subject = rep("Reading",3000)
  # out_caf$cnt = rep(unique(read_rc[[i]]$CNT),3000)
  # plotCAF(out_caf, read_rc[[i]], tmp)
  # out_select=rbind(out_select, tmp)
  # }
  #out= rbind(out, c("cntid"=i, "cnt"= unique(x_filter$CNT), "N"=samplesize, "subject"=j, "obs"=actual, "adj"=intdiff))
#}
# names(out) = c("book",names(tmp))
# #write_csv(out, "/Users/radhika/Google Drive Stanford/PISA gender/Data subsets and Code/group_diff_adjusted v4.csv")
# out$chk= (as.numeric(out[,5]) - as.numeric(out[,6]))/as.numeric(out[,5])
# out |> filter(abs(chk)<0.2) |> nrow() #124/213

#out= read_csv("/Users/radhika/Google Drive Stanford/PISA gender/Data subsets and Code/group_diff_adjusted v3.csv")
### Save graphs
# 
# cafcomplied = function(x) {
#   x$group = x$gender
#   x = x |>
#     filter(!is.na(rt),
#            !is.na(group))
#   caf<-getcaf(x, deg=2) 
#   caf0=getcaf0(x, deg=2)
#   
#   out1= data.frame(cbind(caf[[1]], rep("Group 1",1000)))
#   out2= data.frame(cbind(caf[[2]], rep("Group 2",1000)))
#   out3= data.frame(cbind(caf0, rep("Merged",1000)))
#   out_caf = rbind(out1, out2, out3)
#   
#   names(out_caf) = c("rt", "prob", "group")
#   
#   #names(out_caf) = c("rt", "prob", "cntryID", "subject", "group")
#   out_caf$rt = as.numeric(out_caf$rt) # V1 is time, V2 is accuracy prediction
#   out_caf$prob= as.numeric(out_caf$prob)
#   return(out_caf)
#   
# }
# 
# out=data.frame()
# for (i in 1:length(math)){
#   out_caf = cafcomplied(math[[1]])
#   out_caf$subject = rep("Math",3000)
#   out_caf$cntry_id = rep(unique(math[[1]]$CNT), 3000)
#   
#   
#   tmp=runpisahelper(read_rf[[i]], "reading fluency")
#   out=rbind(out, unlist(tmp))
#   
#   tmp=runpisahelper(read_rc[[i]], "reading other")
#   out=rbind(out, unlist(tmp))
#   #out= rbind(out, c("cntid"=i, "cnt"= unique(x_filter$CNT), "N"=samplesize, "subject"=j, "obs"=actual, "adj"=intdiff))
# }
# names(out) = names(unlist(tmp))


###########


################################################
# Graphs on full data
################################################
# library(readr)
#



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
