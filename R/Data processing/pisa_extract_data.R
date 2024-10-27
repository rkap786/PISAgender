## function for running pisahelper
library(readr)
library(PISAhelper)
library(tidyverse)
library(fs)
library(ggpubr)
library(ggplot2)




setwd("/Users/radhika/Library/CloudStorage/GoogleDrive-rkap786@stanford.edu/My Drive/0. Projects - Stanford/PISA gender/Final code files revision/")
source("Code/PISA_dataprep_funcs.R")

f <- dir_ls('Data/Raw data/country datasets/')
n=length(f)


#gender datafile
gen<-read_csv("Data/Raw data/pisa_stu_gender.csv")

########### 
## Pull data we want to use from country level PISA files + add gender

read_rf= list()
# read_rc= list()
# math= list() 
science=list()
counter=1
n=length(f)

for (i in 1:n){ 
  print(i)
  pisa_sample = read_csv(f[i])
  #pisa_math = extract_math_country(pisa_sample)
  #pisa_math= addgender(pisa_math, gen)
  rf= extract_read_country(pisa_sample) 
  rf= addgender(rf, gen)
  #rc= addgender(pisa_read[[2]], gen)
  pisa_science= extract_science_country(pisa_sample)
  pisa_science= addgender(pisa_science, gen)
  # identical(rc$itemno, character(0)) | is.null(rc$itemno) |
  #   identical(pisa_math$itemno, character(0)) | is.null(pisa_math$itemno) |
    
  if(  !(
         identical(rf$itemno, character(0)) | is.null(rf$itemno) |
         identical(pisa_science$itemno, character(0)) | is.null(pisa_science$itemno)
         )
       ) {
  # pisa_math= clean(pisa_math)
  rf=clean(rf)
  read_rf[[counter]]= rf
  # rc=clean(rc)
  # read_rc[[counter]]=rc
  # math[[counter]] =pisa_math
  pisa_science= clean(pisa_science)
  science[[counter]]= pisa_science
  counter=counter+1
  }
}



save(read_rf, file="Data/Final data/read_fluency.Rdata")
#save(read_rc, file="Data/Final data/read_comprehension.Rdata")
#save(math, file="Data/Final data/math.Rdata")
save(science, file="Data/Final data/science.Rdata")
##########################################
 read_rf= load("read_fluency.Rdata")
# read_rc= load("read_comprehension.Rdata")
# math= load("math.Rdata")

# gender = 1 is female,2 is male
## run PISAhelper


### get country level descriptives
run_country_desc= function(x, subject){
  cnt= unique(x$CNT)
  x= x |>
  group_by(gender) |>
    mutate(n= n_distinct(id)) |> 
  summarise(resp_mean= mean(resp),
            rt_median= median(rt),
            rt_lq= quantile(rt, probs = 0.25),
            rt_uq = quantile(rt, probs = 0.75),
            n=mean(n)
  ) |>
  mutate(gender= if_else(gender==1, "Female", "Male")) |>
  pivot_wider(
              names_from = gender,
              values_from = c(resp_mean, 
                              rt_median, rt_lq, rt_uq, n)) |>
  mutate(resp_diff= resp_mean_Female - resp_mean_Male,
         rt_diff= rt_median_Female - rt_median_Male) 

  x=list(cbind("country"=cnt, "subject"= subject,x))
  return(x)
  }

# for (i in 1:length(science)){
#     science[[i]]$rt=science[[i]]$rt/1000
#   }
# 

tmp=list()
country_desc=data.frame()
for (i in 1:length(math)){
  tmp=run_country_desc(math[[i]], "math")
  country_desc=rbind(country_desc, unlist(tmp))
  tmp=run_country_desc(read_rf[[i]], "reading fluency")
  country_desc=rbind(country_desc, unlist(tmp))
  tmp=run_country_desc(read_rc[[i]], "reading other")
  country_desc=rbind(country_desc, unlist(tmp))
}

for (i in 1:length(science)){
  if(!is.null(science[[i]])) {
  tmp=run_country_desc(science[[i]], "science")
  country_desc=rbind(country_desc, unlist(tmp))
  }
}

names(country_desc) = names(unlist(tmp)) 

# x1= science[[13]] |>
#   mutate(rt= log(rt)) |>
#   filter(rt>= quantile(x$rt, 0.01) & rt<= quantile(x$rt, 0.99))
# 
# x2= science[[13]] |>
#   filter(rt>= quantile(x$rt, 0.01) & rt<= quantile(x$rt, 0.99)) |>
#   mutate(rt= log(rt))
# 
# 
# x3= science[[13]] |>
#   filter(rt>= quantile(x$rt, 0.01) & rt<= quantile(x$rt, 0.99)) |>
#   mutate(rt= log(rt))  |>
#   group_by(item) |>
#   mutate(rt= scale(rt)) 


library(learningtower)
data("countrycode")
country_desc= left_join(country_desc, countrycode)
write_csv(country_desc, "country_descriptives.csv")

#pisa_read= read_csv("Final datasets/pisa_read_gender.csv")
country_desc_science= country_desc |>
  filter(subject=="science")
write_csv(country_desc_science, "country_descriptives_science.csv")

#country_desc=read.csv("country_descriptives.csv")
country_desc |>
  filter(subject=="reading other") |>
  summary(resp_mean_Female)

#summary(country_desc$resp_mean_Male)





# 
# #### estimate CAFs and adjusted difference
# runpisahelper= function(x, subject) {
#   M<-by(x$resp,x$gender,mean)
#   actual=M[[2]]-M[[1]]
#   x$group= case_when(actual>0 & x$gender==1 ~ 1,
#                      actual>0 & x$gender==2 ~ 2,
#                      actual<0 & x$gender==1 ~ 2,
#                      actual<0 & x$gender==2 ~ 1,)
#   M<-by(x$resp,x$group,mean)
#   actual=M[[2]]-M[[1]]
#   caf<-getcaf(x) 
#   #print(M)
#   ##integrate
#   #caf.tmp<-data.frame(t=caf[[2]][,1],yhat=caf[[2]][,2]-caf[[1]][,2])
#   caf.tmp1<-data.frame(t=caf[[2]][,1],yhat=caf[[1]][,2])
#   caf.tmp2<-data.frame(t=caf[[2]][,1],yhat=caf[[2]][,2])
#   intdiff<-integrate(x$rt,caf.tmp2)
#   samplesize=n_distinct(x$id) 
#   out= list(c("cntid"=unique(x$CNTRYID), "cnt"= unique(x$CNT), "N"=samplesize, "subject"=subject, "obs"=actual, "adj"=intdiff))
# }
# 
# 
# 
# 
# out=data.frame()
# for (i in 1:length(math)){
#   tmp=runpisahelper(math[[i]], "math")
#   out=rbind(out, unlist(tmp))
#   
#   tmp=runpisahelper(read_rf[[i]], "reading fluency")
#   out=rbind(out, unlist(tmp))
#   
#   tmp=runpisahelper(read_rc[[i]], "reading other")
#   out=rbind(out, unlist(tmp))
#   #out= rbind(out, c("cntid"=i, "cnt"= unique(x_filter$CNT), "N"=samplesize, "subject"=j, "obs"=actual, "adj"=intdiff))
# }
# names(out) = names(unlist(tmp))
# 
# 
# # Country level Descriptives on x
# x=pisa_gender
# country_count = x |>
#   filter(!is.na(rt),
#          !is.na(resp),
#          !is.na(gender)) |>
#   group_by(CNT) |>
#   mutate(n= n_distinct(id)) |>
#   select(CNT, n) |>
#   unique()
# 
# ###### Plot  caf
# plots=list()
# for (i in 1:length(read_rf)) {
#   x=read_rf[[1]]
#   x$group= x$gender
#   caf= getcaf(x)
#   plots=plotcaf(caf, unique(x$CNT))
# }
# 
# g1= out |>
#   mutate(adjusted = as.numeric(adj),
#          actual=as.numeric(obs),
#          col= if_else(actual<adjusted, "blue", if_else(actual==adjusted,"black","red")),
#          subject=factor(subject, levels=c("reading fluency", "reading other", "math"))) |>
#   ggplot(aes(x=actual, y= adjusted, color=col, label=cnt)) +
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
# 
# ################################################
# # Graphs on sample data
# ################################################
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
# g1_1= pisa_gender %>%
#   filter(!is.na(rt), !is.na(gender)) %>%
#   mutate(gender_str= if_else(gender==1, "Female", "Male"),
#          subject=factor(subject, levels=c("reading fluency", "reading other", "math"))) %>%
#   ggplot(
#     aes(x=rt, group=as.factor(gender_str), color=as.factor(gender_str))
#   ) +
#   geom_density() +
#   facet_wrap(.~subject)  + theme_classic() +
#   theme(legend.position="bottom", axis.title = element_text(size=8)) +
#   labs(color="Gender") +
#   xlab("Log response time") + ylab("Density")  
# 
# g1_2= pisa_gender %>%
#   filter(!is.na(rt)) %>%
#   filter(!is.na(gender))  %>%
#   mutate(gender=if_else(gender==1, "Female", "Male")) %>%
#   group_by(subject, item, gender) %>%
#   summarise(mean_score= mean(resp)) %>%
#   pivot_wider(names_from = gender,
#               values_from= c(mean_score)) %>%
# mutate(
#   diff_score= Female-Male,
#   subject=factor(subject, levels=c("reading fluency", "reading other", "math"))
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
# 
