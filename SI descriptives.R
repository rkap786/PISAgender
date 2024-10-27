## function for running pisahelper
library(readr)
library(PISAhelper)
library(tidyverse)
library(fs)
library(ggpubr)
library(dplyr)
library(ggplot2)
setwd("/Users/radhika/Library/CloudStorage/GoogleDrive-rkap786@stanford.edu/My Drive/0. Projects - Stanford/PISA gender/Final code files revision/")
source("Code/PISAgender/PISAhelper/R/scorediff.R")
source("Code/PISAgender/PISAhelper/R/getcaf_deg.R")
source("Code/PISAgender/PISAhelper/R/getcaf0_deg.R")
source("Code/PISAgender/PISA_dataprep_funcs.R")
source("Code/PISAgender/PISAhelper/R/integrate.R")
source("Code/PISAgender/analysis_fncs.R")
#source("pisa scorediff plot oct22.R")
#source("Updated code/caf estimation.R")
#source("Updated code/plot_caftime.R")

##########################################
#load("Data/Final data/read_fluency.Rdata")
load("Data/Final data/science.Rdata")
out=data.frame()
for (i in 1:length(science)) {
  x=science[[i]] |>
    filter(BOOKID>=13 & BOOKID<=18) 
  x= filterdata(x, qu=0.99, ql=0.01) #drop 1% and log rt
  x1= x|> group_by(CNT, gender) |>
    summarise(
      mean.rt= mean(rt, na.rm=T),
      mean.resp= mean(resp, na.rm = T)
              )
  x2= x|> select(id, CNT, gender) |> unique() |>
    group_by(CNT, gender) |>
    summarise(n=n())
  x=left_join(x1,x2)
  
  out=bind_rows(out, x)
}

out=out |> rename(cnt=CNT) 
out = out |> mutate(gender= if_else (gender==1, "female", "male")) |>
  pivot_wider(id_cols = cnt,
              names_from = gender,
              values_from=c(n, mean.rt, mean.resp)) 


### Print descriptive data
#stats= read_csv("Data/Final data/readrf_results_q99q1.csv")
stats= read_csv("Data/Final data/science_book13to18_results_q99q1.csv")
stats = stats |> dplyr::select(cnt, cntname, N, actualdiff, a_c, a_s) 
stats= left_join(stats, out)
stats = stats |>
  mutate(female.per= n_female*100/ (n_female+n_male),
         actualdiff=actualdiff*(-100),
         a_c=a_c*(-100)) |>
  select(cntname, cnt, N, female.per, actualdiff, a_c) 
stats$subject= rep("Science", nrow(stats))


write_csv(stats,"Data/Final data/science_descriptive.csv")



##########################################
#load("Data/Final data/read_fluency.Rdata")
load("Data/Final data/science.Rdata")

out=data.frame()
for (i in 1:length(science)) {
  x=science[[i]] |>
    filter(BOOKID>=13 & BOOKID<=18) 
  x= filterdata(x, qu=0.99, ql=0.01) #drop 1% and log rt
  x1= x|> group_by(CNT, gender) |>
    summarise(
      mean.rt= mean(rt, na.rm=T),
      mean.resp= mean(resp, na.rm = T)
    )
  x2= x|> select(id, CNT, gender) |> unique() |>
    group_by(CNT, gender) |>
    summarise(n=n())
  x=left_join(x1,x2)
  
  out=bind_rows(out, x)
}

out=out |> rename(cnt=CNT) 
out = out |> mutate(gender= if_else (gender==1, "female", "male")) |>
  pivot_wider(id_cols = cnt,
              names_from = gender,
              values_from=c(n, mean.rt, mean.resp)) 


### Print descriptive data
#stats= read_csv("Data/Final data/readrf_results_q99q1.csv")
stats= read_csv("Data/Final data/science_book13to18_results_q99q1.csv")
stats = stats |> dplyr::select(cnt, cntname, N, actualdiff, a_c, a_s) 
stats= left_join(stats, out)
stats = stats |>
  mutate(female.per= n_female*100/ (n_female+n_male),
         actualdiff=actualdiff*(-100),
         a_c=a_c*(-100)) |>
  select(cntname, cnt, N, female.per, actualdiff, a_c) 
stats$subject= rep("Science", nrow(stats))
names(stats) = c("Country name", "Code", "N", "% female", 
                 "Observed difference", "Adjusted difference", "Subject")
write_csv(stats,"Data/Final data/science_descriptive.csv")
finalstats = stats
#################################################################################
#################################################################################

load("Data/Final data/read_fluency.Rdata")


out=data.frame()
for (i in 1:length(read_rf)) {
  x= read_rf[[i]]
  x= filterdata(x, qu=0.99, ql=0.01) #drop 1% and log rt
  
  x1= x|> group_by(CNT, gender) |>
    summarise(
      mean.rt= mean(rt, na.rm=T),
      mean.resp= mean(resp, na.rm = T)
    )
  x2= x|> select(id, CNT, gender) |> unique() |>
    group_by(CNT, gender) |>
    summarise(n=n())
  x=left_join(x1,x2)
  
  out=bind_rows(out, x)
}

out=out |> rename(cnt=CNT) 
out = out |> mutate(gender= if_else (gender==1, "female", "male")) |>
  pivot_wider(id_cols = cnt,
              names_from = gender,
              values_from=c(n, mean.rt, mean.resp)) 


### Print descriptive data
stats= read_csv("Data/Final data/readrf_results_q99q1.csv")
stats = stats |> dplyr::select(cnt, cntname, N, actualdiff, a_c, a_s) 
stats= left_join(stats, out)
stats = stats |>
  mutate(female.per= n_female*100/ (n_female+n_male),
         actualdiff=actualdiff*(-100),
         a_c=a_c*(-100)) |>
  select(cntname, cnt, N, female.per, actualdiff, a_c) 
stats$subject= rep("Reading fluency", nrow(stats))
names(stats) = c("Country name", "Code", "N", "% female", 
                 "Observed difference", "Adjusted difference", "Subject")
write_csv(stats,"Data/Final data/readrf_descriptive.csv")

finalstats=bind_rows(finalstats,stats)

names(finalstats) = c("Country name", "Code", "N", "% female", 
                      "Observed difference", "Adjusted difference", "Subject")

write_csv(finalstats,"Data/Final data/SI_descriptive.csv")
