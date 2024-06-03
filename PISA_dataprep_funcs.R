library(readr)
library(tidyverse)
library(dplyr)

#####Extract math and reading

extract_math_country = function(data) {
  
dat_math_2018= data %>%
  dplyr::select(ends_with("ID"),
         "CNT","Region","STRATUM","SUBNATIO","OECD","ADMINMODE","LANGTEST_QQQ",
         "LANGTEST_COG","LANGTEST_PAQ", "BOOKID","RDESIGN","RCORE_TEST","RS1_TEST",
         "RS1_LEV","RS2_TEST","RS2_LEV","RCORE_PERF","RCO1S_PERF",
         (starts_with("CM") & ( ends_with("S") | ends_with("T") | ends_with("F"))),
         (starts_with("DM") & ( ends_with("S") | ends_with("T") | ends_with("F")))) 

#colnames(dat_math_2018)
#dim(dat_math_2018)
math_2018_wide = dat_math_2018 %>%
  gather(key="item", value = "resp",
         CM033Q01S:CM967Q03F) %>%
  filter(!is.na(resp)) %>%
  separate(item, c("itemno","identifier"), sep = -1) %>%
  spread(identifier, resp)


#save(math_2018_wide, file = "pisa_math_sample.Rdata")  
return(math_2018_wide)
}


extract_science_country = function(data) {
  
  dat_science= data %>%
    dplyr::select(ends_with("ID"),
           "CNT","Region","BOOKID",
           (starts_with("CS") & ( ends_with("S") | ends_with("T") | ends_with("F"))),
           (starts_with("DS") & ( ends_with("S") | ends_with("T") | ends_with("F")))) 
  
  #colnames(dat_science)
  #dim(dat_science)
  dat_science_wide = dat_science %>%
    gather(key="item", value = "resp",
           CS408Q01S:DS413Q06RF) %>%
    filter(!is.na(resp)) %>%
    separate(item, c("itemno","identifier"), sep = -1) %>%
    spread(identifier, resp)
  
  
  #save(math_2018_wide, file = "pisa_math_sample.Rdata")  
  return(dat_science_wide)
}

extract_read_country = function(data) {
  
  dat_read_2018= data |>
    dplyr::select(ends_with("ID"),
           "CNT","Region","STRATUM","SUBNATIO","OECD","ADMINMODE","LANGTEST_QQQ",
           "LANGTEST_COG","LANGTEST_PAQ", "BOOKID","RDESIGN","RCORE_TEST","RS1_TEST",
           "RS1_LEV","RS2_TEST","RS2_LEV","RCORE_PERF","RCO1S_PERF",
           (starts_with("CR590") & ( ends_with("S") | ends_with("T") ))
           )
  
  ####Wide dataset                
  #dat_read_2018 =sample_n(dat_read_2018, 10000)
  
 # colnames(dat_read_2018)

  read_rf = dat_read_2018 |>
    gather(key="item", value = "resp",
           CR590Q04S:CR590Q66T) |>
    filter(!is.na(resp)) |>
    separate(item, c("itemno","identifier"), sep = -1) |>
    spread(identifier, resp)
  #save(math_2018_wide, file = "pisa_math_sample.Rdata")  
  
  # read_rc= read_2018_wide |>
  #   filter(!str_detect(itemno, "CR590"))

  return(read_rf)
}


## RF questions start with CR590, 65 items
# read_2018_wide |>
#   group_by(itemno) |>
#   summarise(meantime= mean(T, na.rm=T)) |>
#   ggplot(aes(x=meantime)) +
#   geom_histogram() +
#   geom_vline(xintercept= 11000)

# itemlist= read_2018_wide |>
#   group_by(itemno) |>
#   summarise(meantime= mean(T, na.rm=T)) |>
#   filter(meantime<11000) |>
#   select(itemno) |>
#   unique()


addgender = function(dat, gen) {
  ## merge gender
  #load("raw_pisa2018math_rk.Rdata")
  #dim(math_2018_wide)
  dat = left_join(dat,gen, by=c("CNTRYID", "CNTSCHID", "CNTSTUID"))
  #colnames(pisa_math_gender)
  #dim(pisa_math_gender)
  return(dat)
  
}

clean=function(data) {
  
  data = data %>%
    filter(S<2)  %>%
    rename(resp=S,
           id = CNTSTUID,
           item= itemno,
           gender=ST004D01T,
           rt= T) |> 
    mutate(rt= rt/1000) |>
    filter(!is.na(rt),
           !is.na(gender))
  
  return(data)
}
# filter(
#   rt>=quantile(x$rt, 0.01) & rt<=(quantile(x$rt, 0.99)),
#   rt = log(T/1000)) |>