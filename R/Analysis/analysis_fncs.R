
filterdata= function(x, qu, ql) {
  x$group= x$gender
  upper=quantile(x$rt, qu)
  lower=quantile(x$rt, ql)
  
  x1= x |>
    filter(rt>=(lower) & rt<=(upper))  |>
    mutate(rt= log(rt)) 
  
  x1 = x1 |>
    group_by(item) |>
    mutate(meanrt= mean(rt),
           rt = rt-meanrt) |>
    dplyr::select(-meanrt) |>
    ungroup()
  
  return(x1)
}

#### estimate CAFs and adjusted difference
runpisahelper= function(x, subject) {
  #M<-by(x$resp,x$gender,mean)
  #actual=M[[2]]-M[[1]]
  # x$group= case_when(actual>0 & x$gender==1 ~ 1,
  #                    actual>0 & x$gender==2 ~ 2,
  #                    actual<0 & x$gender==1 ~ 2,
  #                    actual<0 & x$gender==2 ~ 1)
  # 
  x$group = x$gender
  result=scorediff(x, deg=2)
  samplesize=n_distinct(x$id)
  out=c(result, "N"=n_distinct(x$id),"cntid"=unique(x$CNTRYID), "cnt"= unique(x$CNT),"subject"=subject)
  #out= c("cntid"=unique(x$CNTRYID), "cnt"= unique(x$CNT), "N"=samplesize, "subject"=subject, "obs"=as.numeric(result[1]),"estdiff"=result[2], "adjc"=result[3],"adjs"=result[4])
  out
}


plotCAF = function(x, tmp, out_caf) {
  library(ggplot2)
  library(cowplot)
  #x$group= x$gender
  gcaf= out_caf  |> 
    ggplot(aes(rt, prob, color=group)) + geom_line() +
    scale_color_manual(name="",values=c("red", "blue", "black"), label=c("females", "males"))  +xlab("log t (seconds)") + ylab("Accuracy offset") +
    guides(colour = guide_legend(nrow = 2)) +
    scale_x_continuous(breaks = c(seq(-7, 7, 1)), limits = c(-7,7)) +
    theme_minimal(base_size = 14) 
     #+theme(legend.position = "right",legend.title=element_blank(), legend.direction="horizontal")
    
  image0= paste0(unique(tmp[7]), " ", unique(tmp[8]))
  caption0= paste0(unique(tmp$cntname), " : ", unique(out_caf$subject),"\n A:", 
                   round(-as.numeric(tmp$actualdiff),4), "; \nA_c:", round(-tmp$a_c,4))
  gtime= x |> ggplot(aes(rt, group=factor(group), color= factor(group))) + geom_density() + 
    scale_color_manual(name="",values=c("red", "blue"), label=c("females", "males")) +
    xlab("log t (seconds)") +
    labs(caption = caption0) +
    scale_x_continuous(breaks = c(seq(-7, 7, 1)), limits = c(-7,7)) +
    theme_minimal(base_size = 14)  #+theme(legend.position = "right", legend.title=element_blank())
  
  plot0= ggarrange(gcaf,gtime, ncol=1,common.legend = TRUE, legend = "right",
                   align = "hv")
  #ggsave(paste0("/Users/radhika/Google Drive Stanford/PISA gender/Graphs/Country CAFs/",image0,".png"), plot=plot0)
  #aligned= align_plots(gcaf, gtime, align = "v")
  #plot_grid(aligned[[1]],aligned[[2]], ncol = 1, align = "h", ) 
  
  return(plot0)
}

cafcomplied = function(x) {
  x$group = x$gender
  x = x |>
    filter(!is.na(rt),
           !is.na(group))
  caf<-getcaf_deg(x, deg=2) 
  caf0=getcaf0_deg(x, deg=2)
  
  out1= data.frame(cbind(caf[[1]], rep("Group 1",1000)))
  out2= data.frame(cbind(caf[[2]], rep("Group 2",1000)))
  out3= data.frame(cbind(caf0, rep("Merged",1000)))
  out_caf = rbind(out1, out2, out3)
  
  names(out_caf) = c("rt", "prob", "group")
  
  #names(out_caf) = c("rt", "prob", "cntryID", "subject", "group")
  out_caf$rt = as.numeric(out_caf$rt) # V1 is time, V2 is accuracy prediction
  out_caf$prob= as.numeric(out_caf$prob)
  out_caf$cnt= unique(x$CNT)
  return(out_caf)
  
}



### Find distance between group response time distributions
### Function from:https://stackoverflow.com/questions/55604403/computing-the-kl-divergence-with-kernel-density-estimates
kld_base = function(x,y,...){
  integrand = function(x,y,t){
    f.x =  approx(density(x)$x,density(x)$y,t)$y
    f.y =  approx(density(y)$x,density(y)$y,t)$y
    tmpRatio = f.x *(log2(f.x) - log2(f.y))
    tmpRatio = ifelse(is.infinite(tmpRatio),0,ifelse(is.na(tmpRatio),0,tmpRatio))
    return(tmpRatio)
  }
  return(stats::integrate(integrand,-Inf,Inf,x = x,y = y,stop.on.error=FALSE)$value)
}
