# Main script to run analysis
# Total run time: 48 hours 25 minutes

rm(list=ls())

library(tidyverse)

# setwd("xxx")

source("scripts/covid_triage_simr.R")

cases_raw=read.csv("inputs/inputs-demand.csv")
groups_raw=read.csv("inputs/inputs-groups.csv")
los<-read.csv("inputs/inputs-los.csv")

#cap=40                    # capacity, i.e. number of beds
nreps=1000                                  # number of simulation replications

crit_ls<-list()
crit_ls[[1]]<-c(T,T,T,T,T,T)
crit_ls[[2]]<-c(T,T,T,T,T,F)
crit_ls[[3]]<-c(T,T,T,T,F,F)
crit_ls[[4]]<-c(T,T,T,F,F,F)
crit_ls[[5]]<-c(T,T,F,F,F,F)

print("Entering first loop...")

for (cap in seq(10,200,10)) {
  # Start timer
  start.time <- Sys.time()

  folder_name<-paste0("outputs/zresults_",cap,"_",format(Sys.time(), "%Y_%m_%d_%H_%M_%S"))
  dir.create(folder_name)
  outp_agg<-data.frame(scenario=character(),policy=numeric(),policy_param=numeric(),crit=character(),metric=character(),currency=character(),value=numeric())
  outp_raw<-data.frame(scenario=character(),policy=numeric(),policy_param=numeric(),crit=character(),dates=numeric(),group=character(),metric=character(),currency=character(),value=numeric())
  for (scenario in unique(cases_raw$scenario)) {
    for (policy in 1:3) {
      if (policy==2) {
        policy_param_ls<-seq(3,6,3)
      } else {
        policy_param_ls<-NA
      }
      for (policy_param in policy_param_ls) {
        if (policy==1) {
          crit_ls1<-crit_ls
        } else {
          crit_ls1<-crit_ls[-1]
        }
        for (crit in crit_ls1) {
          name<-paste0(scenario,"_",policy,"_",policy_param,"_",paste(crit,collapse="_"))
          res<-covid_simr2(name=name,cases=cases_raw[which(cases_raw$scenario==scenario),],groups=groups_raw,los=los,cap=cap,policy=policy,policy_param=policy_param,crit=crit,nreps=nreps)
          outp_agg<-rbind(outp_agg,as.data.frame(res[[1]]))
          outp_raw<-rbind(outp_raw,as.data.frame(res[[2]]))
          print(paste("completed:",name),quote=FALSE)
        }
      }
    }
  }
  write.csv(outp_agg,paste0(folder_name,"/outp_agg.csv"),row.names=FALSE)
  write.csv(outp_raw,paste0(folder_name,"/outp_raw.csv"),row.names=FALSE)

  # Finish timer and save to file
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  writeLines(paste(time.taken, attr(time.taken, "units")),
             paste0(folder_name,"/time.txt"))
}

