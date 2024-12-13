# This file runs one of the model scenarios and checks if you are getting
# consistent results with us. It has been modified to have a much shorter run
# time than in the reproduction.

# Run time: 40 seconds

library(tidyverse)
library(parallel)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Import some of the input parameters
cases_raw <- read.csv("../../inputs/inputs-demand.csv")
groups_raw <- read.csv("../../inputs/inputs-groups.csv")
los <- read.csv("../../inputs/inputs-los.csv")

# Import the model code
source("../../scripts/covid_triage_simr.R")

################################

test_that("Results from single mini run are consistent", {
  # Start timer
  start.time <- Sys.time()
  
  # Define some of the other input parameters
  cap <- 20
  scenario <- "cyclical"
  policy <- 1
  policy_param <- NA
  crit <- c(T,T,T,T,T,T)
  nreps <- 2

  # Create blank data frame to store results
  outp_agg<-data.frame(
    scenario=character(),policy=numeric(),policy_param=numeric(),
    crit=character(),metric=character(),currency=character(),value=numeric())
  
  # Get name for model run
  name<-paste0(scenario,"_",policy,"_",policy_param,"_",paste(crit,collapse="_"))
  
  # Run model
  res<-covid_simr2(name=name,
                   cases=cases_raw[which(cases_raw$scenario==scenario),],
                   groups=groups_raw,
                   los=los,
                   cap=cap,
                   policy=policy,
                   policy_param=policy_param,
                   crit=crit,
                   nreps=nreps,
                   testrun=TRUE,
                   scen=scenario)
  
  # Save result to dataframe
  outp_agg<-rbind(outp_agg,as.data.frame(res[[1]]))

  # Import the expected results
  exp_agg <- read.csv("expected_results/outp_agg.csv")
  
  # Compare the dataframes
  expect_equal(outp_agg, exp_agg)

  # Finish timer and print elapsed time
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
})