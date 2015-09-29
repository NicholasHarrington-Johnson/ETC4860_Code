## Call all file
rm(list=ls())

## Loading packages

source("packages.R")

## Loading functions

source("function.R")

## Public Holidays
phols <- fread("Public_Holidays_Scaled.csv")
phols <- readph(phols)

## Bookings
numr <- c(1:30)[-17]

load("truncated_data.Rda")

all_mses <- list()

cl <- makeCluster(4)
registerDoParallel(cl)

all_mses <- list()
pckz <- c("data.table","dplyr","fpp","timeDate","splines","doParallel")

all_mses <- foreach(rstnum = numr,.combine=list,.multicombine = TRUE,.packages = pckz) %dopar% 
     rest_mod(tr[[rstnum]],starttraining=(nrow(tr[[rstnum]])-3),h=2)

save(all_mses,file="just_r_small_29.Rda")

stopCluster(cl)
