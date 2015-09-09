## Call all file
rm(list=ls())

# Run this on the cluster for everything to work nice

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

# Run for all restaurants and output to an R file

cl <- makeCluster(8)
registerDoParallel(cl)
# some combine stuff
# for foreach
comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}
# end that

all_mses <- list()
pckz <- c("data.table","dplyr","fpp","timeDate","splines","doParallel")

all_mses <- foreach(rstnum = numr,.combine='comb',.multicombine = TRUE,.packages = pckz) %dopar%
     rest_mod(tr[[rstnum]],starttraining=50,h=14)

save(all_mses,file="all_mses.Rda")

stopCluster(cl)
