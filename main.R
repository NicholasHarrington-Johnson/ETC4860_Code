## Call all file
rm(list=ls())

## Loading packages

source("packages.R")

## Reading data

eztable <- fread("booking_top30_restaurants_all.txt")

## Loading functions

source("function.R")

## Public Holidays
phols <- fread("Public_Holidays_Scaled.csv")
phols <- readph(phols)

## Bookings
numr <- c(1:30)[-17]
#r <- clean(eztable)

## Truncating data

#tr <- truncatep(r)
#save(tr,file="truncated_data.Rda")

load("truncated_data.Rda")
## Plotting

for(i in numr)
{
  plotpub(tr[[i]],i)
}

## Best overall model

#out <- bmod(tr)
#print(out)

#rstnum <-2
#plotpub(tr[[rstnum]],rstnum)
#out <- mseevaluate(tr[[rstnum]],h=14)

#print(out[[1]])
#plotmse(out[[2]])
#ploth(out[[3]]) 

# Run for all restaurants and output to an R file
numr <- 4
all_mses <- list()

#for (rstnum in numr){
#  all_mses[[rstnum]] <- rest_mod(tr[[rstnum]],starttraining=(nrow(tr[[rstnum]])-50),h=2)
#}

cl <- makeCluster(2)
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
Sys.time()
all_mses <- foreach(rstnum = numr,.combine='comb',.multicombine = TRUE,.packages = pckz) %dopar% 
     rest_mod(tr[[rstnum]],starttraining=50,h=14)
Sys.time()
save(all_mses,file="just_r_4.Rda")

stopCluster(cl)

## baseline forecast

primif <- list()

for (rstnum in numr){
  primif[[rstnum]] <- abs(tr[[rstnum]]$b_t0[15:nrow(tr[[rstnum]])] - tr[[rstnum]]$b_t0[1:(nrow(tr[[rstnum]])-14)])
}

# Plot scaled RMSE

savepdf("R_4_ScaledRMSE")
scaleplotbyday(all_mses,1)
dev.off()

# Plot h step RMSE
savepdf("R_4_hRMSE")
ploth(visuali(all_mses,1))
dev.off