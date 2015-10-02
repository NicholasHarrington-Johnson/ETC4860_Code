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

## Evaluate the Best Benchmark model

#out <- bmod(tr)
#print(out)

# Comes out as ARIMA with PH Dummys

## Evaluate all models using rolling forecast origin
# Run for all restaurants and output to an R file

#all_mses <- list()

#cl <- makeCluster(4)
#registerDoParallel(cl)

#all_mses <- list()
#pckz <- c("data.table","dplyr","fpp","timeDate","splines","doParallel")

#all_mses <- foreach(rstnum = numr,.combine=list,.multicombine = TRUE,.packages = pckz) %dopar% 
 #    rest_mod(tr[[rstnum]],starttraining=50,h=14)

#save(all_mses,file="resid_file.Rda")

#stopCluster(cl)

## baseline forecast

primif <- list()
numr <- 4
for (rstnum in numr){
  primif[[rstnum]] <- abs(tr[[rstnum]]$b_t0[15:nrow(tr[[rstnum]])] - tr[[rstnum]]$b_t0[1:(nrow(tr[[rstnum]])-14)])
}
# Just for now primif is only for one restaurant
# When additional restaurant rmses finish running primif should be changed back to a list
load("just_r_4.Rda")
primif <- primif[[4]]

source("other_visual_stuff.R")