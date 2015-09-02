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
numr <- c(1:3)[-2]
all_mses <- list()
for (rstnum in numr){
  all_mses[[rstnum]] <- rest_mod(tr[[rstnum]],starttraining=(nrow(tr[[rstnum]])-3),h=2)
}


save(all_mses,file="all_mses.Rda")
