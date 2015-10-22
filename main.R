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
numr <- c(5,23,24,29)
cl <- makeCluster(8)
registerDoParallel(cl)

all_mses <- list()
pckz <- c("data.table","dplyr","fpp","timeDate","splines","doParallel","ggplot2","reshape2","ggfortify")

all_mses <- foreach(rstnum = numr,.combine=list,.multicombine = TRUE,.packages = pckz) %dopar% 
     rest_mod(tr[[rstnum]],starttraining=100,h=14,rstnum=rstnum)

#save(all_mses,file="resid_file.Rda")

stopCluster(cl)

# Re-do primif for completed restaurants
# New Results came through
completed <- c(1,2,3,4,6,7,9,10,11,12,13,14,15,16,18,19,20,21,22,25,26,28,30)

primif <- list()
numr <- completed
for (rstnum in numr){
  primif[[rstnum]] <- abs(tr[[rstnum]]$b_t0[15:nrow(tr[[rstnum]])] - tr[[rstnum]]$b_t0[1:(nrow(tr[[rstnum]])-14)])
}

# Load completed restaurants
big_un <- list()
for (i in completed){
  load(paste("Rs_R_",i,".Rda",sep=""))
  big_un[[i]] <- obj
}

# Load just ARIMA with previous bookings (no spline)
pb.ns <- list()
for (i in completed){
  load(paste("Rs_R_",i,"singlebook.Rda",sep=""))
  pb.ns[[i]] <- obj
}
new.ns <- make.resids(pb.ns,completed,starttraining = 100)

# Incorporate previous results with new ARIMA with previous bookings (no spline)
all.mod <- list()
for (i in completed){
  numit <- dim(big_un[[i]])[3]
  errors_array <- array(0,dim=c(7,14,numit),dimnames=list(c(row.names(big_un[[1]][,,1]),"Previous_b_ARIMA"),colnames(big_un[[1]][,,1]),NULL))
  for (j in 1:numit){
    errors_array[,,j]<- rbind(big_un[[i]][,,j],new.ns[[i]][,,j])
  }
  all.mod[[i]]<-errors_array
}

# Plot adjusted RMSE of completed restaurants
for (j in completed){
  #bpscaled(all_rmses = big_un,j,h = 14)
  scaleplotbyday(all_mses = all.mod,resty = j)
}

# Box Plot of total Adjusted RMSEs
bpallscaled(all.mod,completed)

# By Day Plot of total Adjusted RMSEs
byday_alladj(all.mod,completed)

# P/B of completed restaurants
pb_all(all.mod,completed)
plotpb(pb_all(all.mod,completed))

# P/B of all completed restaurants for Pickup, ARIMA, Spline_1_knot
pb_all_tru(all.mod,completed)
plotpb_tru(pb_all_tru(all.mod,completed))

# Investigating all adjusted RMSE
for (iter in completed){
  plotscaled(rmse.scaled = scalemse(listy = all.mod,iter),iter)
}

errors <- naive.seas(completed)

for (i in completed){
  ploth(all.mod,i,errors)
}

#source("gg_other_visual_stuff.R")