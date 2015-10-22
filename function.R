## This file contains all necessary functions

##########################################################
##########################################################

## Function to read in data

readfunction <- function(rstnum,startdate=0,enddate=9999,h=7)
{ 
  # Remove weird numbers. 
  # Negative people, or more than 10000 people, in a single booking.
  # Probably errors
  eztable <- filter(eztable, people > 0, people < 1e4)
  
  # Remove cancellations
  eztable <- filter(eztable, status!="canceled" & status!='no-show')
  
  # List of restaurants in order of popularity
  restaurants <- rev(sort(table(eztable[,restaurant_id])))
  
  # Just look at one restaurant. Could be any of them.
  # Pick the second most popular as an example
  r2 <- subset(eztable, restaurant_id==names(restaurants)[rstnum])
  
  # Convert reservation and booking dates to numerical days
  res <- as.timeDate(r2$reservation_datetime)
  firstres <- min(res)
  res <- as.numeric(res)
  book <- as.numeric(as.timeDate(r2$booking_date))
  
  # Remove rows where booking is after reservation
  j <- (book <= res)
  r2 <- filter(r2, j)
  book <- book[j]
  res <- res[j]
  
  # Create daily totals. i.e., number of people booked for each day
  # Probably much easier ways to do this, but I can't think of them.
  # b = number of discrete bookings
  # p = number of people booked
  res <- as.numeric(res)
  book <- as.numeric(book)
  book <- book - min(res) + 1
  res <- res - min(res) + 1
  b <-  matrix(0L, nrow=max(res), ncol=max(res-book+1))
  rownames(b) <- paste("Day",1:nrow(b))
  colnames(b) <- paste(0:(ncol(b)-1))
  p <- b
  for(i in 1:nrow(b))
  {
    tmp <- filter(r2, res==i) # Bookings for day firstres+i-1.
    if(nrow(tmp)>0)
    {
      bk <- i - book[res==i] # days from reservation to booking
      daystores <- tabulate(bk+1) # Add one so zeros are counted
      b[i,1:length(daystores)] <- daystores
      daystores <- tabulate(rep(bk,tmp$people)+1) # Add one so zeros are counted
      p[i,1:length(daystores)] <- daystores
    }
  }
  
  # Remove head rows with zero bookings (prior to restaurant using system)
  firstnonzero <- min(which(rowSums(b)>0))
  # Remove tail rows with zero bookings
  lastnonzero <- max(which(rowSums(b)>0))
  # Remove some additional rows before all bookings available
  b <- b[firstnonzero:lastnonzero,]
  p <- p[firstnonzero:lastnonzero,]
  
  # Compute cumulative reservations
  # i.e., reservations
  cumB <- t(apply(b,1,function(x){rev(cumsum(rev(x)))}))
  cumP <- t(apply(p,1,function(x){rev(cumsum(rev(x)))}))
  
  # Column 1 represents time series of total bookings
  # Rows represent cumulating reservations for each day.
  
  # Rearrange matrix so each row contains possible predictors.
  # Each row contains bookings available at that date.
  B <- cumB
  P <- cumP
  for(j in 1:(nrow(B)-1))
  {
    if(j < nrow(B)-1)
    {
      zb <- diag(cumB[(j+1):nrow(cumB),2:ncol(cumB)])
      zp <- diag(cumP[(j+1):nrow(cumP),2:ncol(cumP)])
    }
    else
    {
      zb <- cumB[(j+1):nrow(cumB),2:ncol(cumB)]
      zp <- cumP[(j+1):nrow(cumP),2:ncol(cumP)]
    }
    B[j,2:ncol(B)] <- c(zb, rep(NA, ncol(B)-length(zb)-1))
    P[j,2:ncol(P)] <- c(zp, rep(NA, ncol(P)-length(zp)-1))
  }
  
  #################################################################
  
  # Date Stuff
  start <- as.Date(firstres)
  yr <- as.numeric(substr(start,1,4))
  day <- as.numeric(start - as.Date(paste(as.character(yr),"-01-01",sep="")))
  
  ## public holiday stuff
  
  pubd <- window(phols$pubd,start = yr+ (day/365),end = yr+ ((day+nrow(P)-1)/365))
  pubi <- window(phols$pubi,start = yr+ (day/365),end = yr+ ((day+nrow(P)-1)/365))
  pubny <- window(phols$pubny,start = yr + (day/365),end = yr+ ((day+nrow(P)-1)/365))
  
  #################################################################
  
  # Return time series  
  obj <- data.frame(P,pubd,pubi,pubny)
  return(obj)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

## Truncata truncates data

truncata <- function(frame, prop=0.05,prope=0.05)  
{
  
  # Values at lower and upper 5%
  
  lower <- round(as.numeric(quantile(time(frame$X0),prop)),digits=0)
  upper <- round(as.numeric(quantile(time(frame$X0),1-prope)),digits=0)
  
  # Keep tsp for pubd
  
  starting <- tsp(frame$pubd)[1]+((lower-1)/365)
  ending <- starting+((upper-lower)/365)
  
  # Assign new dimensions
  
  data1 <- frame[lower:upper,1:15]
  data2<-matrix(0L,(nrow(data1)-14),15)
  for (i in 1:15){
    data2[,i] <- data1[(15-i+1):(nrow(data1)-i+1),paste("X",toString(i-1),sep="")]
  }
  
  colnames(data2)<-paste("b_t",((1:15)-1),sep="")
  
  data2 <- cbind(data2,frame[(lower+14):upper,c("pubd","pubi","pubny")])
  starting <- starting+(14/365)
  data2$pubd <- ts(data2$pubd,start=starting,frequency=365)
  data2$pubi <- ts(data2$pubi,start=starting,frequency=365)
  data2$pubny <- ts(data2$pubny,start=starting,frequency=365)
  
  #if (tsp(data2$pubd)[2]!=ending) {print("Error on the dimensions front")}
  
  return(data2)
  
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

## truncating based on manual inspection

truncatep <- function(r)
{
  tr <- list()
  
  # Truncate 5% plus any additional manual inspection requires
  prop <- rep(0.05,30)
  prope <- prop
  # Manual Inspection
  prope[1] <- 0.06
  prope[5] <- 0.09
  prope[8] <- 0.08
  prope[10] <- 0.12
  prope[15] <- 0.1
  prope[20] <- 0.35
  prope[22] <- 0.07
  prope[23] <- 0.16
  prope[25] <- 0.18
  # Truncating data
  for(i in numr)
  {
    tr[[i]]<-truncata(r[[i]], prop[i],prope[i])
  }
  return(tr)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

clean <- function(eztable)
{
  # Read in data for resaurants 1 to 30
  r <- list()
  
  for(i in 1:30)
  {
    print(paste("reading restaurant",i))
    r[[i]] <- readfunction(i)
  }
  # Plot all data
  for(i in 1:30)
  {
    plot.ts(r[[i]]$X0, main=paste("Restaurant",i))
  }
  return(r)  
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

## Arimamodel applies different arima models to data
arimamodel <- function(tri, h=14){
  # This function employs 5 different models that use a log of the total people attending a restaurant
  # Zeros have been accounted for by adding 1 to the data
  # Data has weekly frequency
  #########################################
  ##### Creating and Organising Data ######
  #########################################
  
  totpeople <- tri$b_t0
  
  # Creating log of data with weekly frequency
  
  logpeople <- ts(log(totpeople+1), start=1, frequency=7)
  
  # Create x regressor public holiday dummies
  
  xdums <- cbind(as.numeric(tri$pubd),as.numeric(tri$pubi),as.numeric(tri$pubny))
  
  colnames(xdums) <- c("going down","going up","ny")
  
  # Change public holiday dates to numeric
  nphols <- as.numeric(as.timeDate(phols$Date))
  
  # Create time series public holiday variable with appropriate dimensions
  # Dimensions - 2011 - 2015
  pholt <- as.numeric(seq(as.Date("2011-01-01"),as.Date("2015-12-31"),by="1 day"))
  
  ispubh <- ts(pholt %in% phols, start=2011, frequency=365)
  
  # Dimensions - start when y series ends
  end <- tail(time(totpeople),n=1)
  
  fispubh <- window(ispubh,start=end+(1/365))
  # Public Holidays with suspected decreases
  fpubd <- nphols[which(phols$Holiday=="1")]
  fpubd <- ts(as.numeric(pholt %in% fpubd), start=2011,frequency = 365)
  # Begin at end of y series
  fpubd <- window(fpubd,start=end+(1/365))
  # Public Holidays with suspected increases
  fpubi <- nphols[which(phols$Holiday=="2")]
  fpubi <- ts(as.numeric(pholt %in% fpubi), start=2011,frequency = 365)
  # Begin at end of y series
  fpubi <- window(fpubi,start=end+(1/365))
  # New Years Eve - suspected increases
  fpubny <- nphols[which(phols$Holiday=="3")]
  fpubny <- ts(as.numeric(pholt %in% fpubny),start=2011,frequency = 365)
  # Begin at end of y series
  fpubny <- window(fpubny,start=end+(1/365))
  
  # Create matrix of public holidays for forecasting
  
  xfor <- cbind(as.numeric(fpubd),as.numeric(fpubi),as.numeric(fpubny))
  
  colnames(xfor) <- c("going down","going up","ny")
  
  xny <- as.numeric(fpubny)
  
  #########################################
  ##### Forecasting with Models 1-5 #######
  #########################################
  
  #########################################
  
  ## Model 1
  
  # Arima fit no public holidays
  
  fit1 <- auto.arima(logpeople)
  
  # Arima fit1 forecast
  
  fc1 <- forecast(fit1,h=h)
  fc1$mean <- exp(fc1$mean)-1
  fc1$lower <- exp(fc1$lower)-1
  fc1$upper <- exp(fc1$upper)-1
  fc1$x <- tri$b_t0
  tsp(fc1$x) <- tsp(tri$pubd)
  fc1$mean <- ts(fc1$mean, start = tsp(fc1$x)[2]+1/365, frequency=365)
  tsp(fc1$upper) <- tsp(fc1$lower) <- tsp(fc1$mean)
  plot(fc1,main="Regular arima model")
  
  #########################################  
  
  ## Model 2
  
  # Arima fit with public holidays
  
  fit2 <- auto.arima(logpeople, xreg=xdums)
  
  # Arima fit2 forecast
  
  fc2 <- forecast(fit2,xreg=xfor[1:h,], h=h)
  fc2$mean <- exp(fc2$mean)-1
  fc2$lower <- exp(fc2$lower)-1
  fc2$upper <- exp(fc2$upper)-1
  fc2$x <- tri$b_t0
  tsp(fc2$x) <- tsp(tri$pubd)
  fc2$mean <- ts(fc2$mean, start = tsp(fc2$x)[2]+1/365, frequency=365)
  tsp(fc2$upper) <- tsp(fc2$lower) <- tsp(fc2$mean)
  plot(fc2,main="Arima model with public holidays")
  
  #########################################
  
  ## Model 3
  
  # Arima fit to capture regular (weekly) fluctuations in bookings # Some restaurants close regularly
  
  fit3 <- auto.arima(logpeople, xreg=seasonaldummy(logpeople), seasonal=FALSE)
  
  # Arima fit3 forecast
  
  fc3 <- forecast(fit3,xreg=seasonaldummyf(logpeople,h))
  fc3$mean <- exp(fc3$mean)-1
  fc3$lower <- exp(fc3$lower)-1
  fc3$upper <- exp(fc3$upper)-1
  fc3$x <- tri$b_t0
  tsp(fc3$x) <- tsp(tri$pubd)
  fc3$mean <- ts(fc3$mean, start = tsp(fc3$x)[2]+1/365, frequency=365)
  tsp(fc3$upper) <- tsp(fc3$lower) <- tsp(fc3$mean)
  plot(fc3,main="arima with regular seasonal fluctuation")  
  
  #########################################  
  
  ## Model 4
  
  # Arima fit with seasonality and public holidays
  
  fit4 <- auto.arima(logpeople, xreg=cbind(seasonaldummy(logpeople), xdums), seasonal=FALSE)
  
  # Arima fit4 forecast
  
  fc4 <- forecast(fit4,xreg=cbind(seasonaldummyf(logpeople,h),xfor[1:h,]))
  fc4$mean <- exp(fc4$mean)-1
  fc4$lower <- exp(fc4$lower)-1
  fc4$upper <- exp(fc4$upper)-1
  fc4$x <- tri$b_t0
  tsp(fc4$x) <- tsp(tri$pubd)
  fc4$mean <- ts(fc4$mean, start = tsp(fc4$x)[2]+1/365, frequency=365)
  tsp(fc4$upper) <- tsp(fc4$lower) <- tsp(fc4$mean)
  plot(fc4,main="arima with regular seasonal fluctuation and holiday effects")  
  
  #########################################
  
  ## Model 5
  
  #   Arima fit with seasonality and new year
  fit5 <- auto.arima(logpeople, xreg=cbind(seasonaldummy(logpeople), as.numeric(tri$pubny)), seasonal=FALSE)
  
  # Arime fit5 forecast
  
  fc5 <- forecast(fit5,xreg=cbind(seasonaldummyf(logpeople,h),xny[1:h]))
  fc5$mean <- exp(fc5$mean)-1
  fc5$lower <- exp(fc5$lower)-1
  fc5$upper <- exp(fc5$upper)-1
  fc5$x <- tri$b_t0
  tsp(fc5$x) <- tsp(tri$pubd)
  fc5$mean <- ts(fc5$mean, start = tsp(fc5$x)[2]+1/365, frequency=365)
  tsp(fc5$upper) <- tsp(fc5$lower) <- tsp(fc5$mean)
  plot(fc5,main="arima with regular seasonal fluctuation and ny effects") 
  
  ################################
  
  ## Preparing output for "best model"
  
  modelwin <- data.frame(matrix(0,nrow=1,ncol=6))
  colnames(modelwin) <- c("arima_plain","arima_pubs","arima_season","arima_season_pubs","arima_season_ny","ETS")
  
  # Model 1
  fcw1 <- modelwin
  fcw1[1] <- 1
  # Model 2
  fcw2 <- modelwin
  fcw2[2] <- 1
  # Model 3
  fcw3 <- modelwin
  fcw3[3] <- 1
  # Model 4
  fcw4 <- modelwin
  fcw4[4] <- 1
  # Model 5
  fcw5 <- modelwin
  fcw5[5] <- 1
  
  ############################
  
  ## Finalising output of best model
  
  best <- fit1
  modelwin <- fcw1
  if(fit2$aicc < best$aicc){
    best <- fit2
    modelwin <- fcw2
  } else if(fit3$aicc < best$aicc){
    best <- fit3
    modelwin <- fcw3
  } else if(fit4$aicc < best$aicc){
    best <- fit4
    modelwin <- fcw4
  }  else if(fit5$aicc < best$aicc) {
    best <- fit5
    modelwin <- fcw5
  }
  obj <- list(modelwin,best)
  
  return(obj)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

## Plotting function

plotpub <- function(frame,name)
{
  ## This function plots the total people attending a restaurant booking with public holidays data
  # Public holidays expected to increase the number of people are marked as blue 
  # Public holidays expected to decrease the number of people are marked as red
  # New Year's Eve is marked as green
  
  y <- ts(frame$b_t0,frequency =365)
  tsp(y) <- tsp(frame$pubd)
  xd <- frame$pubd
  xu <- frame$pubi
  xny <- frame$pubny
  
  logpd <- ts(as.logical(xd))
  tsp(logpd) <- tsp(xd)
  
  logpu <- ts(as.logical(xu))
  tsp(logpu) <- tsp(xd)
  
  logpny <- ts(as.logical(xny))
  tsp(logpny) <- tsp(xd)
   
  datcol <- c(1:length(y))-1
  
  obv1 <- tsp(y)[1]
  year1 <- floor(obv1)
  yfrac <- obv1 - year1
  ndays <- yfrac*365
  stdate <- as.Date(0,origin=paste(toString(year1),"01-01",sep="-"))+ndays
  datcol <- datcol+stdate
  
  n_y <- as.numeric(y)
  n_d <- as.numeric(logpd)*n_y
  n_d[n_d==0] <-NA
  n_nu <- as.numeric(logpu)*n_y
  n_nu[n_nu==0] <-NA
  n_ny <- as.numeric(logpny)*n_y
  n_ny[n_ny==0] <-NA
  plottable <- data.frame(datcol,n_y,n_d,n_nu,n_ny)
  
  p <- ggplot(data=plottable,aes(x=datcol,y=n_y))+
    geom_line(aes(color="Bookings"))+
    geom_point(data=plottable,aes(x=datcol,y=n_d,color="Decrease"),size=4)+
    geom_point(data=plottable,aes(x=datcol,y=n_nu,color="Increase"),size=4)+
    geom_point(data=plottable,aes(x=datcol,y=n_ny,color="NY"),size=4)+
    labs(x="Year",y=latex2exp("$b_{t,0}$"),colour="Legend")+ggtitle(paste("Restaurant",name))
 p <- p+scale_x_date(labels = date_format("%b-%Y"))
 print(p)
  
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

## Winning model function
bmod <- function(tr)
{
  # Defining variables to write to
  rmodels <- list()
  rwins <- list()
  
  # Modeling all restaurants with 5 different models
  # Loop returns model with lowest AICC
  for (i in numr)
  {
    rmodels[[i]] <- arimamodel(tr[[i]])
    rwins[[i]] <- rmodels[[i]][[1]]
    print(paste("restaurant",i,"modelling complete"))
  }
  
  # Loop shows which model performed best overall
  rwintot <- rwins[[1]]
  for (i in numr[-1])
  {
    rwintot <- rwins[[i]]+rwintot
  }
  rwintot <- rwintot/29
  
  return(rwintot)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

readph <- function(phols){
  
  # Change public holiday dates to numeric
  nphols <- as.numeric(as.timeDate(phols$Date))
  
  # Create time series public holiday variable with appropriate dimensions
  # Dimensions hardcoded 2011 to end 2015
  pholt <- as.numeric(seq(as.Date("2011-01-01"),as.Date("2015-12-31"),by="1 day"))
  
  # Public Holidays with suspected decreases
  pubd <- nphols[which(phols$Holiday=="1")]
  pubd <- ts(as.numeric(pholt %in% pubd), start=2011,frequency = 365)
  
  # Public Holidays with suspected increases
  pubi <- nphols[which(phols$Holiday=="2")]
  pubi <- ts(as.numeric(pholt %in% pubi), start=2011,frequency = 365)
  
  # New Years Eve - suspected increases
  pubny <- nphols[which(phols$Holiday=="3")]
  pubny <- ts(as.numeric(pholt %in% pubny),start=2011,frequency = 365)
  
  obj <- data.frame(pubd,pubi,pubny)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

pickup <- function(P,h=7){
  ## This function runs a pickup method on bookings of total people to forecast bookings for h=7 days in advance
  ## It outputs the grossing up factors for h days in advance
  
  # Start by taking log+1
  
  logP <- P+1
  
  for (i in 1:(h+1)){
    logP[i] <- log(logP[i])
  }
  
  # Vector of grossing up factors
  
  dev <- colSums(logP)
  
  adev <- rep(0,h)
  
  for (i in 1:h){
    adev[i] <- dev[i]-dev[(i+1)]
  }
  
  adev <- adev/nrow(logP)
  
  return(adev)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

fpickup <- function(P,h=7){
  ## This function outputs a forecast for h days using tr data
  ## It employs the use of pickup.R to find logarithmic additive (multiplicative) development factors
  adev <- pickup(P,h)
  
  lastdays <- tail(P,h)
  lastdays <- lastdays +1
  lastdays <- log(lastdays)
  
  # Create a logical upper triangle matrix
  tmp <- upper.tri(lastdays[,1:(h+1)],diag=FALSE)
  
  # Create an upper triangle matrix of logged bookings
  
  logu <- tmp*lastdays[,1:(h+1)]
  
  # Create a matrix for development factors
  tmpl <- matrix(0,nrow=nrow(logu),ncol=ncol(logu))
  
  for (i in 1:nrow(tmpl)){
    tmpl[,i] <- tmpl[,i]+adev[i]
  }
  
  ltmpl <- lower.tri(lastdays[,1:(h+1)],diag=TRUE)
  
  tmpl <- tmpl * ltmpl
  
  # Cumulate gross up factors
  
  tmpl <- t(apply(tmpl,1,function(x){rev(cumsum(rev(x)))}))
  
  # Incorporate existing bookings information into cumulative matrix
  
  bdat <- logu[row(logu)+1==col(logu)]
  
  ctmp <- lower.tri(lastdays[,1:(h+1)],diag=TRUE)
  
  btmpl <- matrix(0,nrow=nrow(logu),ncol=ncol(logu))
  for (i in 1:nrow(btmpl)){
    btmpl[i,] <- btmpl[i,]+bdat[i]
  }
  
  btmpl <- btmpl * ctmp
  
  logu <- logu+btmpl 
  
  # Apply grossing up factors
  fb <- tmpl + logu
  
  # Forecast for 1:h days ahead
  
  fb_t0 <- fb$b_t0
  
  # Return data to actual people booked
  
  fb_t0 <- exp(fb_t0)-1
  
  return(fb_t0)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

arimaphf <- function(P,h=7,justplot=0){
  ## This function outputs a forecast with horizon h using an arima model with public holidays data
  ## No additional bookings information is included in this model
  P <- tr[[25]]
  
  tri <- P[1:(nrow(P)-h),]
  tri$pubd <- window(P$pubd,start=tsp(P$pubd)[1],end=(tsp(P$pubd)[2]-(h/365)),frequency=365)
  
  totpeople <- tri$b_t0
  
  tsp(totpeople) <- tsp(tri$pubd)
  
  # Creating log of data with weekly frequency
  
  logpeople <- ts(log(totpeople+1), start=1, frequency=7)
  
  #########################################################
  
  ## Before fitting remove 0s from logpeople data
  logpeople[logpeople<1]<-NA
  # Autoarima should handle this
  
  # Create x regressor public holiday dummies
  
  xdums <- NULL
  excludeph <- rep(TRUE,3)
  ## We must remove the public holiday regressors if:
  # They only occur on days when logpeople is NA
  # OR
  # They sum to zero
  
  pnyna <- tri$pubny
  pnyna[is.na(logpeople)]<-NA
  
  if(sum(pnyna>0.5,na.rm=TRUE)){
    tri$pubny[is.na(logpeople)]<-NA
    xdums <- cbind(as.numeric(tri$pubny),xdums)
    excludeph[3] <- FALSE
  }
  
  pina <- tri$pubi
  pina[is.na(logpeople)]<-NA
  
  if(sum(pina>0.5,na.rm=TRUE)){
    xdums <- cbind(as.numeric(tri$pubi),xdums)
    excludeph[2] <- FALSE
  }
  
  pdna <- tri$pubd
  pdna[is.na(logpeople)]<-NA
  
  if(sum(pdna>0.5,na.rm=TRUE)){
    xdums <- cbind(as.numeric(tri$pubd),xdums)
    excludeph[1] <- FALSE
  }
  
  # Change public holiday dates to numeric
  nphols <- as.numeric(as.timeDate(phols$Date))
  
  # Create time series public holiday variable with appropriate dimensions
  # Dimensions - 2011 - 2015
  pholt <- as.numeric(seq(as.Date("2011-01-01"),as.Date("2015-12-31"),by="1 day"))
  
  ispubh <- ts(pholt %in% phols, start=2011, frequency=365)
  
  # Dimensions - start when y series ends
  endw <- tail(time(totpeople),n=h+1)
  
  ## end window for remaining forecasts
  enddata <- tail(time(P$pubd),n=1)
  
  fispubh <- window(ispubh,start=endw[[1]]+(1/365),end=enddata)
  # Public Holidays with suspected decreases
  fpubd <- nphols[which(phols$Holiday=="1")]
  fpubd <- ts(as.numeric(pholt %in% fpubd), start=2011,frequency = 365)
  # Begin at end[[1]] of y series
  fpubd <- window(fpubd,start=endw[[1]]+(1/365),end=enddata)  
  
  # Exclude from forecast if omitted from regression
  if (excludeph[1]==TRUE){
    fpubd <- NULL
  }
  
  # Public Holidays with suspected increases
  fpubi <- nphols[which(phols$Holiday=="2")]
  fpubi <- ts(as.numeric(pholt %in% fpubi), start=2011,frequency = 365)
  # Begin at end[[1]] of y series
  fpubi <- window(fpubi,start=endw[[1]]+(1/365),end=enddata)
  
  # Exclude from forecast if omitted from regression
  if (excludeph[2]==TRUE){
    fpubi <- NULL
  }
  
  # New Years Eve - suspected increases
  fpubny <- nphols[which(phols$Holiday=="3")]
  fpubny <- ts(as.numeric(pholt %in% fpubny),start=2011,frequency = 365)
  # Begin at end[[1]] of y series
  fpubny <- window(fpubny,start=endw[[1]]+(1/365),end=enddata)
  
  
  # Exclude from forecast if omitted from regression
  if (excludeph[3]==TRUE){
    fpubny <- NULL
  }
  
  # Create matrix of public holidays for forecasting
  
  xfor <- cbind(as.numeric(fpubd),as.numeric(fpubi),as.numeric(fpubny))
  if (excludeph[1]==TRUE & excludeph[2]==TRUE & excludeph[3]==TRUE){
    xfor <- NULL
  }
  
  #########################################################
  if(is.null(xdums)){fit2 <- auto.arima(logpeople)}
  else {
    fit2 <- auto.arima(logpeople, xreg=xdums)
  }
  
  # Arima fit2 forecast
  
  fc2 <- forecast(fit2,xreg=xfor[1:h,], h=h)
  fc2$mean <- exp(fc2$mean)-1
  fc2$lower <- exp(fc2$lower)-1
  fc2$upper <- exp(fc2$upper)-1
  fc2$x <- ts(tri$b_t0,frequency=365)
  tsp(fc2$x)<-tsp(tri$pubd)
  fc2$x <- window(fc2$x,end=tsp(tri$pubd)[2])
  fc2$x <- window(fc2$x,start=tsp(tri$pubd)[1])
  fc2$mean <- ts(fc2$mean, start = tsp(fc2$x)[2]+1/365, frequency=365)
  tsp(fc2$upper) <- tsp(fc2$lower) <- tsp(fc2$mean)
  fcplot <- fc2
  fcplot[is.na(fcplot)]<-0
  # Just for other_visual_stuff
  #par(mar=c(5.1, 4.1, 4.1, 9.25), xpd=TRUE)
  #plot(fcplot,ylim=range(totpeople,na.rm=TRUE),main="Restaurant 4: ARIMA Model with Public Holidays",xlab="Year",ylab="b_t0",include=70)
 
  time <- attr(fcplot$x, "tsp")
  time <- seq(time[1], attr(fcplot$mean, "tsp")[2], by=1/time[3])
  lenx <- length(fcplot$x)
  lenmn <- length(fcplot$mean)
  
  df <- data.frame(time=time,
                   x=c(fcplot$x, fcplot$mean),
                   fcplot=c(rep(NA, lenx), fcplot$mean),
                   low1=c(rep(NA, lenx), fcplot$lower[, 1]),
                   upp1=c(rep(NA, lenx), fcplot$upper[, 1]),
                   low2=c(rep(NA, lenx), fcplot$lower[, 2]),
                   upp2=c(rep(NA, lenx), fcplot$upper[, 2])
  )
  
  df <- df[(nrow(df)-70):nrow(df),]
  
  plotfc <- ggplot(df, aes(time, x),environment = environment()) +
    geom_ribbon(aes(ymin=low2, ymax=upp2), fill="yellow") +
    geom_ribbon(aes(ymin=low1, ymax=upp1), fill="orange") +
    geom_line() +
    geom_line(data=df[!is.na(df$fcplot), ], aes(time, fcplot), color="blue", na.rm=TRUE) +
    scale_x_continuous("Year") +
    scale_y_continuous(latex2exp("$\\mathbf{b}_{t}$"))+ggtitle("Restaurant 4: ARIMA Model with Public Holidays")
  
  if(justplot==1){print(plotfc)}
  return(fc2)
  
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################


arimaspline <- function(P,h=7,k=1,includefit=0,fullframe){
  ## This function outputs a forecast with horizon h using an arima model with public holidays data
  ## This function incorporates a spline variable taken from b_th with k knots
  
  #tri <- P[1:(nrow(P)-h),]
  #tri$pubd <- window(P$pubd,start=tsp(P$pubd)[1],end=(tsp(P$pubd)[2]-(h/365)),frequency=365)
  tri <- P
  # Estimating a cubic spline
  
  b_th  <- head(fullframe[,(h+1)],(n=nrow(tri)+h))
  
  # Enforcing knot positions that ignore zeros - applying knots at zero errors
  
  tmp_knots <- quantile(b_th[b_th>0],prob=((1:k)/(k+1)))
  
  totspline <- ns(b_th,knots=tmp_knots)
  
  # Bookings for predicting
  
  totsplinex <- head(totspline,nrow(tri))
  
  # Attaching time series properties to data
  
  totpeople <- tri$b_t0
  
  tsp(totpeople) <- tsp(tri$pubd)
  
  # Creating log of data with weekly frequency
  
  logpeople <- ts(log(totpeople+1), start=1, frequency=7)
  
  #########################################################
  
  ## Before fitting remove 0s from logpeople data
  logpeople[logpeople<1]<-NA
  # Autoarima should handle this
  
  #########################################################
  
  # X regressor public holiday dummies and spline variables
  xdums <- cbind(totsplinex)
  excludeph <- rep(TRUE,3)
  
  ## We must remove the public holiday regressors if:
  # They only occur on days when logpeople is NA
  # OR
  # They sum to zero
  
  pnyna <- tri$pubny
  pnyna[is.na(logpeople)]<-NA
  
  if(sum(pnyna,na.rm=TRUE)>0.5){
    tri$pubny[is.na(logpeople)]<-NA
    xdums <- cbind(as.numeric(tri$pubny),xdums)
    excludeph[3] <- FALSE
  }
  
  pina <- tri$pubi
  pina[is.na(logpeople)]<-NA
  
  if(sum(pina,na.rm=TRUE)>0.5){
    xdums <- cbind(as.numeric(tri$pubi),xdums)
    excludeph[2] <- FALSE
  }
  
  pdna <- tri$pubd
  pdna[is.na(logpeople)]<-NA
  
  if(sum(pdna,na.rm=TRUE)>0.5){
    xdums <- cbind(as.numeric(tri$pubd),xdums)
    excludeph[1] <- FALSE
  }
  
  # Dimensions - start when y series ends
  endw <- tail(time(totpeople),n=1)
  
  ## end window for remaining forecasts
  enddata <- tail(time(P$pubd),n=1)+(h/365)
   if(enddata<endw){
     stop("Date problems")
   }   
  # Begin at end[[1]] of y series
  fpubd <- window(fullframe$pubd,start=endw[[1]]+(1/365),end=enddata)
  
  # Exclude from forecast if omitted from regression
  if (excludeph[1]==TRUE){
    fpubd <- NULL
  }
  
  # Begin at end[[1]] of y series
  fpubi <- window(fullframe$pubi,start=endw[[1]]+(1/365),end=enddata)
  
  # Exclude from forecast if omitted from regression
  if (excludeph[2]==TRUE){
    fpubi <- NULL
  }
  
  # Begin at end[[1]] of y series
  fpubny <- window(fullframe$pubny,start=endw[[1]]+(1/365),end=enddata)
  
  # Exclude from forecast if omitted from regression
  if (excludeph[3]==TRUE){
    fpubny <- NULL
  }
  
  # Spline Forecast Variable
  
  splinef <- tail(totspline,n=h)
  
  # Create matrix of public holidays for forecasting
  if (h==1){
    if (k==1){
      xfor <- cbind(as.numeric(fpubd),as.numeric(fpubi),as.numeric(fpubny),c(splinef[1]),c(splinef[2]))
    } else if (k==2){
      xfor <- cbind(as.numeric(fpubd),as.numeric(fpubi),as.numeric(fpubny),c(splinef[1]),c(splinef[2]),c(splinef[3]))
    }
    else if (k==3){
      xfor <- cbind(as.numeric(fpubd),as.numeric(fpubi),as.numeric(fpubny),c(splinef[1]),c(splinef[2]),c(splinef[3]),c(splinef[4]))
    }
    else if (k==4){
      xfor <- cbind(as.numeric(fpubd),as.numeric(fpubi),as.numeric(fpubny),c(splinef[1]),c(splinef[2]),c(splinef[3]),c(splinef[4]),c(splinef[5]))
    }
  } else if (h>1){  
    if (k==1){
      xfor <- cbind(as.numeric(fpubd),as.numeric(fpubi),as.numeric(fpubny),c(splinef[1:h,1]),c(splinef[1:h,2]))
    } else if (k==2){
      xfor <- cbind(as.numeric(fpubd),as.numeric(fpubi),as.numeric(fpubny),c(splinef[1:h,1]),c(splinef[1:h,2]),c(splinef[1:h,3]))
    }
    else if (k==3){
      xfor <- cbind(as.numeric(fpubd),as.numeric(fpubi),as.numeric(fpubny),c(splinef[1:h,1]),c(splinef[1:h,2]),c(splinef[1:h,3]),c(splinef[1:h,4]))
    }
    else if (k==4){
      xfor <- cbind(as.numeric(fpubd),as.numeric(fpubi),as.numeric(fpubny),c(splinef[1:h,1]),c(splinef[1:h,2]),c(splinef[1:h,3]),c(splinef[1:h,4]),c(splinef[1:h,5]))
    }
  }  
  #########################################################
  
  fit2 <- auto.arima(logpeople, xreg=xdums)
  
  # Arima fit2 forecast
  
  fc2 <- forecast(fit2,xreg=xfor, h=h)
  fc2$mean <- exp(fc2$mean)-1
  fc2$lower <- exp(fc2$lower)-1
  fc2$upper <- exp(fc2$upper)-1
  fc2$x <- ts(tri$b_t0,frequency=365)
  tsp(fc2$x)<-tsp(tri$pubd)
  fc2$x <- window(fc2$x,end=tsp(tri$pubd)[2])
  fc2$x <- window(fc2$x,start=tsp(tri$pubd)[1])
  fc2$mean <- ts(fc2$mean, start = tsp(fc2$x)[2]+1/365, frequency=365)
  tsp(fc2$upper) <- tsp(fc2$lower) <- tsp(fc2$mean)
  fcplot <- fc2
  fcplot[is.na(fcplot)]<-0
  plot(fcplot,ylim=range(totpeople,na.rm=TRUE),main=paste(toString(h)," step Arima spline model with ",toString(k)," knots"))
  if(includefit==1){fc2 <- list(fc2,fit2)}
  return(fc2)
  
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

plotmse <- function(data){
  ## This function plots the mean squared error of various models as training set size increases
  colz <- c("blue","red","black","green")
  y <- ts(data$msepick2,start=1)
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  plot(y,col=colz[1],ylim=c(0,max(data)),xlab="Size of Training Set",ylab="Mean Squared Error")
  lines(data$msearim2,col=colz[2])
  lines(data$msearims2,col=colz[3])
  lines(data$msearimsp12,col=colz[4])
  title(main="Mean Squared Error of Models")
  
  legend("topright",inset=c(-0.35,0), legend=c("Pickup","Arima PH","Arima PH k=1","Arima PH k=2"),col=colz,pch=19)
  
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

splinefcwdiag <- function(P,h=7,k=1,fullframe){
  ## This function ensures that an h step arima forecast with splines is taking splines from the correct b_tj
  ## An h step forecast will take splines from b_t(1:h) for a forecast of length h
  
  fc <- rep(0,h)
  
  tri <- P[1:(nrow(P)-h),]
  tri$pubd <- ts(tri$pubd,start=tsp(P$pubd)[1],frequency = 365)
  
  for (j in 1:h){
    fc[j] <-arimaspline(tri,j,k,fullframe=fullframe)$mean[j]
  }   
  
  return(fc)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

ploth <- function(listy,resty,errors){
  ## This function plots the mean squared error of 7 models across multiple forecast horizons
  hmse <- visuali(listy = listy,resty)
  
  fc_h <- c(1:14)
  tranp <- data.frame(t(hmse)[(1:14),],fc_h)
  
  ##
  
  #Remove the weird ones
  tranp$Spline_1_knot[(tranp$Spline_1_knot)>max(tr[[resty]]$b_t0) ]<- NA
  tranp$Spline_2_knot[(tranp$Spline_2_knot)>max(tr[[resty]]$b_t0) ] <- NA
  tranp$Spline_3_knot[(tranp$Spline_3_knot)>max(tr[[resty]]$b_t0) ] <- NA
  tranp$Spline_4_knot[(tranp$Spline_4_knot)>max(tr[[resty]]$b_t0) ] <- NA
  # Tell me if it's weird
  messy.spline <- NULL
  if(any(is.na(tranp))){
    messy.spline <- ": I think you should Investigate"
  } else {
    messy.spine <- ""
  }
  colz <- colnames(tranp)
  tranp <- data.frame(tranp,t(errors[resty,]))
  colnames(tranp) <- c(colz,"Naive_Seasonal")
  
 out <- ggplot(tranp,aes(x=fc_h))+
    geom_line(aes(y=Pickup,colour="Pickup"))+
    geom_line(aes(y=ARIMA,colour="ARIMA"))+
    geom_line(aes(y=Spline_1_knot,colour="Spline_1_knot"))+
    geom_line(aes(y=Spline_2_knot,colour="Spline_2_knot"))+
    geom_line(aes(y=Spline_3_knot,colour="Spline_3_knot"))+
    geom_line(aes(y=Spline_4_knot,colour="Spline_4_knot"))+
    geom_line(aes(y=Previous_b_ARIMA,colour="Previous_b_ARIMA"))+
    geom_line(aes(y=Naive_Seasonal,colour="Naive_Seasonal"))+
    ggtitle(paste("Restaurant ",resty,": RMSE by Forecast Horizon",messy.spline,sep=""))+
    labs(x="Forecast Horizon",y="RMSE")+
    scale_colour_manual(guide=guide_legend(title=""),values=c("Pickup"="blue","ARIMA"="red","Spline_1_knot"="black","Spline_2_knot"="orange","Spline_3_knot"="purple","Spline_4_knot"="green","Previous_b_ARIMA"="pink","Naive_Seasonal"="yellow"))
print(out)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

rest_mod <- function(P,starttraining=100,h=14,rstnum){
  fullframe <-P
  ## This function doesn't perform actual analysis on the mse from models 
  ## - rather it just outputs the actual squared error of the models
  # Takes a while to run  
  ##
  coln <- rep(NA,h)
  for (i in 1:h){
    coln[i]<- paste("h=",toString(i),sep="")
  }
  modelz <- c("Pickup","ARIMA","Spline_1_knot","Spline_2_knot","Spline_3_knot","Spline_4_knot")
  hmse <- data.frame(matrix(0,nrow=6,ncol=h),row.names=modelz)
  colnames(hmse)<-coln
  
  errors_array <- array(0,dim=c(6,h,(nrow(P)-starttraining-h+1)),dimnames=list(modelz,coln,NULL))
  
  ##
  len <- nrow(P)-h
  numit <- len - starttraining
  
  for (size in starttraining:len){
    
    tot <- head(P,n=size+h)
    tot$pubd <- window(P$pubd,start=tsp(P$pubd)[1],end=(tsp(P$pubd)[1]+((size+h-1)/365)),frequency=365)
    
    # Making test set
    test <- tail(tot,n=h)
    test$pubd <- window(tot$pubd,start=(tsp(tot$pubd)[1]+((size)/365)),end=tsp(tot$pubd)[2],frequency=365)
    
    # Running models
    
    pick2 <- fpickup(tot,h=h)    
    # Residuals
    rpick2 <- pick2 - test$b_t0
    
    arim2 <- arimaphf(tot,h=h)
    # Residuals
    rarim2 <- arim2$mean - test$b_t0
    
    arims2 <- splinefcwdiag(tot,h,k=1,fullframe=fullframe)
    # Residuals
    rarims2 <- arims2 - test$b_t0
    
    arimsp12 <- splinefcwdiag(tot,h,k=2,fullframe=fullframe)
    # Residuals
    rarimsp12 <- arimsp12 - test$b_t0
    
    arimsp13 <- splinefcwdiag(tot,h,k=3,fullframe=fullframe)
    # Residuals
    rarimsp13 <- arimsp13 - test$b_t0
    
    arimsp14 <- splinefcwdiag(tot,h,k=4,fullframe=fullframe)
    # Residuals
    rarimsp14 <- arimsp14 - test$b_t0
    
    errors_array[,,(size-starttraining+1)] <- rbind(rpick2,rarim2,rarims2,rarimsp12,rarimsp13,rarimsp14)
    print(size)
  }
  obj <- errors_array
  
  save(obj,file=paste("Rs_R_",rstnum,".Rda",sep=""))
  
  return(obj)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

reasonablematrix <- function(vector,h=14){
  coln <- rep(NA,h)
  for (i in 1:h){
    coln[i]<- paste("h=",toString(i),sep="")
  }
  modelz <- c("Pickup","ARIMA","Spline_1_knot","Spline_2_knot","Spline_3_knot","Spline_4_knot","Previous_b_ARIMA")
  
  out <- matrix(vector,nrow=7,ncol=h,dimnames=list(modelz,coln))
  return(out)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

visuali <- function(listy,resty,h=14){
  
  rmse <- abs(listy[[resty]])
  
  numit <- dim(rmse)[3]
  
  summed <- rmse[,,1]
  for (i in 2:numit){
    summed <- rmse[,,i]+summed
  }
  
  summedrmse <- summed/numit
  
  return(summedrmse)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

## Might be nice to have a % best kind of thing too

percent_best <- function(listy,resty,h=14){
  rmse <- abs(listy[[resty]])
  
  coln <- rep(NA,h)
  for (i in 1:h){
    coln[i]<- paste("h=",toString(i),sep="")
  }
  modelz <- c("Pickup","ARIMA","Spline_1_knot","Spline_2_knot","Spline_3_knot","Spline_4_knot","Previous_b_ARIMA")
  
  percb <- matrix(0,nrow=7,ncol=h,dimnames=list(modelz,coln))
  
  numit <- dim(rmse)[3]
  
  for (i in 1:numit){
    for (h in 1:14){
      hvec <- rmse[,,i][,h]
      if (min(hvec)==hvec[1]){
        percb[,h] <- percb[,h]+c(1,0,0,0,0,0,0)
      } else if (min(hvec)==hvec[2]){
        percb[,h] <- percb[,h]+c(0,1,0,0,0,0,0)
      } else if (min(hvec)==hvec[3]){
        percb[,h] <- percb[,h]+c(0,0,1,0,0,0,0)
      } else if (min(hvec)==hvec[4]){
        percb[,h] <- percb[,h]+c(0,0,0,1,0,0,0)
      } else if (min(hvec)==hvec[5]){
        percb[,h] <- percb[,h]+c(0,0,0,0,1,0,0)
      } else if (min(hvec)==hvec[6]){
        percb[,h] <- percb[,h]+c(0,0,0,0,0,1,0)
      } else if (min(hvec)==hvec[7]){
        percb[,h] <- percb[,h]+c(0,0,0,0,0,0,1)
      }
    }
  
  }
  percb <- t(percb/numit)
  
  total <- colSums(percb)/h
  
  obj <- rbind(percb,total)
  
  return(obj)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

savepdf <- function(file, width=16, height=10)
{
  ## This function saves images nicely without whitespace
  .._fname <- paste(file,".pdf",sep="")
  pdf(.._fname, width=width/2.54, height=height/2.54, pointsize=10)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

scalemse <- function(listy,resty,h=14){
  
  thisrest <- listy[[resty]]
  
  numit <- dim(thisrest)[3]
  
  # Get 14 step errors
  
  rmsepickup.14<-rmsearim.14<-rmsearimsp1.14<-rmsearimsp2.14<-rmsearimsp3.14<-rmsearimsp4.14<-rmsearimspb<-rep(0,numit)
  all_rmses.14 <- data.frame(rmsepickup.14,rmsearim.14,rmsearimsp1.14,rmsearimsp2.14,rmsearimsp3.14,rmsearimsp4.14,rmsearimspb)
  
  for(iter in 1:numit){
    all_rmses.14[iter,] <- abs(thisrest[,,iter][,14])
  }
  
  # Set zero values to 1 otherwise it won't divide
  
  primif[[resty]][primif[[resty]]==0] <- 1
  primif[[resty]] <- tail(primif[[resty]],numit)
  # This step won't be necessary anymore because you're taking the median primif[primif==0] <- mean(primif,na.rm=TRUE)
  primif[[resty]] <- tail(primif[[resty]],numit)
  
  
  rmse.scaled <- all_rmses.14 * 0
  for (iter in 1:numit){
  for (model in 1:7){
    rmse.scaled[iter,model] <- log(all_rmses.14[iter,model] / primif[[resty]][iter])
  }
  }
  rmse.scaled<- exp(rmse.scaled)
  return(rmse.scaled)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

plotscaled <- function(rmse.scaled,resty){
  
  colnames(rmse.scaled) <- c("Pickup","ARIMA","Spline_1_knot","Spline_2_knot","Spline_3_knot","Spline_4_knot","Previous_b_ARIMA")
  
  Iteration <- c(1:nrow(rmse.scaled))
  id.scaled <- cbind(rmse.scaled,Iteration)
  
  dat_long <- melt(id.scaled,id.vars="Iteration")
  
  p <- ggplot(dat_long,aes(x=Iteration,y=value,colour=variable))+
    geom_line()+
    labs(y="RMSE")+
    ggtitle(paste("Restaurant ",resty,": Adjusted RMSE of Models",sep=""))+
    theme(legend.title=element_blank())
  
  print(p)
  
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

scaleplotbyday <- function(all_mses,resty){
  scaled <- scalemse(all_mses,resty)
  
  # Clump by seasonality
  
  # For now ignore remainders
  numw <- floor(nrow(scaled)/7)
  rmsetr <- tail(scaled,7*numw)
  # By manual inspection of tr[[4]] the last observation falls on a Tuesday
  # Thus the last forecast falls on a Tuesday
  # Thus when vectors of residuals are folded they must be folded such that the last observation falls on a Tuesday
  Days <- matrix(0,nrow=7,ncol=7)
  
  for (iter in 1:7){
    singlemod <- data.frame(t(matrix(rmsetr[,iter],nrow=7,ncol=numw)))
    Days[iter,] <- apply(singlemod,2,median)
  }
  
  time <- c("Wed","Thu","Fri","Sat","Sun","Mon","Tue")
  
  # Rows swapped for alphabetical consistency
  sw_days <- Days
  sw_days[1,]<-sw_days[2,]
  sw_days[2,]<-Days[1,]
  Days<-sw_days
  
  row.names(Days)<-c("ARIMA","Pickup","Spline_1_knot","Spline_2_knot","Spline_3_knot","Spline_4_knot","Previous_b_ARIMA")
  scale.D <- data.frame(t(Days),time)
  
  # Ordering
  scale.D$time <- as.character(scale.D$time)
  scale.D$time <- factor(scale.D$time,levels=unique(scale.D$time))
  
  meltD <- melt(scale.D,id="time")
  
  p <- ggplot(meltD,aes(x=time,y=value,colour=variable))+geom_line(aes(group=variable))+
    ggtitle(paste("Restaurant ",resty,": Adjusted RMSE of Models",sep=""))+
    labs(x="Day",y="Adjusted RMSE")+
    scale_color_manual("",labels=c("ARIMA","Pickup","Spline_1_knot","Spline_2_knot","Spline_3_knot","Spline_4_knot","Previous_b_ARIMA"),values=c("red","blue","black","green","yellow","purple","orange"))
  print(p)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

plotpb <- function(pb)
{
  ## This function plots the percent best performance of models across forecast horizons
  pb <- as.data.frame(pb)
  fc_h <- c(1:14)
  tranp <- data.frame(pb[(1:14),],fc_h)
  
  dat_long <- melt(tranp,id.vars="fc_h")
  p <- ggplot(dat_long,aes(x=fc_h,y=value,colour=variable))+geom_line()+
    ggtitle("Percent Best by Forecast Horizon")+
    labs(x="Forecast Horizon",y="Percent Best")+
    scale_colour_manual(guide=guide_legend(title=""),values=c("Pickup"="blue","ARIMA"="red","Spline_1_knot"="black","Spline_2_knot"="orange","Spline_3_knot"="purple","Spline_4_knot"="green","Previous_b_ARIMA"="yellow"))
  print(p)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

bpscaled <- function(all_rmses,resty,h=14){
  frame <- scalemse(all_rmses,resty,h)
  lframe <- log(frame)
  
  #ggplot(as.data.frame(lframe))+geom_density(aes(x=rmsepickup.14))+geom_density(aes(x=rmsearim.14))+geom_density(aes(x=rmsearimsp1.14))
  colnames(lframe)<-c("Pickup","ARIMA","Spline_1_knot","Spline_2_knot","Spline_3_knot","Spline_4_knot","Previous_b_ARIMA")
  
  iterant <- c(1:nrow(lframe))
  datf <- data.frame(lframe,iterant)
  datfm <- melt(datf,id="iterant")
  p <- ggplot(datfm)+geom_boxplot(aes(x=variable,y=value))+
    ggtitle(paste("Restaurant ",resty,": Box Plot of Log Adjusted RMSE",sep=""))+
    labs(x="",y="Log Adjusted RMSE")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  print(p)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

bpallscaled <- function(all.mod,completed,h=14,ycap=0){
  
  adj <- list()
  
  for(iter in completed){
    adj[[iter]] <- scalemse(listy = all.mod,resty = iter)
  }
  smallest <- completed[1]
  for(iter in 2:length(completed)){
    if(nrow(adj[[completed[iter]]]) <= nrow(adj[[smallest]])){
      smallest <- completed[iter]
    }
  }
  
  tot_adj <- tail(adj[[completed[1]]],nrow(adj[[smallest]]))
  
  for(iter in completed[-1]){
    tot_adj <- tot_adj + tail(adj[[iter]],nrow(adj[[smallest]]))
  }
  
  lframe <- log(tot_adj/length(completed))
  
  iterant <- c(1:nrow(lframe))
  colnames(lframe)<-c("Pickup","ARIMA","Spline_1_knot","Spline_2_knot","Spline_3_knot","Spline_4_knot","Previous_b_ARIMA")
  datf <- data.frame(lframe,iterant)
  datfm <- melt(datf,id="iterant")
  if(ycap==0){p <- ggplot(datfm)+geom_boxplot(aes(x=variable,y=value))+
    ggtitle("All Restaurants: Box Plot of Log Adjusted RMSE")+
    labs(x="",y="Log Adjusted RMSE")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } else if(ycap==1){
    p <- ggplot(datfm)+geom_boxplot(aes(x=variable,y=value))+
      ggtitle("All Restaurants: Box Plot of Log Adjusted RMSE")+
      labs(x="",y="Log Adjusted RMSE")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      scale_y_continuous(limits=c(-1,5))
  }
  print(p)
  
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

byday_alladj <- function(listy,completed,h=14){
  
  adj <- list()
  
  for(iter in completed){
    adj[[iter]] <- scalemse(listy = listy,resty = iter)
  }
  smallest <- completed[1]
  for(iter in 2:length(completed)){
    if(nrow(adj[[completed[iter]]]) <= nrow(adj[[smallest]])){
      smallest <- completed[iter]
    }
  }
  
  tot_adj <- tail(adj[[completed[1]]],nrow(adj[[smallest]]))
  
  for(iter in completed[-1]){
    tot_adj <- tot_adj + tail(adj[[iter]],nrow(adj[[smallest]]))
  }
  
  scaled <- tot_adj/length(completed)
# For now ignore remainders
numw <- floor(nrow(scaled)/7)
rmsetr <- tail(scaled,7*numw)
# By manual inspection of tr[[4]] the last observation falls on a Tuesday
# Thus the last forecast falls on a Tuesday
# Thus when vectors of residuals are folded they must be folded such that the last observation falls on a Tuesday
Days <- matrix(0,nrow=7,ncol=7)

for (iter in 1:7){
  singlemod <- data.frame(t(matrix(rmsetr[,iter],nrow=7,ncol=numw)))
  Days[iter,] <- apply(singlemod,2,median)
}

time <- c("Wed","Thu","Fri","Sat","Sun","Mon","Tue")

# Rows swapped for alphabetical consistency
sw_days <- Days
sw_days[1,]<-sw_days[2,]
sw_days[2,]<-Days[1,]
Days<-sw_days

row.names(Days)<-c("ARIMA","Pickup","Spline_1_knot","Spline_2_knot","Spline_3_knot","Spline_4_knot","Previous_b_ARIMA")
scale.D <- data.frame(t(Days),time)

# Ordering
scale.D$time <- as.character(scale.D$time)
scale.D$time <- factor(scale.D$time,levels=unique(scale.D$time))

meltD <- melt(scale.D,id="time")

ggplot(meltD,aes(x=time,y=value,colour=variable))+geom_line(aes(group=variable))+
  ggtitle("All Restaurants: Adjusted RMSE of Models")+
  labs(x="Day",y="Adjusted RMSE")+
  scale_color_manual("",labels=c("ARIMA","Pickup","Spline_1_knot","Spline_2_knot","Spline_3_knot","Spline_4_knot","Previous_b_ARIMA"),values=c("red","blue","black","green","yellow","purple","orange"))

}
##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

pb_all <- function(listy,completed){
  tot_p <- percent_best(listy,completed[1])
  
  for (iter in 2:length(completed)){
    tot_p <- tot_p + percent_best(listy,completed[iter])
  }
  
  tot_p <- tot_p/length(completed)
  
  return(tot_p)
  
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

percent_best_tru <- function(listy,resty,h=14){
  rmse <- abs(listy[[resty]])
  
  coln <- rep(NA,h)
  for (i in 1:h){
    coln[i]<- paste("h=",toString(i),sep="")
  }
  modelz <- c("Pickup","ARIMA","Spline_1_knot")
  
  percb <- matrix(0,nrow=3,ncol=h,dimnames=list(modelz,coln))
  
  numit <- dim(rmse)[3]
  
  for (i in 1:numit){
    for (h in 1:14){
      hvec <- rmse[,,i][1:3,h]
      if (min(hvec)==hvec[1]){
        percb[,h] <- percb[,h]+c(1,0,0)
      } else if (min(hvec)==hvec[2]){
        percb[,h] <- percb[,h]+c(0,1,0)
      } else if (min(hvec)==hvec[3]){
        percb[,h] <- percb[,h]+c(0,0,1)
      }
    }
    
  }
  percb <- t(percb/numit)
  
  total <- colSums(percb)/h
  
  obj <- rbind(percb,total)
  
  return(obj)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

pb_all_tru <- function(listy,completed){
  tot_p <- percent_best_tru(listy,completed[1])
  
  for (iter in 2:length(completed)){
    tot_p <- tot_p + percent_best_tru(listy,completed[iter])
  }
  
  tot_p <- tot_p/length(completed)
  
  return(tot_p)
  
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

plotpb_tru <- function(pb)
{
  ## This function plots the percent best performance of models across forecast horizons
  pb <- as.data.frame(pb)
  
  colz <- c("blue","red","black")
  y <- ts(pb$Pickup,start=1)
  par(mar=c(5.1, 4.1, 4.1, 9.25), xpd=TRUE)
  plot(y,col=colz[1],ylim=c(0,max(pb)),xlab="Forecast horizon",ylab="Percent Best")
  lines(pb$ARIMA,col=colz[2])
  lines(pb$Spline_1_knot,col=colz[3])
  title(main="Percent Best by Forecast Horizon")
  
  legend("topright",inset=c(-0.36,0), legend=c("Pickup","ARIMA","Spline_1_knot"),col=colz,pch=19)
  
  gg_pb <- cbind(t(pb[,(1:14)]),c(1:14))
  
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

naive.seas <- function(completed){
  errors <- data.frame(matrix(NA,nrow=length(completed),ncol=14))
  for(num in completed){
    b.t.0 <- tr[[num]]$b_t0
    for (p.h in 1:14){
      errors[num,p.h] <- mean(abs(b.t.0[15:length(b.t.0)]-b.t.0[1:(length(b.t.0)-14)]))
    }
  }
  return(errors)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

p.booking.arima <- function(frame,h=14,rstnum,fullframe,includefit=0){
  
  totpeople <- frame$b_t0
  
  tsp(totpeople) <- tsp(frame$pubd)
  
  # Creating log of data with weekly frequency
  
  logpeople <- ts(log(totpeople+1), start=1, frequency=7)
  
  #########################################################
  
  ## Before fitting remove 0s from logpeople data
  logpeople[logpeople<1]<-NA
  # Autoarima should handle this
  
  #########################################################
  
  # X regressor public holiday dummies and bookings
  fullxdums <- fullframe[1:(nrow(frame)+h),(h+1)]
  xdums <- head(fullxdums,nrow(frame))
  excludeph <- rep(TRUE,3)
  
  ## We must remove the public holiday regressors if:
  # They only occur on days when logpeople is NA
  # OR
  # They sum to zero
  
  pnyna <- frame$pubny
  pnyna[is.na(logpeople)]<-NA
  
  if(sum(pnyna,na.rm=TRUE)>0.5){
    frame$pubny[is.na(logpeople)]<-NA
    xdums <- cbind(as.numeric(frame$pubny),xdums)
    excludeph[3] <- FALSE
  }
  
  pina <- frame$pubi
  pina[is.na(logpeople)]<-NA
  
  if(sum(pina,na.rm=TRUE)>0.5){
    xdums <- cbind(as.numeric(frame$pubi),xdums)
    excludeph[2] <- FALSE
  }
  
  pdna <- frame$pubd
  pdna[is.na(logpeople)]<-NA
  
  if(sum(pdna,na.rm=TRUE)>0.5){
    xdums <- cbind(as.numeric(frame$pubd),xdums)
    excludeph[1] <- FALSE
  }
  
  # Dimensions - start when y series ends
  endw <- tail(time(totpeople),n=1)
  
  ## end window for remaining forecasts
  enddata <- tail(time(totpeople),n=1)+(h/365)
  if(enddata<endw){
    stop("Date problems")
  }   
  # Begin at end[[1]] of y series
  fpubd <- window(fullframe$pubd,start=endw[[1]]+(1/365),end=enddata)
  
  # Exclude from forecast if omitted from regression
  if (excludeph[1]==TRUE){
    fpubd <- NULL
  }
  
  # Begin at end[[1]] of y series
  fpubi <- window(fullframe$pubi,start=endw[[1]]+(1/365),end=enddata)
  
  # Exclude from forecast if omitted from regression
  if (excludeph[2]==TRUE){
    fpubi <- NULL
  }
  
  # Begin at end[[1]] of y series
  fpubny <- window(fullframe$pubny,start=endw[[1]]+(1/365),end=enddata)
  
  # Exclude from forecast if omitted from regression
  if (excludeph[3]==TRUE){
    fpubny <- NULL
  }
  
  # Spline Forecast Variable
  
  xfor <- tail(fullxdums,h)
  
  xfor <- cbind(as.numeric(fpubd),as.numeric(fpubi),as.numeric(fpubny),xfor)  
  #########################################################
  
  fit2 <- auto.arima(logpeople, xreg=xdums)
  
  # Arima fit2 forecast
  
  fc2 <- forecast(fit2,xreg=xfor, h=h)
  fc2$mean <- exp(fc2$mean)-1
  fc2$lower <- exp(fc2$lower)-1
  fc2$upper <- exp(fc2$upper)-1
  fc2$x <- ts(frame$b_t0,frequency=365)
  tsp(fc2$x)<-tsp(frame$pubd)
  fc2$x <- window(fc2$x,end=tsp(frame$pubd)[2])
  fc2$x <- window(fc2$x,start=tsp(frame$pubd)[1])
  fc2$mean <- ts(fc2$mean, start = tsp(fc2$x)[2]+1/365, frequency=365)
  tsp(fc2$upper) <- tsp(fc2$lower) <- tsp(fc2$mean)
  fcplot <- fc2
  fcplot[is.na(fcplot)]<-0
  # plot(fcplot,ylim=range(totpeople,na.rm=TRUE),main=paste(h,"step Arima with past bookings"))
  if(includefit==1){fc2 <- list(fc2,fit2)}
  return(fc2)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

diag.prev.b <- function(P,h=14,rstnum,fullframe){
  top <- head(P,(nrow(P)-h))
  top$pubd <- ts(top$pubd,start=tsp(P$pubd)[1],frequency = 365)
  fc <- rep(0,h)
  for (h.step in 1:h){
    fc[h.step]<-p.booking.arima(frame=top,h = h.step,rstnum = rstnum,fullframe = fullframe)$mean[h.step]
  }
  return(fc)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

rolling.orig.pre.b <- function(rstnum,starttraining=100){
  
  fullframe <-tr[[rstnum]]
  P <- fullframe
  ## This function doesn't perform actual analysis on the mse from models 
  ## - rather it just outputs the actual squared error of the models
  # Takes a while to run  
  ##
  coln <- rep(NA,h)
  for (i in 1:h){
    coln[i]<- paste("h=",toString(i),sep="")
  }
  modelz <- c("Previous_Bookings")
  hmse <- data.frame(matrix(0,nrow=1,ncol=h),row.names=modelz)
  colnames(hmse)<-coln
  
  errors_array <- array(0,dim=c(1,h,(nrow(P)-starttraining-h+1)),dimnames=list(modelz,coln,NULL))
  
  ##
  len <- nrow(P)-h
  numit <- len - starttraining
  
  for (size in starttraining:len){
    
    tot <- head(P,n=size+h)
    tot$pubd <- window(P$pubd,start=tsp(P$pubd)[1],end=(tsp(P$pubd)[1]+((size+h-1)/365)),frequency=365)
    
    # Making test set
    test <- tail(tot,n=h)
    test$pubd <- window(tot$pubd,start=(tsp(tot$pubd)[1]+((size)/365)),end=tsp(tot$pubd)[2],frequency=365)
    
    # Running models
    
    errors_array[,,(size-starttraining+1)] <- diag.prev.b(P=tot,h=h,rstnum=rstnum,fullframe=fullframe)-test$b_t0
    print(size)
  }
  obj <- errors_array
  
  save(obj,file=paste("Rs_R_",rstnum,"singlebook.Rda",sep=""))
  
  return(obj)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

make.resids <- function(pb.ns,completed,starttraining=100){
  # This function takes the forecasts from rolling origin and makes them residuals
  new.ns <- pb.ns
  for (j in completed){
    numit <- dim(pb.ns[[j]])[3]
    test <- tr[[j]]$b_t0
    for (iter in 1:numit){
      new.ns[[j]][,,iter] <- abs(pb.ns[[j]][,,iter] - test[(starttraining+iter):(starttraining+iter-1+14)])
    }
  }
  return(new.ns)
}