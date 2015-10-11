demeaningpubs <- function(startd=0,endd=10000){
  
y <- ts(rep(0,1000000),start=2011,end=2015,frequency=365)

numr <- c(1:30)[-17]

for (i in numr)
{
  x <- tr[[i]]$b_t0
  x <- x/mean(x)
  y <- y + x
}
y <- y/29
numw <- floor(length(y)/7)

y_7 <- head(y,numw*7)
y <-ts(y_7,start=tsp(y)[[1]],frequency=365) 

y_seas1 <- rep(0,numw)
y_seas2 <- y_seas3 <- y_seas4 <- y_seas5 <- y_seas6 <- y_seas7 <- y_seas1

for (j in 1:numw){
  y_seas1[j] <- y[(j*7-6)]
  
  y_seas2[j] <- y[(j*7-5)]
  
  y_seas3[j] <- y[(j*7-4)]
  
  y_seas4[j] <- y[(j*7-3)]
  
  y_seas5[j] <- y[(j*7-2)]
  
  y_seas6[j] <- y[(j*7-1)]
  
  y_seas7[j] <- y[(j*7)]
}

for (k in 1:7){
  y[(j*7-6)] <- y[(j*7-6)]/mean(y_seas1)
  
  y[(j*7-5)] <- y[(j*7-5)]/mean(y_seas2)
  
  y[(j*7-4)] <- y[(j*7-4)]/mean(y_seas3)
  
  y[(j*7-3)] <- y[(j*7-3)]/mean(y_seas4)
  
  y[(j*7-2)] <- y[(j*7-2)]/mean(y_seas5)
  
  y[(j*7-1)] <- y[(j*7-1)]/mean(y_seas6)
  
  y[(j*7)] <- y[(j*7)]/mean(y_seas7)
}

#pubs <- tr[[1]]$ispubh

#pubs <- window(pubs,start=tsp(y)[1])
#pubs <- window(pubs,end=tsp(y)[2],frequency=365)

phols <- fread("Public_Holidays_Scaled.csv")
phols <- readph(phols)

logpd <- ts(as.logical(phols$pubd))
tsp(logpd) <- tsp(phols$pubd)

logpu <- ts(as.logical(phols$pubi))
tsp(logpu) <- tsp(phols$pubd)

logpny <- ts(as.logical(phols$pubny))
tsp(logpny) <- tsp(phols$pubd)
if (startd>1){
  startd <- 2013
  endd <- 2013.25
  y <- window(y,start=startd,end=endd)
  logpd <- window(logpd,start=startd,end=endd)
  logpu <- window(logpu,start=startd,end=endd)
  logpny <- window(logpny,start=startd,end=endd)
} else {
  startd <- tsp(y)[1]
  endd <- tsp(y)[2]
  logpd <- window(logpd,start=startd,end=endd)
  logpu <- window(logpu,start=startd,end=endd)
  logpny <- window(logpny,start=startd,end=endd)
}

datcol <- c(1:length(y))-1

obv1 <- tsp(y)[1]
year1 <- floor(obv1)
yfrac <- obv1 - year1
ndays <- yfrac*365
stdate <- as.Date(0,origin=paste(toString(year1),"01-01",sep="-"),offset=ndays)
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
  labs(x="Year",y="Scaled Aggregate b_t0",colour="Legend")+ggtitle("Aggregated All Restaurants: No Seasonality")
p+scale_x_date(labels = date_format("%b-%Y"))
}