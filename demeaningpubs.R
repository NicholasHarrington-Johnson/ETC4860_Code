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

plot(y,main=paste("Aggregated Restaurants (No Seasonality)"),  xlab="Year", ylab="Total people booked")
points(time(logpd)[logpd],(y)[logpd],col="red",pch=19)
points(time(logpu)[logpu],(y)[logpu],col="blue",pch=19)
points(time(logpny)[logpny],(y)[logpny],col="green",pch=19)
legend("topright",inset=c(-0.36,0), legend=c("Down","Up","New Year"),col=c("red","blue","green"),pch=19)
