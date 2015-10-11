b_t0 <- tr[[4]]$b_t0
b_t14 <- tr[[4]]$b_t14
tsp(b_t0)<-tsp(tr[[4]]$pubd)

datcol <- c(1:length(b_t0))-1

obv1 <- tsp(b_t0)[1]
year1 <- floor(obv1)
yfrac <- obv1 - year1
ndays <- round(yfrac*365)
stdate <- as.Date(0,origin=paste(toString(year1),"01-01",sep="-"))+ndays
datcol <- datcol+stdate

moncol <- format(as.timeDate(datcol),"%b")

datdates <- data.frame(b_t0,moncol,b_t14)
datdates$moncol <- as.character(datdates$moncol)
datdates$moncol <- factor(datdates$moncol,levels=unique(datdates$moncol))


savepdf("Monthly_b_t0")
ggplot(datdates,aes(x=moncol,y=b_t0))+geom_violin(fill="orange")+
  geom_point(aes(size=b_t14),colour="blue",position="jitter")+
  ggtitle("Restaurant 4: Monthly Bookings")+
  labs(x="Month",y="b_t0")
dev.off()

daycol <- format(as.timeDate(datcol),"%a")

daydates <- data.frame(b_t0,daycol,b_t14)

daydates$daycol <- as.character(daydates$daycol)
daydates$daycol <- factor(daydates$daycol,levels=unique(daydates$daycol))

savepdf("Daily_b_t0")
ggplot(daydates,aes(x=daycol,y=b_t0))+geom_violin(fill="orange")+
  geom_point(aes(size=b_t14),colour="blue",position="jitter")+
  ggtitle("Restaurant 4: Daily Bookings")+
  labs(x="Day",y="b_t0")
dev.off()
