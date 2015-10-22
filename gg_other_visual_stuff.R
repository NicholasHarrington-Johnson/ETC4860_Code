# Raw Data
#par(mar=c(5.1, 4.1, 4.1, 9.25), xpd=TRUE)
b_t0 <- ts(tr[[4]]$b_t0,start=tsp(tr[[4]]$pubd)[1],frequency=365)
datap <- autoplot(b_t0)+ggtitle("Restaurant 4")+labs(x="Year",y=latex2exp("$\\mathbf{b}_{t}$"))
savepdf("R4_b_t0")
  print(datap)
dev.off()

# Histogram of logged data
histp <- qplot(log(tr[[4]]$b_t0+1),geom="histogram",binwidth=0.1)+
  labs(x=latex2exp("$\\ln(\\mathbf{b}_{t}+1)$"))+
  ggtitle("Restaurant 4: Histogram of Logged Bookings Data")
savepdf("R_4_Histogram")
  print(histp)
dev.off()

# Restaurant 4 by month and by day with b_t14 there too
source("temporal_periods.R")

# Plot Aggregated Restaurants with no seasonality and PH dummys
source("demeaningpubs.R")
savepdf("PHol_NS")
demeaningpubs()
dev.off()
# Close Up
savepdf("PHol_NS_CloseUp")
demeaningpubs(startd=2013,endd=2013.25)
dev.off()

# Plot Restaurant 4 and Public Holidays Information
savepdf("Restaurant4")
plotpub(tr[[4]],4)
dev.off()

# Plot Restaurant 4 ARIMA with Public Holidays Forecast for 2 weeks
savepdf("Restaurant_4ARIMAForecast")
arimaphf(tr[[4]],h=14,justplot = 1)
dev.off()

# Plot in sample forecast of Spline models
# In Sample Evaluation of Forecasts
# One knot
P <- head(tr[[4]],(nrow(tr[[4]])-14))
P$pubd <- ts(P$pubd,start=tsp(tr[[4]]$pubd)[1],frequency=365)
spm1 <- arimaspline(P,h=14,k=1,includefit=1,fullframe=tr[[4]])
fit1 <- spm1[[2]]
fit_1 <- fitted(fit1)
fit_1 <- ts(fit_1,start=tsp(tr[[4]]$pubd)[1],frequency=365)
fit1$x <- ts(fit1$x,start=tsp(tr[[4]]$pubd)[1],frequency=365)
# Four knots
spm2 <- arimaspline(P,h=14,k=4,includefit=1,fullframe=tr[[4]])
fit <- spm2[[2]]
fit_4 <- fitted(fit)
fit_4 <- ts(fit_4,start=tsp(tr[[4]]$pubd)[1],frequency=365)
fit$x <- ts(fit$x,start=tsp(tr[[4]]$pubd)[1],frequency=365)
######################
Data <- exp(fit$x)-1
Spline_1_knot <- exp(fit_1)-1
Spline_4_knot <- exp(fit_4)-1
# Time
time <- attr(fit$x, "tsp")
time <- seq(time[1], time[2], by=1/time[3])
######################
fulldat <- data.frame(time,Data,Spline_1_knot,Spline_4_knot)
mfulldat <- melt(fulldat,id="time")
insample.p <- ggplot(mfulldat,aes(x=time,y=value,colour=variable,linetype=variable))+geom_line()+labs(x="Year",y=latex2exp("$\\mathbf{b}_{t}$"),colour="")+
  ggtitle("Restaurant 4: In Sample Forecast")+
  scale_linetype_manual(values=c(1,2,3),guide='none')+
  scale_color_manual(values=c("red","green","blue"))

savepdf("R_4_in_samplefc_sp2")
print(insample.p)
dev.off()

# Zoom in
zfulldat <- fulldat[(nrow(fulldat)-500):(nrow(fulldat)-400),]
zmfulldat <- melt(zfulldat,id="time")
zoom.p <- ggplot(zmfulldat,aes(x=time,y=value,colour=variable,linetype=variable))+geom_line()+labs(x="Year",y=latex2exp("$\\mathbf{b}_{t}$"),colour="")+
  ggtitle("Restaurant 4: In Sample Forecast")+
  scale_linetype_manual(values=c(1,2,3),guide='none')+
  scale_color_manual(values=c("red","green","blue"))
savepdf("R_4_in_samplefc_sp2_zoom")
print(zoom.p)
dev.off()

# Plot %Best by Forecast Horizon
savepdf("R_4_PB_Graphic")
plotpb(percent_best(all.mod,4))
dev.off()

# Plot scaled RMSE

savepdf("R_4_ScaledRMSE")
scaleplotbyday(all.mod,4)
dev.off()

# Plot h step RMSE
savepdf("R_4_hRMSE")
ploth(all.mod,4,errors)
dev.off()

# Plot Box Plot of Scaled RMSE
savepdf("R_4_BoxPlot_ScaledlRMSE")
bpscaled(all.mod,4)
dev.off()

# Two Week Forecast
# This function saves as a pdf within the function
source("2_week_R_4.R")
two_week_R_4(tr[[4]])

# Box Plot of All Restaurants
savepdf("BP_All")
bpallscaled(all.mod,completed)
dev.off()
# Zoomed Box Plot of All Restaurants
savepdf("BP_All_Zoom")
bpallscaled(all.mod,completed,ycap=1)
dev.off()

# PB of All Restaurants
savepdf("PB_All")
plotpb(pb_all(all.mod,completed))
dev.off()

# Instability stuff

savepdf("R_15_misbehaving")
plotscaled(scalemse(all.mod,15),15)
dev.off()

# Resids forecasted at 1500 in restaurant 15 around the 150th iteration.
problemrow <- 251# Found problem here # Huge forecasts for no reason
P <- head(tr[[15]],problemrow)
P$pubd <- ts(P$pubd,start=tsp(tr[[15]]$pubd)[1],frequency = 365)
arimaspline(P = P,h = 14,k = 4,fullframe = tr[[15]])
beta <- c(2.9083,  3.3554,  2.3786,  9.1840,  7.9785)
totsplinerow <- head(tr[[15]]$b_t14,problemrow+14)
nknots <-4
knots <- quantile(totsplinerow[totsplinerow>0],prob=(1:nknots)/(nknots+1))
totsplinex <- ns(totsplinerow,knots=knots)
pred <- totsplinex%*%beta
pred <- data.frame(tail(log(tr[[15]]$b_t14+1),length(pred)),pred)
colnames(pred)<-c("log_bt14","Splines")
splinep <- ggplot(pred,aes(x=c(1:nrow(pred))))+
  geom_point(aes(y=Splines))+
  geom_line(aes(y=log_bt14))+
  labs(x="",y=latex2exp("$\\ln(y+1)$"))+
  ggtitle(latex2exp("$f(b_{t,14})$ and $\\ln(b_{t,14}+1)$"))
savepdf("beta_times_spline")
print(splinep)
dev.off()


