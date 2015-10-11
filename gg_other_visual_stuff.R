# Raw Data
savepdf("R4_b_t0")
#par(mar=c(5.1, 4.1, 4.1, 9.25), xpd=TRUE)
b_t0 <- ts(tr[[4]]$b_t0,start=tsp(tr[[4]]$pubd)[1],frequency=365)
autoplot(b_t0)+ggtitle("Restaurant 4")+labs(x="Year",y="b_t0")
dev.off()

# Histogram of logged data

savepdf("R_4_Histogram")
qplot(log(tr[[4]]$b_t0+1),geom="histogram",binwidth=0.1)+labs(x="Log(b_t0+1)")+ggtitle("Restaurant 4: Histogram of Logged Bookings Data")
dev.off()

# Plot Aggregated Restaurants with no seasonality and PH dummys
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
savepdf("R_4_in_samplefc_sp2")
fulldat <- data.frame(time,Data,Spline_1_knot,Spline_4_knot)
mfulldat <- melt(fulldat,id="time")
ggplot(mfulldat,aes(x=time,y=value,colour=variable,linetype=variable))+geom_line()+labs(x="Year",y="b_t0",colour="")+
  ggtitle("Restaurant 4: In Sample Forecast")+
  scale_linetype_manual(values=c(1,2,3),guide='none')+
    scale_color_manual(values=c("red","green","blue"))
dev.off()

# Zoom in
savepdf("R_4_in_samplefc_sp2_zoom")
zfulldat <- fulldat[(nrow(fulldat)-500):(nrow(fulldat)-400),]
zmfulldat <- melt(zfulldat,id="time")
ggplot(zmfulldat,aes(x=time,y=value,colour=variable,linetype=variable))+geom_line()+labs(x="Year",y="b_t0",colour="")+
  ggtitle("Restaurant 4: In Sample Forecast")+
  scale_linetype_manual(values=c(1,2,3),guide='none')+
  scale_color_manual(values=c("red","green","blue"))
dev.off()

# Plot %Best by Forecast Horizon
savepdf("R_4_PB_Graphic")
plotpb(percent_best(big_un,4))
dev.off()

# Plot scaled RMSE

savepdf("R_4_ScaledRMSE")
scaleplotbyday(big_un,4)
dev.off()

# Plot h step RMSE
savepdf("R_4_hRMSE")
ploth(big_un,4)
dev.off()

# Plot Box Plot of Scaled RMSE
savepdf("R_4_BoxPlot_ScaledlRMSE")
bpscaled(big_un,4)
dev.off()

# Two Week Forecast
# This function saves as a pdf within the function
two_week_R_4(tr[[4]])
