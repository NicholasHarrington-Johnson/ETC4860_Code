# Raw Data
savepdf("R4_b_t0")
par(mar=c(5.1, 4.1, 4.1, 9.25), xpd=TRUE)
b_t0 <- ts(tr[[4]]$b_t0,start=tsp(tr[[4]]$pubd)[1],frequency=365)
plot(b_t0,type="l",main="Restaurant 4",ylab="b_t0",xlab="Year")
dev.off()

# Histogram of logged data

savepdf("R_4_Histogram")
hist(log(tr[[4]]$b_t0+1),main="Restaurant 4: Histogram of Logged Bookings Data",xlab="Log of total people + 1")
dev.off()

# Plot Aggregated Restaurants with no seasonality and PH dummys
savepdf("PHol_NS")
source("demeaningpubs.R")
dev.off()

# Plot Restaurant 4 and Public Holidays Information
savepdf("Restaurant4")
plotpub(tr[[4]],4)
dev.off()

# Plot Restaurant 4 ARIMA with Public Holidays Forecast for 2 weeks
savepdf("Restaurant_4ARIMAForecast")
arimaphf(tr[[4]],h=14)
dev.off()

# In Sample Evaluation of Forecasts
# One knot
spm1 <- arimaspline(tr[[4]],h=14,k=1,includefit=1)
fit1 <- spm1[[2]]
fit_1 <- fitted(fit1)
fit_1 <- ts(fit_1,start=tsp(tr[[4]]$pubd)[1],frequency=365)
fit1$x <- ts(fit1$x,start=tsp(tr[[4]]$pubd)[1],frequency=365)
# Four knots
spm2 <- arimaspline(tr[[4]],h=14,k=4,includefit=1)
fit <- spm2[[2]]
fit_4 <- fitted(fit)
fit_4 <- ts(fit_4,start=tsp(tr[[4]]$pubd)[1],frequency=365)
fit$x <- ts(fit$x,start=tsp(tr[[4]]$pubd)[1],frequency=365)
savepdf("R_4_in_samplefc_sp2")
par(mar=c(5.1, 4.1, 4.1, 9.25), xpd=TRUE)
plot((exp(fit$x)-1),type="l",main="Restaurant 4 In Sample Forecast (h=14)",ylab="b_t0",xlab="Year")
lines((exp(fit_1)-1),col="red")
lines((exp(fit_4)-1),col="purple")
legend("topright",inset=c(-0.36,0), legend=c("Data","ARIMA_1_knot","ARIMA_4_knot"),col=c("black","red","purple"),pch=19)
dev.off()

# Plot %Best by Forecast Horizon
savepdf("R_4_PB_Graphic")
plotpb(percent_best(all_mses,1))
dev.off()

# Plot scaled RMSE

savepdf("R_4_ScaledRMSE")
scaleplotbyday(all_mses,1)
dev.off()

# Plot h step RMSE
savepdf("R_4_hRMSE")
ploth(visuali(all_mses,1))
dev.off()

# Plot Box Plot of Scaled RMSE
savepdf("R_4_BoxPlot_ScaledlRMSE")
bpscaled(all_mses,1)
dev.off()