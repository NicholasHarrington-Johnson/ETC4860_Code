# Histogram of logged data

savepdf("R_4_Histogram")
hist(log(tr[[4]]$b_t0+1),main="Restaurant 4: Histogram of Logged Bookings Data",xlab="Log of total people + 1")
dev.off()

# Plot Aggregated Restaurants with no seasonality and PH dummys
savepdf("PHol_NS")
source("demeaningpubs.R")
dev.off()

# In Sample Evaluation of Forecasts
spm2 <- arimaspline(tr[[4]],h=14,k=4,includefit=1)
fit <- spm2[[2]]
fit_4 <- fitted(fit)
fit_4 <- ts(fit_4,start=tsp(tr[[4]]$pubd)[1],frequency=365)
fit$x <- ts(fit$x,start=tsp(tr[[4]]$pubd)[1],frequency=365)
savepdf("R_4_in_samplefc_sp2")
plot((exp(fit$x)-1),type="l",main="Restaurant 4 In Sample Forecast",ylab="b_t0",xlab="Year")
lines((exp(fit_4)-1),col="purple")
legend("topright",inset=c(-0.36,0), legend=c("Data","ARIMA_4_knot"),col=c("black","purple"),pch=19)
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