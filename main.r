###This part is for training set with AIC, BIC and residual analysis
rm(list = ls())
library("tseries")
library("rugarch")
library("dst")
library("fGarch")
SPY1 = get.hist.quote(instrument="spy", start="2006-01-01", end="2009-12-31", quote="AdjClose", provider="yahoo",compression="d", retclass="zoo")
SPY1 = diff(SPY1)/SPY1[-length(SPY1)]
SPY2 = get.hist.quote(instrument="spy", start="2013-01-01", end="2016-12-31", quote="AdjClose", provider="yahoo",compression="d", retclass="zoo")
SPY2 = diff(SPY2)/SPY2[-length(SPY2)]


total = 3
data = SPY2
dist = 'norm' ##norm, snorm, std, sstd
AIC = matrix(0, total, total)
BIC = matrix(0, total, total)
for(p in 1:total){
	for(q in 1:total){
		ic = order_select(data, p, q, dist)
		AIC[p, q] = ic[1]
		BIC[p, q] = ic[2]
	}
}
aic = min(AIC)
bic = min(BIC)
chosenaic = which(AIC==aic,arr.ind=T) #prediction
chosenbic = which(BIC==bic,arr.ind=T) #truemodel

###This part is for forecasting
chosenbic = c(1, 1)
res = prediction(data, chosenbic[1], chosenbic[2], dist)
var = res[1:(length(res)-1)]	
hit = res[length(res)]
Time = time(SPY2)[253:length(SPY2)]
chosen = which(var > 0)
var = var[chosen]
Time = Time[chosen]
png(file="VaR_prediction_1_gjrgarch", bg="white", width = 1280, height = 720, type = "cairo")
plot(x=Time, y=var,"l", main="VaR_prediction")
dev.off()
