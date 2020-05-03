#install.packages("rugarch", dependencies = TRUE)

box_plot = function(data, p, q, name, lag = 20){
	temname = paste("Ljung-Box test of ", name, '(', as.character(p), ',', as.character(q), ') process','(', dist, ')', sep = '')
	p = c()
	for(i in 1:lag){
		tem = Box.test(data, lag = i, type = 'Ljung')
		p = c(p, tem$p.value)
	}
	plot(p, main = temname, cex = 0.5, pch = 20, xlab = "lags", ylab = "pvalue", cex.lab = 1.5, cex.axis = 1.2)
	abline(h=0.05,lty=2,col="red")
}


order_select <- function(training, p, q, dist){
	name = "1armaGARCH"
	#fit1 = garchFit(substitute(~ garch(a,b),list(a=p, b=q)),data=training, cond.dist = dist)
	spec = ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(p,q)), mean.model = list(armaOrder = c(0,0)), distribution.model = dist)
	fit1 = ugarchfit(spec=spec, data = training)
	if(is.null(fit1@fit$residuals)){
		ic = c(0, 0)
		return(ic)
	}
	non_norm_res = fit1@fit$residuals
	sigma = fit1@fit$sigma
	res = non_norm_res / sigma
	ic = infocriteria(fit1)[1:2]
	namefile = paste("Res", name, as.character(p), as.character(q), dist, ".png", sep = '')
	png(file=namefile, bg="white", width = 1280, height = 720, type = "cairo")
	layout(matrix(c(1,1,1,2,3,4,5,5,5), 3, 3, byrow = TRUE))
	temname = paste("Plot of residuals of ", name, '(', as.character(p), ',', as.character(q), ') process','(', dist, ')', sep = '')
	ts.plot(res, main = temname)
	temname = paste("ACF of ", name, '(', as.character(p), ',', as.character(q), ') process', '(', dist, ')',sep = '')
	acf(res, 20, main = temname, cex.lab = 1.5, cex.axis = 1.2)
	temname = paste("PACF of ", name, '(', as.character(p), ',', as.character(q), ') process','(', dist, ')', sep = '')
	pacf(res, 20, main = temname, cex.lab = 1.5, cex.axis = 1.2)
	temname = paste("QQplot of ", name, '(', as.character(p), ',', as.character(q), ') process', '(', dist, ')',sep = '')
	qqnorm(res, main = temname, cex.lab = 1.5, cex.axis = 1.2)
	box_plot(res, p, q, name)
	dev.off()
	ic
}

prediction <- function(data, p , q, dist){
	var = c()
	quantile = 0.05
	num = 0
	start = 252
	n = length(data) - start
	for(i in 1:n){
		print(i)
		training = data[i:(start+i-1)]
		#fit1 = garchFit(substitute(~ garch(a,b),list(a=p, b=q)),data=training, cond.dist = dist)
		spec = ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(p,q)), mean.model = list(armaOrder = c(0,0)), distribution.model = dist)
		fit1 = ugarchfit(spec=spec, data = training)
		if(is.null(fit1@fit$residuals)){
		    var = c(var, 0)
		    next
		}
		#pre = predict(fit1, n.ahead=1)
		pre = ugarchforecast(fit1, n.ahead = 1, data=data)
		mu = as.numeric(pre@forecast$seriesFor)
		sd = as.numeric(pre@forecast$sigmaFor)
		tem = -mu - sd * qnorm(quantile, 0, 1)
		var = c(var, tem)
	}
	test = data[(start + 1):length(data)]
	correct = 0
	total = 0
	for(i in 1:length(test)){
	  if(var[i] == 0){
	    next
		}
		  total = total + 1
	    if(test[i] <= -var[i]){
	      correct = correct + 1
	    }
	}
	hit = correct/total
	var = c(var, hit)
	var
}
