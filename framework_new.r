library(fGarch)
library(rugarch)

box_plot = function(data, p, q, name, lag = 20){
	temname = paste("Ljung-Box test of ", name, '(', as.character(p), ',', as.character(q), ') process', sep = '')
	p = c()
	for(i in 1:lag){
		tem = Box.test(data, lag = i, type = 'Ljung')
		p = c(p, tem$p.value)
	}
	plot(p, main = temname, cex = 0.5, pch = 20, xlab = "lags", ylab = "pvalue", cex.lab = 1.5, cex.axis = 1.2)
	abline(h=0.05,lty=2,col="red")
}


order_select <- function(training, p, q){
	name = "APARCH"
	arma.aparch.t = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(p,q)),distribution.model = "norm") 
	fit1 = ugarchfit(data = training, spec=arma.aparch.t)
	res = fit1@fit$residuals
	ic = c(infocriteria(fit1)[1], infocriteria(fit1)[2])
	namefile = paste("Res", name, as.character(p), as.character(q), ".png", sep = '')
	png(file=namefile, bg="white", width = 1280, height = 720, type = "cairo")
	layout(matrix(c(1,1,1,2,3,4,5,5,5), 3, 3, byrow = TRUE))
	temname = paste("Plot of residuals of ", name, '(', as.character(p), ',', as.character(q), ') process', sep = '')
	ts.plot(res, main = temname)
	temname = paste("ACF of ", name, '(', as.character(p), ',', as.character(q), ') process', sep = '')
	acf(res, 20, main = temname, cex.lab = 1.5, cex.axis = 1.2)
	temname = paste("PACF of ", name, '(', as.character(p), ',', as.character(q), ') process', sep = '')
	pacf(res, 20, main = temname, cex.lab = 1.5, cex.axis = 1.2)
	temname = paste("QQplot of ", name, '(', as.character(p), ',', as.character(q), ') process', sep = '')
	qqnorm(res, main = temname, cex.lab = 1.5, cex.axis = 1.2)
	box_plot(res, p, q, name)
	#dev.off()
	ic
}

prediction <- function(data, p , q){
	var = c()
	quantile = 0.05
	num = 0
	start = 252
	n = length(data) - start
	for(i in 1:n){
	  if(i == 600 || i == 599 || i == 649){
	    var = c(var, 0)
	    next
	  }
	  print(i)
		training = data[i:(start+i-1)]
		arma.aparch.t = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(p,q)),distribution.model = "norm") 
		print(i)
		fit1 = ugarchfit(data = training, spec=arma.aparch.t)
	  # print(fit1@fit$residuals)
		print(i)
	  if(is.null(fit1@fit$residuals)){
	    var = c(var, 0)
	    next
	  }

		pre = ugarchforecast(fit1,n.ahead=1,data=training)
		mu = as.numeric(pre@forecast$seriesFor[1])
		sd = as.numeric(pre@forecast$sigmaFor[1])
		tem = -mu - sd * qnorm(quantile, 0, 1)
		var = c(var, tem)
	}
	test = data[(start + 1): length(data)]
	correct = 0
	total = 0
  print(var)
  print(test)
  print(length(test))
	for(i in 1:length(test)){
	  if(var[i] == 0){
	    next
	  }
	  total = total + 1
    if(test[i] <= -var[i]){
      correct = correct + 1
    }
	}
	print(total)
	hit = correct/total
	var = c(var, correct)
	var = c(var, total)
	var = c(var, hit)
	var
}
