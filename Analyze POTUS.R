# ------------------------------------------------------------------------
# ---- Packages ----------------------------------------------------------
# ------------------------------------------------------------------------

require(data.table)
require(doParallel)
require(foreach)
require(zoo)
require(forecast)
require(ggplot2)
require(gridExtra)
require(randomForest)
require(leaps)

# -----------------------------------------------------------------------------------
# ---- Functions --------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# ---- prints the data types of each column in a data frame ----------------------------------------------------------------------------------------------------

types = function(dat)
{
	dat = data.frame(dat)
  
	Column = sapply(1:ncol(dat), function(i)
	(
		colnames(dat)[i]
    ))
  
	Data_Type = sapply(1:ncol(dat), function(i)
    (
		class(dat[,i])
    ))
  
	results = data.frame(cbind(Column, Data_Type))	
	results
}

# ---- plots 6 residual plots ---------------------------------------------------------------------------------------------------------------------------------

residplots = function(actual, fit, binwidth = NULL, from = NULL, to = NULL, by = NULL, histlabel.y = -10, n = NULL, basefont = 20)
{
	require(ggplot2)
	
	residual = actual - fit 
	DF = data.frame("actual" = actual, "fit" = fit, "residual" = residual)
	
    rvfPlot = ggplot(DF, aes(x = fit, y = residual)) + 
			  geom_point(na.rm = TRUE) +
			  stat_smooth(method = "loess", se = FALSE, na.rm = TRUE, color = "blue") +
			  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
			  xlab("Fitted values") +
			  ylab("Residuals") +
			  ggtitle("Residual vs Fitted Plot") + 
			  theme_bw(base_size = basefont) +
			  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
    
	ggqq = function(x, distribution = "norm", ..., conf = 0.95, probs = c(0.25, 0.75), note = TRUE, alpha = 0.33, main = "", xlab = "\nTheoretical Quantiles", ylab = "Empirical Quantiles\n")
	{
		# compute the sample quantiles and theoretical quantiles
		q.function = eval(parse(text = paste0("q", distribution)))
  		d.function = eval(parse(text = paste0("d", distribution)))
  		x = na.omit(x)
  		ord = order(x)
  		n = length(x)
  		P = ppoints(length(x))
  		DF = data.frame(ord.x = x[ord], z = q.function(P, ...))
  
  		# compute the quantile line
  		Q.x = quantile(DF$ord.x, c(probs[1], probs[2]))
  		Q.z = q.function(c(probs[1], probs[2]), ...)
  		b = diff(Q.x) / diff(Q.z)
  		coef = c(Q.x[1] - (b * Q.z[1]), b)
  
  		# compute the confidence interval band
  		zz = qnorm(1 - (1 - conf) / 2)
  		SE = (coef[2] / d.function(DF$z, ...)) * sqrt(P * (1 - P) / n)
  		fit.value = coef[1] + (coef[2] * DF$z)
  		DF$upper = fit.value + (zz * SE)
  		DF$lower = fit.value - (zz * SE)
  
  		# plot the qqplot
  		p = ggplot(DF, aes(x = z, y = ord.x)) + 
    		geom_point(color = "black", alpha = alpha) +
    		geom_abline(intercept = coef[1], slope = coef[2], size = 1, color = "blue") +
    		geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15) +
    		coord_cartesian(ylim = c(min(DF$ord.x), max(DF$ord.x))) + 
    		labs(x = xlab, y = ylab) +
    		theme_bw(base_size = basefont) +
			theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
						
  		# conditional additions
  		if(main != "")(p = p + ggtitle(main))
  		
  		return(p)
	}

    qqPlot = ggqq(residual, 
				  alpha = 1,				  
				  main = "Normal Q-Q Plot", 
				  xlab = "Theoretical Quantiles", 
				  ylab = "Residuals")
    
    rvtPlot = ggplot(data.frame("x" = 1:length(DF$residual), "y" = DF$residual), aes(x = x, y = y)) + 
			  geom_line(na.rm = TRUE) +
			  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
			  xlab("Obs. Number") +
			  ylab("Residuals") +
			  ggtitle("Residual Time Series") + 
			  theme_bw(base_size = basefont) +
			  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
        
	variogramDF = function(x)
	{
		n = length(x) - 2
	
		num = sapply(1:n, function(k)
						  sapply(1:(length(x) - k), function(i)
													x[i + k] - x[i]))
	
		num = sapply(1:length(num), function(j)
									var(num[[j]]))

		den = var(sapply(1:(length(x) - 1), function(i)
											x[i + 1] - x[i]))
	
		val = num / den
	
		DF = data.frame("Lag" = 1:n, "Variogram" = val)
	
		return(DF)
	}

	DFv = variogramDF(x = DF$residual)

	varioPlot = ggplot(DFv, aes(x = Lag, y = Variogram)) + 
				geom_point() +
				geom_line(color = "blue") +
				xlab("Lag") +
				ylab("Variogram") +
				ggtitle("Variogram of Residuals") + 
				theme_bw(base_size = basefont) +
				theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
	
	test = t.test(DF$residual)
	
	CI = data.frame("x" = test$estimate, 
					"LCB" = test$conf.int[1], 
					"UCB" = test$conf.int[2], 
					row.names = 1)
	
	histPlot = ggplot(DF, aes(x = residual)) +
			   geom_histogram(color = "white", fill = "black", binwidth = binwidth) +
			   geom_segment(data = CI, aes(x = LCB, xend = LCB, y = 0, yend = Inf), color = "blue") +
			   geom_segment(data = CI, aes(x = UCB, xend = UCB, y = 0, yend = Inf), color = "blue") +
			   annotate("text", x = CI$x, y = histlabel.y, 
						label = "T-Test C.I.", size = 5, 
						color = "blue", fontface = 2) + 
			    ggtitle("Residual Histogram") +
			   labs(x = "Residuals", y = "Frequency") +
	 		   theme_bw(base_size = basefont) +
			   theme(legend.key.size = unit(.25, "in"),
					 legend.position = "bottom", plot.title = element_text(hjust = 0.5))
	
	if(class(from) != "NULL" & class(to) != "NULL" & class(by) != "NULL") (histPlot = histPlot + scale_x_continuous(breaks = seq(from = from, to = to, by = by)))
	
	ggacf = function(x, n = NULL, conf.level = 0.95, main = "ACF Plot", xlab = "Lag", ylab = "Autocorrelation", basefont = 20) 
	{
		if(class(n) == "NULL")
		{
			n = length(x) - 2
		}
	
		ciline = qnorm((1 - conf.level) / 2) / sqrt(length(x))
		bacf = acf(x, lag.max = n, plot = FALSE)
		bacfdf = with(bacf, data.frame(lag, acf))
		bacfdf = bacfdf[-1,]
		
		p = ggplot(bacfdf, aes(x = lag, y = acf)) + 
			geom_bar(stat = "identity", position = "dodge", fill = "black") +
			geom_hline(yintercept = -ciline, color = "blue", size = 1) +
			geom_hline(yintercept = ciline, color = "blue", size = 1) +
			geom_hline(yintercept = 0, color = "red", size = 1) +
			labs(x = xlab, y = ylab) +
			ggtitle(main) +
			theme_bw(base_size = basefont) +
			theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

		return(p)
	}

	acfPlot = ggacf(x = DF$residual, main = "ACF Plot of Residuals", basefont = basefont, n = n)
	
    return(list("rvfPlot" = rvfPlot, 
				"qqPlot" = qqPlot, 
				"rvtPlot" = rvtPlot, 
				"varioPlot" = varioPlot, 
				"histPlot" = histPlot, 
				"acfPlot" = acfPlot))
}

# ---- plots the fitted v. actuals of a predictive model -----------------------------------------------------------------------------------------------------------------------------------------------

modplot = function(mod, xaxis = NULL, y.levels = NULL, actuals = NULL, n.ahead = NULL, level = c(80, 95), newdata = NULL, limits = NULL, nnet.y.min = 0, nnet.y.max = 1, xlab = "Observation", ylab = "Value", main = "Fitted v. Actual\nPrediction Plot", basefont = 20)
{
	require(ggplot2)
	require(forecast)
	# require(nnetpredint)
	# require(dplyr)
	
	# build fits

	if("HoltWinters" %in% class(mod))
	{
		fits = as.numeric(fitted(mod)[,1])
		fits = c(rep(NA, length(mod$x) - length(fits)), fits)
		
	} else if("nnet" %in% class(mod))
	{
		fits = (as.numeric(fitted(mod)) * (nnet.y.max - nnet.y.min)) + nnet.y.min
		
	} else if("randomForest" %in% class(mod))
	{
		fits = as.numeric(mod$predicted)
		
	} else
	{
		fits = as.numeric(fitted(mod))
	}

	# build actuals
	
	if(class(actuals) == "NULL")
	{
		if("HoltWinters" %in% class(mod))
		{
			actuals = as.numeric(mod$x)
			
		} else if("nnet" %in% class(mod))
		{
			actuals = ((as.numeric(resid(mod)) + as.numeric(fitted(mod))) * (nnet.y.max - nnet.y.min)) + nnet.y.min
			
		} else if("randomForest" %in% class(mod)) 
		{
			actuals = as.numeric(mod$y)
			
		} else
		{
			actuals = as.numeric(resid(mod)) + fits
		}
		
		if(class(n.ahead) != "NULL")
		{
			actuals = c(actuals, rep(NA, n.ahead))
		}
	}
	
	# build xaxis

	if(class(xaxis) == "NULL")
	{
		if(class(n.ahead) == "NULL")
		{
			xaxis = 1:length(fits)
			
		} else
		{
			xaxis = 1:(length(fits) + n.ahead)
		}	
	}
	
	# initialize upper and lower prediction interval values
	
	upper1 = as.numeric(rep(NA, length(fits)))
	upper2 = upper1
	lower1 = upper1
	lower2 = upper1
	
	# initialize a color vector to seperate training data from testing data
	
	color = factor(rep(0, length(fits)), levels = c(0, 1))
	
	# compute predictions if n.ahead is specified 
		# also update fits, upper and lower prediction interval values, and color 
	
	if(class(n.ahead) != "NULL")
	{
		if(class(mod)[1] == "lm")
		{
			predictions = data.frame(forecast(mod, newdata = newdata, h = n.ahead, level = level))
			
		} else if(class(mod)[1] == "glm")
		{
			predictions = data.frame(predict(mod, newdata = newdata, n.ahead = n.ahead, se.fit = TRUE))
			
			predictions = data.frame("fit" = predictions$fit, 
									 "lower2" = predictions$fit - predictions$se.fit,
									 "upper2" = predictions$fit + predictions$se.fit,
									 "lower1" = predictions$fit - (3 * predictions$se.fit),
									 "upper1" = predictions$fit + (3 * predictions$se.fit))
			
		} else if(class(mod)[1] == "ARIMA")
		{
			predictions = data.frame(forecast(mod, h = n.ahead, level = level, xreg = newdata))
			
		} else if(class(mod)[1] == "nnet")
		{				
			predictions = data.frame("fit" = predict(mod, newdata = newdata, n.ahead = n.ahead), 
									 "lower2" = rep(NA, n.ahead),
									 "upper2" = rep(NA, n.ahead),
									 "lower1" = rep(NA, n.ahead),
									 "upper1" = rep(NA, n.ahead))
									 
			predictions$fit = (predictions$fit * (nnet.y.max - nnet.y.min)) + nnet.y.min
			predictions$lower2 = (predictions$lower2 * (nnet.y.max - nnet.y.min)) + nnet.y.min
			predictions$upper2 = (predictions$upper2 * (nnet.y.max - nnet.y.min)) + nnet.y.min
			predictions$lower1 = (predictions$lower1 * (nnet.y.max - nnet.y.min)) + nnet.y.min
			predictions$upper1 = (predictions$upper1 * (nnet.y.max - nnet.y.min)) + nnet.y.min
			
		} else if("randomForest" %in% class(mod))
		{
			predictions = predict(mod, newdata = newdata, predict.all = TRUE)
			
			se = function(x)
			{
				result = sd(x) / sqrt(length(x))
				return(result)
			}
			
			se.fit = as.numeric(apply(X = predictions$individual, MARGIN = 1, FUN = se))
			fit = as.numeric(predictions$aggregate)
			
			predictions = data.frame("fit" = fit, 
									 "lower2" = fit - se.fit,
									 "upper2" = fit + se.fit,
									 "lower1" = fit - (3 * se.fit),
									 "upper1" = fit + (3 * se.fit))
			
		} else
		{
			predictions = data.frame(forecast(mod, h = n.ahead, level = level))
		}
		
		fits = as.numeric(c(fits, predictions[,1]))
		
		upper1 = as.numeric(c(upper1, predictions[,5]))
		lower1 = as.numeric(c(lower1, predictions[,4]))
		upper2 = as.numeric(c(upper2, predictions[,3]))
		lower2 = as.numeric(c(lower2, predictions[,2]))
		
		color = c(as.numeric(color) * 0, rep(1, length(predictions[,1])))
		color = factor(color, levels = c(0, 1))
	} 

	# build DF
	
	DF = data.frame("Observation" = xaxis, 
					"Actuals" = actuals, 
					"Fitted" = fits,
					"Upper1" = upper1,
					"Lower1" = lower1,
					"Upper2" = upper2,
					"Lower2" = lower2,
					"Color" = color)
		
	# subset DF if limits was specified
	
	if(class(limits) != "NULL")
	{
		DF = DF[-c(which(DF[,1] < limits[1]), which(DF[,1] > limits[2])),]
	}
	
	# plot DF
	
	if(class(y.levels) != "NULL")
	{
		p = ggplot(DF, aes(x = Observation, y = Actuals, color = Color)) + 
			scale_color_manual(values = c("black", "red"), drop = TRUE, limits = levels(DF$Color)) +
			geom_point(na.rm = TRUE) + 
			geom_line(aes(x = Observation, y = Fitted), color = "blue", na.rm = TRUE) + 
			geom_ribbon(aes(ymin = Lower1, ymax = Upper1), alpha = .1, color = NA) +
			geom_ribbon(aes(ymin = Lower2, ymax = Upper2), alpha = .125, color = NA) +
			labs(x = xlab, y = ylab) +
			ggtitle(main) + 
			scale_y_discrete(breaks = 1:length(y.levels), limits = c(1, length(y.levels)), labels = y.levels) +
			theme_bw(base_size = basefont) +
			theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

	} else
	{
		p = ggplot(DF, aes(x = Observation, y = Actuals, color = Color)) + 
			scale_color_manual(values = c("black", "red"), drop = TRUE, limits = levels(DF$Color)) +
			geom_point(na.rm = TRUE) + 
			geom_line(aes(x = Observation, y = Fitted), color = "blue", na.rm = TRUE) + 
			geom_ribbon(aes(ymin = Lower1, ymax = Upper1), alpha = .1, color = NA) +
			geom_ribbon(aes(ymin = Lower2, ymax = Upper2), alpha = .125, color = NA) +
			labs(x = xlab, y = ylab) +
			ggtitle(main) + 
			theme_bw(base_size = basefont) +
			theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

	}

	return(p)
}

# ---- plots an acf or pacf plot ---------------------------------------------------------------------------------------------------------------------------------

ggacf = function(x, n = NULL, partial = FALSE, conf.level = 0.95, main = "ACF Plot", xlab = "Lag", ylab = "Autocorrelation", basefont = 20) 
{
	require(ggplot2)
	
	if(class(n) == "NULL")
	{
		n = length(x) - 2
	}
	
	ciline = qnorm((1 - conf.level) / 2) / sqrt(length(x))
	
	if(partial == TRUE)
	{
		bacf = pacf(x, lag.max = n, plot = FALSE)
	} else
	{
		bacf = acf(x, lag.max = n, plot = FALSE)
	}
	
	bacfdf = with(bacf, data.frame(lag, acf))
	
	if(partial == FALSE)
	{
		bacfdf = bacfdf[-1,]
	}
	
	acfplot = ggplot(bacfdf, aes(x = lag, y = acf)) + 
			  geom_bar(stat = "identity", position = "dodge", fill = "black") +
			  geom_hline(yintercept = -ciline, color = "blue", size = 1) +
			  geom_hline(yintercept = ciline, color = "blue", size = 1) +
			  geom_hline(yintercept = 0, color = "red", size = 1) +
			  labs(x = xlab, y = ylab) +
			  ggtitle(main) +
			  theme_light(base_size = basefont) +
			  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

	return(acfplot)
}

# ---- builds a summary of a regsubsets object --------------------------------------

regsum = function(x, basefont = 20)
{
	dat = summary(x)

	dat = data.frame("Model" = 1:nrow(dat$which), 
						 dat$which, 
						 "RSS" = dat$rss, 
						 "AdjR2" = dat$adjr2, 
						 "Cp" = dat$cp, 
						 "BIC" = dat$bic)

	for(i in 2:(ncol(dat) - 4))
	{
		dat[,i] = factor(dat[,i], levels = c("TRUE", "FALSE"))
	}
	
	dat$Model = factor(dat$Model, levels = 1:nrow(dat))
	
	require(ggplot2)

	plots = list("AdjR2" = ggplot(dat, aes(x = Model, y = AdjR2, color = AdjR2, fill = AdjR2)) +
						geom_bar(stat = "identity", position = "identity") +
						scale_color_gradientn(colors = heat.colors(nrow(dat))) +
						scale_fill_gradientn(colors = heat.colors(nrow(dat))) +
		   				ggtitle(expression("R"["Adj"]^2)) +
						labs(y = "Value") + 
		   				theme_dark(base_size = basefont) +
						theme(legend.position = "none", plot.title = element_text(hjust = 0.5)),
				
				"Cp" = ggplot(dat, aes(x = Model, y = Cp, color = Cp, fill = Cp)) +
						geom_bar(stat = "identity", position = "identity") +
						scale_color_gradientn(colors = heat.colors(nrow(dat))) +
						scale_fill_gradientn(colors = heat.colors(nrow(dat))) +
		   				ggtitle(expression("Mallows' C"["p"])) +  
						labs(y = "Value") +
		   				theme_dark(base_size = basefont) +
						theme(legend.position = "none", plot.title = element_text(hjust = 0.5)),
				
				"BIC" = ggplot(dat, aes(x = Model, y = BIC, color = BIC, fill = BIC)) +
						geom_bar(stat = "identity", position = "identity") +
						scale_color_gradientn(colors = heat.colors(nrow(dat))) +
						scale_fill_gradientn(colors = heat.colors(nrow(dat))) +
		   				ggtitle("BIC") +
						labs(y = "Value") +
		   				theme_dark(base_size = basefont) +
						theme(legend.position = "none", plot.title = element_text(hjust = 0.5)),

				"RSS" = ggplot(dat, aes(x = Model, y = RSS, color = RSS, fill = RSS)) +
						geom_bar(stat = "identity", position = "identity") +
						scale_color_gradientn(colors = heat.colors(nrow(dat))) +
						scale_fill_gradientn(colors = heat.colors(nrow(dat))) +
		   				ggtitle("RSS") +  
						labs(y = "Value") +
		   				theme_dark(base_size = basefont) +
						theme(legend.position = "none", plot.title = element_text(hjust = 0.5)))

	return(list("dat" = dat, "plots" = plots))
}

}

# ------------------------------------------------------------------------
# ---- Import Data -------------------------------------------------------
# ------------------------------------------------------------------------

{

# check work directory

getwd()
setwd("F:/Documents/Working/Forecasting Methods/Independent Study/US President Elections")

# import the data

winner = data.table(read.csv("winner.csv"))
states = data.table(read.csv("states.csv"))
turnout = data.table(read.csv("Turnout.csv"))
WID = data.table(read.csv("WID.csv"))

# lets look at the data types

winner
types(winner)

states
types(states)

turnout
types(turnout)

WID
types(WID)

# lets update WID
	# don't be concerned with the NA warning, this just means there are missing data points because of unavailable data

WID[, AvgGDP := as.numeric(as.character(AvgGDP))]
WID[, AvgNatIncome := as.numeric(as.character(AvgNatIncome))]
WID[, AvgNatWealth := as.numeric(as.character(AvgNatWealth))]
WID[, WealthIncomeRatio := as.numeric(as.character(WealthIncomeRatio))]

}

# ------------------------------------------------------------------------
# ---- Plotting Data -----------------------------------------------------
# ------------------------------------------------------------------------

{

plots = list()
economy = list()
voting = list()

# ---- WID metrics based on which party owned the presidency -------------

# join winner onto WID to determine which party owned the presidency each year

setkey(WID, Year)
setkey(winner, Year)

dat = data.table(winner[WID])
dat = data.table(dat[Year >= 1912])
dat[, Party := factor(ifelse(na.locf(DemocratWon) == 1, "Democratic", "Republican"), 
						levels = c("Democratic", "Republican"))]
dat[, DemocratWon := NULL]
dat[, Party := shift(Party)]
dat = data.table(dat[-1,])

# lets plot WealthIncomeRatio

WIR = ggplot(dat, aes(x = Year, y = WealthIncomeRatio, color = Party, group = FALSE)) + 
		geom_line(na.rm = TRUE) +
		geom_point(na.rm = TRUE) +
		scale_color_manual(values = c("blue", "red")) +
		# scale_x_continuous(limits = c(1919, 1949)) +
		# scale_x_continuous(limits = c(1980, 2015)) + 
		ggtitle("USA Average National Wealth to Income Ratio") + 
		labs(x = "Year", y = "Ratio", color = "President") + 
		theme_bw(base_size = 25) +
		theme(legend.position = "top", legend.key.size = unit(.25, "in"), plot.title = element_text(hjust = 0.5)) +
		guides(color = guide_legend(override.aes = list(size = 10, linetype = 0)))

WIR
economy$WIR = WIR
rm(WIR)

# lets plot AvgNatWealth

NW = ggplot(dat, aes(x = Year, y = AvgNatWealth, color = Party, group = FALSE)) + 
		geom_line(na.rm = TRUE) +
		geom_point(na.rm = TRUE) +
		scale_color_manual(values = c("blue", "red")) +
		# scale_x_continuous(limits = c(1919, 1949)) +
		# scale_x_continuous(limits = c(1980, 2015)) + 
		ggtitle("USA Average National Wealth") + 
		labs(x = "Year", y = "USD($)", color = "President") + 
		theme_bw(base_size = 25) +
		theme(legend.position = "top", legend.key.size = unit(.25, "in"), plot.title = element_text(hjust = 0.5)) +
		guides(color = guide_legend(override.aes = list(size = 10, linetype = 0)))

NW
economy$NW = NW
rm(NW)

# lets plot AvgNatIncome

NI = ggplot(dat, aes(x = Year, y = AvgNatIncome, color = Party, group = FALSE)) + 
		geom_line(na.rm = TRUE) +
		geom_point(na.rm = TRUE) +
		scale_color_manual(values = c("blue", "red")) +
		# scale_x_continuous(limits = c(1919, 1949)) +
		# scale_x_continuous(limits = c(1980, 2015)) + 
		ggtitle("USA Average National Income") + 
		labs(x = "Year", y = "USD($)", color = "President") + 
		theme_bw(base_size = 25) +
		theme(legend.position = "top", legend.key.size = unit(.25, "in"), plot.title = element_text(hjust = 0.5)) +
		guides(color = guide_legend(override.aes = list(size = 10, linetype = 0)))

NI
economy$NI = NI
rm(NI)

# lets plot AvgGDP

GDP = ggplot(dat, aes(x = Year, y = AvgGDP, color = Party, group = FALSE)) + 
		geom_line(na.rm = TRUE) +
		geom_point(na.rm = TRUE) +
		scale_color_manual(values = c("blue", "red")) +
		# scale_x_continuous(limits = c(1919, 1949)) +
		# scale_x_continuous(limits = c(1980, 2015)) + 
		ggtitle("USA Average Gross Domestic Product Per Capita\nIndividuals Over Age 20") + 
		labs(x = "Year", y = "USD($)", color = "President") + 
		theme_bw(base_size = 25) +
		theme(legend.position = "top", legend.key.size = unit(.25, "in"), plot.title = element_text(hjust = 0.5)) +
		guides(color = guide_legend(override.aes = list(size = 10, linetype = 0)))

GDP
economy$GDP = GDP
rm(GDP, dat)

plots$economy = economy
rm(economy)

# ---- Election Results v. National Turnout -----------

# lets join turnout onto winner and plot the relationship

setkey(winner, Year)
setkey(turnout, Year)

dat = data.table(turnout[winner])
dat[, Winner := factor(ifelse(DemocratWon == 1, "Democratic", "Republican"), levels = c("Democratic", "Republican"))]
dat[, DemocratWon := NULL]

# lets do a boxplot of national turnout v. election winner

turnout.box = ggplot(data = dat, aes(x = Winner, y = NatTurnout, fill = Winner, color = Winner)) +
				geom_boxplot(notch = FALSE, na.rm = TRUE, alpha = 0.25) +
				geom_jitter(alpha = 0.5, width = 0.33, na.rm = TRUE) +
				scale_fill_manual(values = c("cornflowerblue", "indianred")) +
				scale_color_manual(values = c("blue", "red")) +
				ggtitle("USA Presidential Election National Turnout") + 
				labs(x = "Winner", y = "Percent") + 
				theme_bw(base_size = 25) +
				theme(legend.position = "top", legend.key.size = unit(.25, "in"), plot.title = element_text(hjust = 0.5)) +
				guides(color = guide_legend(override.aes = list(size = 1, alpha = 1))) +
				coord_flip()

turnout.box
voting$turnout.box = turnout.box
rm(turnout.box)

# lets do a time series of National turnout colored by election winner

turnout.ts = ggplot(data = dat, aes(x = Year, y = NatTurnout, color = Winner, group = FALSE)) +
				geom_line(na.rm = TRUE) +
				geom_point(na.rm = TRUE, size = 2) +
				scale_color_manual(values = c("blue", "red")) +
				# scale_x_continuous(limits = c(1919, 1949)) +
				# scale_x_continuous(limits = c(1980, 2015)) + 
				ggtitle("USA Presidential Election National Turnout") + 
				labs(x = "Year", y = "Percent") + 
				theme_bw(base_size = 25) +
				theme(legend.position = "top", legend.key.size = unit(.25, "in"), plot.title = element_text(hjust = 0.5)) +
				guides(color = guide_legend(override.aes = list(size = 10, linetype = 0)))

turnout.ts
voting$turnout.ts = turnout.ts
rm(turnout.ts, dat)

# ---- Election Results v. State Results -----------

# lets compute what party each state voted for each year

states[, "Active" := as.numeric(TotalVotes > 0)]
states[, "VoteDemocrat" := as.numeric(DemocratEV > RepublicanEV)]
states[, "VoteTie" := as.numeric(DemocratEV == RepublicanEV)]

# lets join winner onto states

setkey(states, Year)
setkey(winner, Year)

states = data.table(winner[states])

# lets compute if a state vote was a winning vote

states[, WinningVote := DemocratWon == VoteDemocrat]

# lets take a subset of states such that we only have active and non-tie states

dat = data.table(states[Active == 1 & VoteTie == 0])

# lets count how many times each state did or didn't have a winning vote

dat = data.table(dat[, .("WinningVote" = as.factor(names(summary(WinningVote))[2:3]), 
							"Counts" = as.numeric(summary(WinningVote)[2:3]),
							"MaxCount" = max(as.numeric(summary(WinningVote)[2:3]))), 
							by = .(State)])

# lets sort dat by MaxCount
# this will allow us to see the order in which states are consistently TRUE or FALSE with regards to WinningVote

dat = data.table(dat[order(-MaxCount, rank(State))])

# lets reorder the levels of the factor States, so that way we can plot States on the x-axis in an order with respect to MaxCount

dat[, State := factor(State, levels = unique(as.character(dat$State)))]

# lets plot the top 10 WinningVotes by State

DT = data.table(dat[1:20,])

states.topten = ggplot(data = DT, aes(x = State, y = Counts, fill = WinningVote)) +
				geom_bar(stat = "identity", position = "dodge") +
				ggtitle("Top Ten Consistent States for Presidential Elections") + 
				labs(y = "Frequency", fill = "Voted for the Winning Party") + 
				theme_bw(base_size = 25) +
				theme(legend.position = "top", legend.key.size = unit(.25, "in"), plot.title = element_text(hjust = 0.5)) 

states.topten
voting$states.topten = states.topten
rm(states.topten, DT)

# lets redo this to see how a state's vote from the previous year, aligns with the next election winner

# shift the columns Active, VoteDemocrat, and VoteTie up one election cycle for each state

states[, lagActive := c(NA, Active[-.N]), by = State]
states[, lagVoteDemocrat := c(NA, VoteDemocrat[-.N]), by = State]
states[, lagVoteTie := c(NA, VoteTie[-.N]), by = State]

# compute lagWinningVote by state and then order by most consistent states

states[, lagWinningVote := DemocratWon == lagVoteDemocrat]

dat = data.table(states[lagActive == 1 & lagVoteTie == 0])
dat = na.omit(dat)

dat = data.table(dat[, .("lagWinningVote" = as.factor(names(summary(lagWinningVote))[2:3]), 
							"lagCounts" = as.numeric(summary(lagWinningVote)[2:3]),
							"lagMaxCount" = max(as.numeric(summary(lagWinningVote)[2:3]))), 
							by = .(State)])

dat = data.table(dat[order(-lagMaxCount, rank(State))])

dat[, State := factor(State, levels = unique(as.character(dat$State)))]

# lets plot the top 10 lagWinningVote by State

DT = data.table(dat[1:20,])

states.topten.lag = ggplot(data = DT, aes(x = State, y = lagCounts, fill = lagWinningVote)) +
					geom_bar(stat = "identity", position = "dodge") +
					ggtitle("Top Ten Consistent States for Presidential Elections") + 
					labs(y = "Frequency", fill = "Voted for the Winning Party in the Previous Election") + 
					theme_bw(base_size = 25) +
					theme(legend.position = "top", legend.key.size = unit(.25, "in"), axis.text.x = element_text(angle = 15, hjust = 2/3), plot.title = element_text(hjust = 0.5)) 

states.topten.lag
voting$states.topten.lag = states.topten.lag
rm(states.topten.lag, DT)

plots$voting = voting
rm(voting)

}

# ------------------------------------------------------------------------
# ---- Choosing Regressors -----------------------------------------------
# ------------------------------------------------------------------------

{

# given that Nevada and Ohio have voted for the winning party 25/27 times since 1912, we will use them as regressors

dat = data.frame(states)
dat = dat[,c(1:3, 13)]
dat$State = as.character(sub(" ", "", dat$State))

dat = data.table(dat)
dat = data.table(dat[State %in% c("Nevada", "Ohio")])
dat = dcast(dat, formula = Year + DemocratWon ~ State, value.var = "VoteDemocrat")
setnames(dat, c("Year", "DemocratWon", "Nevada.VoteDemocrat", "Ohio.VoteDemocrat"))

# lets add national turnout as a regressor

setkey(dat, Year)
setkey(turnout, Year)

dat = data.table(turnout[dat])

# lets add WealthIncomeRatio as a regressor

WIR = data.table(WID[, .(Year)])
WIR[, WIR := WID$WealthIncomeRatio]

# there are three missing data points so let predict three data points with an arima model

# lets look at differenced values of WIR to find any good values of d

x = WIR$WIR[-(107:108)]

par(mfrow = c(2,3))
plot(x, main = "d = 0", type = "b")
lapply(1:5, function(i) plot(diff(x, differences = i), main = paste0("d = ", i), type ="b"))

# d = 2 or d = 3 seems good
# lets look at the acf and pacf plots of the differenced values of WIR to find any good values of p and q

d = 2
grid.arrange(ggacf(x = diff(x, differences = d), partial = TRUE, basefont = 15, main = paste0("PACF Plot of WIR\nDifference of Order ", d)),
			 ggacf(x = diff(x, differences = d), basefont = 15, main = paste0("ACF Plot of WIR\nDifference of Order ", d)),
			 ncol = 2)

# for d = 2:
	# p = 4
	# q = unsure

# for d = 3:
	# p = 6
	# q = 1

# lets build a (6, 3, 1) arima model and compare it with the automated one
	
mod = Arima(ts(x), order = c(6, 3, 1))
mod.auto = auto.arima(ts(x))

# plot the fitted values v. actual values and compare

grid.arrange(modplot(mod, main = "mine"), modplot(mod, main = "auto"), ncol = 2)

# compute performance metrics and compare

accuracy(mod)
accuracy(mod.auto)

# plot the acf of the residuals and compare

grid.arrange(ggacf(x = as.numeric(resid(mod)), main = "mine"), ggacf(x = as.numeric(resid(mod)), main = "auto"), ncol = 2)

# plot the pacf of the residuals and compare

grid.arrange(ggacf(x = as.numeric(resid(mod)), main = "mine", partial = TRUE), ggacf(x = as.numeric(resid(mod)), main = "auto", partial = TRUE), ncol = 2)

# they both have very similar fitted values
# they both have very similar performance metrics
# they both have removed nearly all autocorrelation
# but the automated one has just 1 coefficient
	# this means the three predictions will be identical because it requires just 1 previous data point
# i think using the one i created is better to allow for a different three predictions, which is more realistic

rm(mod.auto, x, d)

new = predict(mod, n.ahead = 3)
new = as.numeric(new$pred)

# lets add these predictions to WIR and then build lag variables to show the last 4 WIR values at each year

WIR = rbind(WIR, data.frame("Year" = 2016, "WIR" = NA))
WIR[, WIR := c(WIR[-(107:109)], new)]
WIR[, WIR_1 := shift(WIR, 1)]
WIR[, WIR_2 := shift(WIR, 2)]
WIR[, WIR_3 := shift(WIR, 3)]
WIR = WIR[5:109]

# let add WIR to dat

setkey(dat, Year)
setkey(WIR, Year)

dat = data.table(WIR[dat])
rm(WIR, new, mod)

# let use the leaps package to determine what combination of regressors is best
	# the leaps package is used for quickly testing various linear regression models

# lets look at main effect models

mods = regsubsets(DemocratWon ~ .,
					data = dat,
					nbest = 1,      
					nvmax = NULL,
					method = "exhaustive",
					really.big = TRUE)

mods.sum = regsum(mods)

grid.arrange(mods.sum$plots[[1]], 
			 mods.sum$plots[[2]], 
			 mods.sum$plots[[3]], 
			 mods.sum$plots[[4]],  
			 ncol = 2)

# here's the best model

mods.sum$dat[3,]
regressors = c("WIR_1", "Nevada.VoteDemocrat", "Ohio.VoteDemocrat")

}

# ------------------------------------------------------------------------
# ---- Export Data -------------------------------------------------------
# ------------------------------------------------------------------------

DT = data.table(dat[,.(Year, DemocratWon, WIR_1, Nevada.VoteDemocrat, Ohio.VoteDemocrat)])

write.csv(DT, "potus.csv", row.names = FALSE)





