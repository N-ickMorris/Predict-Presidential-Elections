cores = 5

# -----------------------------------------------------------------------------------
# ---- Packages ---------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

require(data.table)
require(doParallel)
require(foreach)
require(forecast)
require(ggplot2)
require(gridExtra)
require(randomForest)
require(e1071)
require(RSNNS)

}

# -----------------------------------------------------------------------------------
# ---- Functions --------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# create a sequence of numbers on a log scale

log.seq = function(from, to, length.out)
{
	return(exp(seq(log(from), log(to), length.out = length.out)))
}

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
	
	if(class(actuals) == "NULL" & "svm" %in% class(mod) & !("residuals" %in% names(mod)))
	{
		p = "Input a vector for 'actuals' to plot an SVM Classification model"
		
	} else
	{
		
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

		} else
		{
			actuals = as.numeric(actuals)
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
		
		if(class(mod)[1] == "lm")
		{
			upper1 = as.numeric(predict(mod, interval = "confidence", level = 0.95)[,3])
			upper2 = as.numeric(predict(mod, interval = "confidence", level = 0.80)[,3])
			lower1 = as.numeric(predict(mod, interval = "confidence", level = 0.95)[,2])
			lower2 = as.numeric(predict(mod, interval = "confidence", level = 0.80)[,2])
			
		} else
		{
			upper1 = as.numeric(rep(NA, length(fits)))
			upper2 = upper1
			lower1 = upper1
			lower2 = upper1
		}
		
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
				
			} else if("svm" %in% class(mod))
			{
				predictions = data.frame("fit" = predict(mod, newdata = newdata, n.ahead = n.ahead), 
										 "lower2" = rep(NA, n.ahead),
										 "upper2" = rep(NA, n.ahead),
										 "lower1" = rep(NA, n.ahead),
										 "upper1" = rep(NA, n.ahead))
				
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
			if("glm" %in% class(mod))
			{
				Offset = 1
			} else
			{
				Offset = 0
			}
			
			p = ggplot(DF, aes(x = Observation, y = Actuals, color = Color)) + 
				scale_color_manual(values = c("black", "red"), drop = TRUE, limits = levels(DF$Color)) +
				geom_point(na.rm = TRUE) + 
				geom_line(aes(x = Observation, y = Fitted), color = "blue", na.rm = TRUE) + 
				geom_ribbon(aes(ymin = Lower1, ymax = Upper1), alpha = .1, color = NA) +
				geom_ribbon(aes(ymin = Lower2, ymax = Upper2), alpha = .125, color = NA) +
				scale_y_continuous(breaks = (1:length(y.levels) - Offset), labels = y.levels, limits = (c(0.5, length(y.levels) + 0.5) - Offset)) +
				labs(x = xlab, y = ylab) +
				ggtitle(main) + 
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
	}
	
	return(p)
}

# create a confusion matrix for a glm model

confusion.glm = function(model, name = NULL, des.mat = NULL, response = NULL, cutoff = 0.5)
{
	if(missing(des.mat))
	{
		prediction = predict(model, type = "response") > cutoff
		confusion = table(as.logical(model$y), prediction)
		
	} else 
	{
		if(missing(response) || class(response) != "logical")
		{
			stop("Must give logical vector as response when des.mat given")
		}

		prediction = predict(model, des.mat, type = "response") > cutoff
		confusion  = table(response, prediction)
	}

	confusion = cbind(confusion,
						c(1 - confusion[1,1] / rowSums(confusion)[1],
						 1 - confusion[2,2] / rowSums(confusion)[2]))
	
	if(missing(name))
	{
		colnames(confusion) = c("FALSE", "TRUE", "class.error")
		
	} else
	{
		colnames(confusion) = c(name, "class.error")
		rownames(confusion) = name
	}
	
	return(confusion)
}

}

# -----------------------------------------------------------------------------------
# ---- Import Data ------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# check work directory

getwd()
setwd("F:/Documents/Working/Forecasting Methods/Independent Study/US President Elections")

# import the data

dat = data.table(read.csv("potus.csv"))

# lets ensure our response variable is a factor data type
	
dat[, Winner := factor(ifelse(DemocratWon == 1, "Democratic", "Republican"), levels = c("Democratic", "Republican"))]

}

# -----------------------------------------------------------------------------------
# ---- Logistic Regression Model ----------------------------------------------------
# -----------------------------------------------------------------------------------

{

# ---- fitted model ----

LR_fit = glm(formula = Winner ~ WIR_1 + Nevada.VoteDemocrat + Ohio.VoteDemocrat, 
			family = binomial(link = "logit"),
			control = list(maxit = 50),
			data = dat)

LR.confusion = confusion.glm(LR_fit, name = levels(dat$Winner))

# extract percent of correct predictions

stats = data.frame(sum(diag(LR.confusion)) / sum(LR.confusion[,-ncol(LR.confusion)]))

# extract error percent for each level of the repsonse

stats = cbind(stats, data.frame(matrix(LR.confusion[,ncol(LR.confusion)], ncol = nrow(LR.confusion))))

# give proper column names

name = as.vector(c("accuracy", sapply(levels(dat$Winner), paste0, ".error")))
colnames(stats) = name

# plot the model

plot_LR_fit = modplot(mod = LR_fit,
						xaxis = dat$Year,
						y.levels = levels(dat$Winner),
						xlab = "Year", 
						ylab = "Winner", 
						main = "Presidential Election Winner\nLogistic Regression: Fitted Model")

plot_LR_fit

# save the stats of the model

LR_fit_table = stats
rownames(LR_fit_table) = "LR"

rm(stats, LR.confusion)

}

# -----------------------------------------------------------------------------------
# ---- Random Forest Model ----------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# ---- fitted model ----

# randomForest input parameters of interest:
	# ntree = 500 ~ number of decision trees to create
	# mtry = floor(sqrt(p)) (where p is number of regressors) ~ the number of regressors to randomly choose, from which, one is randomly chosen to then determine how to recursively split a parent node into two child nodes
	# nodesize = 1 ~ minimum size of terminal nodes (ie. the minimum number of data points that can be grouped together in any node of a tree)

# lets create a DOE to test a range of values for each of the above randomForest parameters

DOE = expand.grid(replication = 1:10,
				  ntree = seq(from = 500, to = 1000, by = 250),
				  mtry = seq(from = 1, to = 3, by = 1),
				  nodesize = seq(from = 1, to = 5, by = 2))

DOE$run = unlist(lapply(1:(nrow(DOE) / max(DOE$replication)), function(i) rep(i, max(DOE$replication))))

head(DOE)
tail(DOE)
nrow(DOE)

# lets compute the performance metrics for each random forest model in the DOE

set.seed(42)
seeds = sample(1:500, max(DOE$run))

rfors = foreach(i = 1:nrow(DOE)) %do%
{
	require(foreach)
	require(randomForest)
	
	# set seed based on replication id so that all runs are comparable
	
	set.seed(seeds[DOE$replication[i]])
	
	# build RF
	
	RF = randomForest(formula = Winner ~ WIR_1 + Nevada.VoteDemocrat + Ohio.VoteDemocrat,
						data = dat,
						ntree = DOE$ntree[i],
						mtry = DOE$mtry[i],
						nodesize = DOE$nodesize[i])
	
	# extract percent of correct predictions
	
	stats = data.frame(sum(diag(RF$confusion)) / sum(RF$confusion[,-ncol(RF$confusion)]))
	
	# extract error percent for each level of the repsonse
	
	stats = cbind(stats, data.frame(matrix(RF$confusion[,ncol(RF$confusion)], ncol = nrow(RF$confusion))))
	
	# give proper column names
	
	name = as.vector(c("accuracy", sapply(levels(dat$Winner), paste0, ".error")))
	colnames(stats) = name
	
	return(stats)
}

# add model stats to the DOE

DT = data.frame(rbindlist(rfors))
DOE = data.table(cbind(DOE, DT))
DOE[, replication := NULL]

rm(DT, rfors, seeds)

# average model stats for each run

DOE = data.frame(DOE[, lapply(.SD, mean), by = run])

# lets filter out the top models

# lets plot the histograms of these reponses to find filter limits

par(mfrow = c(2, 2))
lapply(5:7, function(i) hist(DOE[,i], main = colnames(DOE)[i]))

head(DOE)

# apply filters

x = DOE[which(DOE$accuracy >= 0.88 & 
			  DOE$Democratic.error <= 0.145),]

par(mfrow = c(2, 2))
lapply(5:7, function(i) hist(x[,i], main = colnames(x)[i]))

x

# most of these models perform the same, so lets goo with the simplest one with the smallest parameters
	# model 4

# lets plot the fitted v. actuals for model 4

set.seed(42)

RF_fit = randomForest(formula = Winner ~ WIR_1 + Nevada.VoteDemocrat + Ohio.VoteDemocrat,
						data = dat,
						ntree = 500,
						mtry = 2,
						nodesize = 1)

RF_fit

plot_RF_fit = modplot(mod = RF_fit,
						xaxis = dat$Year,
						y.levels = levels(dat$Winner),
						xlab = "Year", 
						ylab = "Winner", 
						main = "Presidential Election Winner\nRandom Forest: Fitted Model")

plot_RF_fit

# save the stats of the model

RF_fit_table = DOE[4, 5:7]
rownames(RF_fit_table) = "RF"

rm(x, DOE)

}

# -----------------------------------------------------------------------------------
# ---- Support Vector Machine Model -------------------------------------------------
# -----------------------------------------------------------------------------------

{

# we will focus on nu-classification, aka v-classification
# this is becuase the parameter 'nu' takes on values [0, 1] so it makes tuning simplier than C-classification which uses the 'cost' parameter which takes on values >= 0

# we will focus on the radial basis as our kernel function for projecting our data into a higher demensional feature space becuase:
	# this seems to be the popular method
	# this only requires one parameter: 'gamma'
	# this may end up being less computationally intensive becuase we are not actually putting our data into higher demsional space, we are estimating it accurately
	# this is the default method of choice in the e1071 package

# important information regarding our parameters:
	# nu:
		# It is used to determine the proportion of the number of support vectors you desire to keep in your solution with respect to the total number of samples in the dataset.
		# If this takes on a large value, then all of your data points could become support vectors, which is overfitting
	# gamma:
		# If gamma is too large, the radius of the area of influence of the support vectors only includes the support vector itself and no amount of regularization with 'cost' will be able to prevent overfitting.
		# When gamma is very small, the model is too constrained and cannot capture the complexity or “shape” of the data. The region of influence of any selected support vector would include the whole training set.
		# grid.py tool from the libSVM package checks values of gamma from 2^-15 to 2^3 
		
# the default values of our parameters are:
	# nu = 0.5
	# gamma = 1 / (number of regressors)

# ---- fitted model ----

# lets create a DOE to test a range of values for each of the svm parameters of interest

DOE = expand.grid(gamma = seq(from = 2^(-15), to = 2^3, length.out = 20),
				  nu = seq(from = 0.3, to = 0.7, by = 0.1))

DOE = rbind(c(1/3, 0.5), DOE)
DOE = DOE[!duplicated(DOE),]
rownames(DOE) = 1:nrow(DOE)

head(DOE)
tail(DOE)
nrow(DOE)

# lets compute the residuals for each svm model in the DOE

registerDoParallel(cores = cores) 

svms = foreach(i = 1:nrow(DOE)) %dopar%
{
	require(foreach)
	require(e1071)
	require(RSNNS)
	
	# build svm model
	
	mod = svm(formula = Winner ~ WIR_1 + Nevada.VoteDemocrat + Ohio.VoteDemocrat,
				data = dat,
				type = "nu-classification",
				gamma = DOE$gamma[i],
				nu = DOE$nu[i])
	
	# build confusion matrix
	
	svm.confusion = confusionMatrix(targets = dat$Winner, predictions = fitted(mod))
	svm.confusion = matrix(svm.confusion, nrow = nrow(svm.confusion))

	class.error = sapply(1:nrow(svm.confusion), function(i) (sum(svm.confusion[i,]) - svm.confusion[i,i]) / svm.confusion[i,i])
	svm.confusion = cbind(svm.confusion, class.error)

	colnames(svm.confusion) = c(levels(dat$Winner), "class.error")
	rownames(svm.confusion) = levels(dat$Winner)
	
	# extract percent of correct predictions

	stats = data.frame(sum(diag(svm.confusion)) / sum(svm.confusion[,-ncol(svm.confusion)]))

	# extract error percent for each level of the repsonse

	stats = cbind(stats, data.frame(matrix(svm.confusion[,ncol(svm.confusion)], ncol = nrow(svm.confusion))))

	# give proper column names

	name = as.vector(c("accuracy", sapply(levels(dat$Winner), paste0, ".error")))
	colnames(stats) = name

	return(stats)
}

registerDoSEQ()

# add model stats to the DOE

DT = data.frame(rbindlist(svms))
DOE = data.table(cbind(DOE, DT))

rm(DT, svms)

# lets filter out the top models

# lets plot the histograms of these reponses to find filter limits

DOE = data.frame(DOE)

par(mfrow = c(2, 2))
lapply(3:5, function(i) hist(DOE[,i], main = colnames(DOE)[i]))

head(DOE)

# apply filters

x = DOE[which(DOE$accuracy == 1 & 
			  DOE$Democratic.error == 0 &
			  DOE$Republican.error == 0),]

par(mfrow = c(2, 2))
lapply(3:5, function(i) hist(x[,i], main = colnames(x)[i]))

x

# most of these models perform the same, so lets goo with the simplest one with the smallest parameters
	# model 3

# lets plot the fitted v. actuals for model 4

SVM_fit = svm(formula = Winner ~ WIR_1 + Nevada.VoteDemocrat + Ohio.VoteDemocrat,
				data = dat,
				type = "nu-classification",
				gamma = DOE$gamma[3],
				nu = DOE$nu[3])

SVM_fit

plot_SVM_fit = modplot(mod = SVM_fit,
						actuals = dat$Winner,
						xaxis = dat$Year,
						y.levels = levels(dat$Winner),
						xlab = "Year", 
						ylab = "Winner", 
						main = "Presidential Election Winner\nSupport Vector Machine: Fitted Model")

plot_SVM_fit

# save the stats of the model

SVM_fit_table = DOE[3, 3:5]
rownames(SVM_fit_table) = "SVM"

rm(x, DOE)

}

# -----------------------------------------------------------------------------------
# ---- Summary of Models ------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# ---- tabular summaries ----

fit_table = rbind(LR_fit_table, RF_fit_table, SVM_fit_table)

# performance metrics of all fitted models
 
fit_table

# ---- graphic summary ----

plot_LR_fit
plot_RF_fit
plot_SVM_fit

}






