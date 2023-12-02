# Find confidence interval of the mean for summarized data
ci = function(sampleMean, sampleVar, n, confidence, popVar=FALSE) { 
    # Notice that it doesn't matter much to distinguish between sample variance and popN variance since such change doesn't affect the formula.
    # What does matter is whether our sample size is large enough (n>30) or not (n<30).
    halfAlpha = 1 - ((.5) * (1-confidence)) # right tail AKA positive alpha
    stderr = sqrt(sampleVar / n)
    largeN = n >= 30
    if ( largeN == TRUE || popVar ) {
    amountOfStdErr = qnorm(p = halfAlpha)
    } else {
        amountOfStdErr = qt(p = halfAlpha, df = n-1)
    }
    confidenceInterval = c(sampleMean - (amountOfStdErr * stderr), sampleMean + (amountOfStdErr * stderr))
    return(confidenceInterval)
}

# Find confidence interval of the mean from raw data
nci = function(data, confidence) { # data is a numeric vector
    halfAlpha = 1 - ((.5) * (1-confidence))
    sampleMean = mean(data)
    sampleVar = var(data)
    n = length(data)
    stderr = sqrt(sampleVar / n)
    largeN = n >= 30
    if ( largeN == TRUE ) {
    amountOfStdErr = qnorm(p = halfAlpha)
    } else {
        amountOfStdErr = qt(p = halfAlpha, df = n-1)
    }
    confidenceInterval = c(sampleMean - (amountOfStdErr * stderr), sampleMean + (amountOfStdErr * stderr))
    return(confidenceInterval)
}


# Find maximum error of a mean estimate for some confidence interval
maxError = function(sampleVar, confidence, n, popVar=FALSE) {
    halfAlpha = 1 - ((.5) * (1-confidence))
    stderr = sqrt(sampleVar / n)
    largeN = n >= 30
    if ( largeN == TRUE || popVar ) {
    amountOfStdErr = qnorm(p = halfAlpha)
    } else {
        amountOfStdErr = qt(p = halfAlpha, df = n-1)
    }
    error = amountOfStdErr * stderr
    return(error)
}

# Find the sample size required for some confidence interval based on summarized data
# Assumption: Sample is taken from a normally distributed popN
confSampleSize = function(maxError, sampleVar, confidence) {
    halfAlpha = 1 - ((.5) * (1-confidence))
    amountOfStdErr = qnorm(p = halfAlpha)
    n = (amountOfStdErr / maxError)^2 * sampleVar
    n = ceiling(n)
    return(n)
}

# Find confidence interval for the difference of two means
diffci = function(sampleMean1, sampleMean2, sampleVar1, sampleVar2, n1, n2, confidence, popVar=FALSE) {
	halfAlpha = 1 - ((.5) * (1-confidence))
    largeN = n1 >= 30 && n2 >= 30
	if ( largeN == TRUE || popVar ) {
    amountOfStdErr = qnorm(p = halfAlpha)
    } else {
        amountOfStdErr = qt(p = halfAlpha, df = n1+n2-2)
		pooledVar = ( (n1-1)*sampleVar1 + (n2-1)*sampleVar2 ) / (n1 + n2 - 2) 
		sampleVar1 = pooledVar
		sampleVar2 = pooledVar
	}
	aggrSampleMean = abs(sampleMean1 - sampleMean2)
	aggrSampleVar = (sampleVar1/n1) + (sampleVar2/n2)
    stderr = sqrt(aggrSampleVar)
    confidenceInterval = c(aggrSampleMean - (amountOfStdErr * stderr), aggrSampleMean + (amountOfStdErr * stderr))
    return(confidenceInterval)
}

# Find confidence interval for binomial proportion p
# Assumption: n is large enough (np > 5 & nq > 5) to justify a normal approximation
proci = function(noSuccess, totalNumber, confidence) {
	if (totalNumber < 6 ) return -1
	halfAlpha = 1 - ((.5) * (1-confidence))
	pHat = noSuccess / totalNumber # sample mean
	stderr = sqrt( (pHat * (1-pHat)) / (totalNumber) )
	amountOfStdErr = qnorm(p = halfAlpha)
    confidenceInterval = c(pHat - (amountOfStdErr * stderr), pHat + (amountOfStdErr * stderr))
	return(confidenceInterval)
	 }
