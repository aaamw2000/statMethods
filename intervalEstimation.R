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
