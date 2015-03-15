corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  source("complete.R")
  
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  completeCases <- complete(directory)
  casesAboveThreshold <- completeCases[completeCases$nobs > threshold,1]
  # find all files in the specdata folder
  allFiles <- list.files(path = directory, full.names = TRUE)
  
  # initialize a vector correlation
  #correlations <- c()
  correlations <- rep(NA,length(casesAboveThreshold))
  for(i in casesAboveThreshold ){
    currentFile <- read.csv(allFiles[i], header=T, sep=",")
    completeCases <- complete.cases(currentFile)
    correlations[i] <- cor(x = currentFile[completeCases,2], y = currentFile[completeCases,3])
  }
  correlations <- correlations[complete.cases(correlations)]
  
  return (correlations)
}