complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  # initialize a data frame hold results data
  df <- data.frame(id = numeric(), nobs = numeric(), stringsAsFactors = FALSE)
  # find all files in the specdata folder
  all_files <- as.character( list.files(directory) )
  file_paths <- paste(directory, all_files, sep="")
  for(i in id) {
    currentFile <- read.csv(file_paths[i], header=T, sep=",")
    head(currentFile)
    nob <- nrow(currentFile[rowSums(is.na(currentFile))==0,])
    df <- rbind(df, data.frame(id = i, nobs = nob))
  }
  return(df)
  
}
# tests
complete("specdata", 1) == data.frame(id=c(1), nobs=c(117))
complete("specdata", c(2, 4, 8, 10, 12)) == data.frame(id=c(2, 4, 8, 10, 12), nobs=c(1041, 474, 192, 148, 96))
