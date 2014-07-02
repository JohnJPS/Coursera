corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    corrList = NULL

    csvFilePathList = list.files(directory,"*.csv",full.names=TRUE)
    
    for (csvFilePath in csvFilePathList) {
        
        iData = read.csv(csvFilePath)
        ccData = iData[complete.cases(iData),]
        if (nrow(ccData) > threshold) {
            currCor = cor(ccData[,2],ccData[,3],use="pairwise")
            corrList = c(corrList,currCor)
        }
    }
    corrList
}

