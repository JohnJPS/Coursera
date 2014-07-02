pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    ################################################################################
    ## functional specifications
    ################################################################################
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    ##
    ## 'pollutant' is a character vector of length 1 indicating 
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    ##
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    ##
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ################################################################################
  
    meanList <- NULL
    countList <- NULL
    for (i in id) {
        iPath = paste("C:/myCoursera/",directory,"/",formatC(i,width=3,format="d",flag="0"),".csv",sep="")
        iData = read.csv(iPath)
        currMean = mean(iData[[pollutant]],na.rm=TRUE,dims=1)
        if (!is.nan(currMean)) {
            meanList = c(meanList, currMean)
            countList = c(countList, sum(!is.na(iData[[pollutant]])))
        }
    }
    mean(meanList * countList) * length(countList) / sum(countList)
}
