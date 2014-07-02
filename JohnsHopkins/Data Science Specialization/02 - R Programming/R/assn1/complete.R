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
    
    idList = NULL
    ccList = NULL
    for (i in id) {
        iPath = paste("C:/myCoursera/",directory,"\\",formatC(i,width=3,format="d",flag="0"),".csv",sep="")
        iData = read.csv(iPath)
        ccData = iData[complete.cases(iData),]
        idList = c(idList,i)
        ccList = c(ccList, nrow(ccData))
    }
    rsp = as.data.frame(cbind(id=idList,nobs=ccList))
    rsp
}
