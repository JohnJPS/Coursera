rankall1 <- function(outcome, num = "best") {
    ## Read outcome data
    df <- read.csv("./outcome-of-care-measures.csv", colClasses = "character")
    
    ## validate input
    num = tolower(num)
    ocCaps = gsub(" ","_",toupper(outcome))
    if (ocCaps != 'PNEUMONIA' && ocCaps != 'HEART_ATTACK' && ocCaps != 'HEART_FAILURE') {
        stop("invalid outcome")
    } else {
        mortRate = paste("Hospital_30_Day_Death__Mortality__Rates_from_",ocCaps,sep="")
    }
    
    sort = "asc"
    if (num == "best") {
        num = 1
    } else if (num == "worst") {
        num = 1
        sort = "desc"
    }
    
    hospList = data.frame(hospital=character(),state=character(),stringsAsFactors=FALSE)
    stateList = sqldf("select distinct State from df order by State")
    for (i in 1:length(stateList$State)) {
        hospCmd = paste("select Hospital_Name from df ",
                        "where State = '",stateList$State[i],"' ",
                        "and CAST(",mortRate," AS DOUBLE) > 0 ",
                        "ORDER BY CAST(",mortRate," AS DOUBLE) ",sort,
                        ", Hospital_Name ",sep = "")
        hospsForState = sqldf(hospCmd)
        hospList[i,] <- list(hospsForState$Hospital_Name[as.numeric(num)],stateList$State[i])
    }
    hospList
}
