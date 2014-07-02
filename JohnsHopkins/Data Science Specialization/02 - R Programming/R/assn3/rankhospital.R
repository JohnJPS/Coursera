rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    df <- read.csv("C:/myCoursera/rprogAssn3/outcome-of-care-measures.csv", colClasses = "character")
    
    ## validate input
    num = tolower(num)
    vState = toupper(state)
    vOutcome = gsub(" ","_",toupper(outcome))
    sqlCmdCheckState   = paste("select count(*) from df where State = '",vState,"'", sep = "")
    stateCount = sqldf(sqlCmdCheckState)
    if (as.numeric(stateCount) < 1) {
        stop("invalid state")        
    }
    if (vOutcome != 'PNEUMONIA' && vOutcome != 'HEART_ATTACK' && vOutcome != 'HEART_FAILURE') {
        stop("invalid outcome")
    }
    
    sqlCmdGetRanks = paste("select Hospital_Name",
                           ", CAST(Hospital_30_Day_Death__Mortality__Rates_from_",vOutcome," AS DOUBLE) ",
                           "from df where State = '",vState,"' and ",
                           "CAST(Hospital_30_Day_Death__Mortality__Rates_from_",vOutcome," AS DOUBLE) > 0 ",
                           "ORDER BY CAST(Hospital_30_Day_Death__Mortality__Rates_from_",vOutcome," AS DOUBLE), HOSPITAL_NAME",sep = "")
    rankings = sqldf(sqlCmdGetRanks)
    if (length(rankings) == 0) {
        stop(paste("No valid results for the combination of State=",vState," and illness=",vOutcome,".",sep=""))
    }

    if (num == "best") {
        num = 1
    } else if (num == "worst") {
        num = length(rankings[,1])
        message(num)
    }
    rankings$Hospital_Name[as.numeric(num)]
}
