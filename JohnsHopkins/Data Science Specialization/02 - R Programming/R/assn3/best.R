best <- function(state, outcome) {
    ## Read outcome data
    df <- read.csv("C:/myCoursera/rprogAssn3/outcome-of-care-measures.csv", colClasses = "character")
        
    ## validate input
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
    
    sqlCmdGetMinRate = paste("select Min(CAST(Hospital_30_Day_Death__Mortality__Rates_from_",vOutcome," AS DOUBLE)) ",
                             "from df where State = '",vState,"' and ",
                             "CAST(Hospital_30_Day_Death__Mortality__Rates_from_",vOutcome," AS DOUBLE) > 0",sep = "")
    minRate = sqldf(sqlCmdGetMinRate)
    if (is.na(minRate)) {
        stop(paste("No valid results for the combination of State=",vState," sand illness=",vOutcome,".",sep=""))
    }
    sqlGetBest = paste("select Hospital_Name from df where State='",vState,
                       "' and CAST(Hospital_30_Day_Death__Mortality__Rates_from_",vOutcome," AS DOUBLE) = ",minRate,
                       " order by Hospital_Name",sep="")
    bestHospital = sqldf(sqlGetBest)
    
    bestHospital$Hospital_Name[1]

}
