q1 <- function() {
    df <- read.csv("C:/Users/WIN 8/OneDrive/myCoursera/JohnsHopkins/Data Science Specialization/Data/getdata_data_ss06hid.csv")
    strsplit(colnames(df),"wgtp")[[123]]
}

q2 <- function() {
    gdpData <- read.csv("C:/Users/WIN 8/OneDrive/myCoursera/JohnsHopkins/Data Science Specialization/Data/getdata_data_GDP-CLEAN.csv")
    gdpDataFiltered = gdpData[!is.na(gdpData$Rank),]
    gdpNums <- gsub(",","",gdpDataFiltered$GDP)
    mean(as.numeric(gdpNums),na.rm=TRUE)
}

q3 <- function() {
    list(grep("*United",gdpData$Long.Name), ## 5
         grep("United$",gdpData$Long.Name), ## 3
         grep("^United",gdpData$Long.Name), ## 4
         grep("^United",gdpData$Long.Name)) ## 3 
}

q4 <- function () {
    gdpData <- read.csv("C:/Users/WIN 8/OneDrive/myCoursera/JohnsHopkins/Data Science Specialization/Data/getdata_data_GDP-CLEAN.csv")
    eduData <- read.csv("C:/Users/WIN 8/OneDrive/myCoursera/JohnsHopkins/Data Science Specialization/Data/getdata_data_EDSTATS_Country.csv")
    gdpDataFiltered = gdpData[!is.na(gdpData$Rank) && !is.na(gdpData$GDP),]
    mergedData = merge(gdpDataFiltered,eduData,by.x="CountryCode",by.y="CountryCode",all=TRUE)
    mergedData$Special.Notes[grep("*Fiscal.*June",mergedData$Special.Notes)]
}

q5 <- function () {
    ##library(quantmod)
    amzn = getSymbols("AMZN",auto.assign=FALSE)
    sampleTimes = index(amzn)
    y <- length(sampleTimes[format(sampleTimes,"%Y")==2012])
    ym <- length(sampleTimes[format(sampleTimes,"%Y %A")=="2012 Monday"])
    list(y,ym)
}