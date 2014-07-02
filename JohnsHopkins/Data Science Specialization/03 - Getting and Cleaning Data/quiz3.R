q1 <- function() {
    df <- read.csv("C:/Users/WIN 8/OneDrive/myCoursera/JohnsHopkins/Data Science Specialization/Data/getdata_data_ss06hid.csv")

    ## create a logical vector that identifies:
    ##    -- the households (TYPE = 1)
    ##    -- on greater than 10 acres (ACR = 3) 
    ##    -- who sold more than $10,000 of agiculture products (AGS = 6)
    
    agricultureLogical <- df[which(df$TYPE==1 & df$ACR==3 & df$AGS==6),]
    head(agricultureLogical)
}

q2 <- function() {
    jp <- readJPEG("C:/Users/WIN 8/OneDrive/myCoursera/JohnsHopkins/Data Science Specialization/Data/getdata_jeff2.jpg",native=TRUE)
    quantile(jp,probs=c(0.3,0.8))    
}

q3 <- function() {
    gdpData <- read.csv("C:/Users/WIN 8/OneDrive/myCoursera/JohnsHopkins/Data Science Specialization/Data/getdata_data_GDP-CLEAN.csv")
    eduData <- read.csv("C:/Users/WIN 8/OneDrive/myCoursera/JohnsHopkins/Data Science Specialization/Data/getdata_data_EDSTATS_Country.csv")
    gdpDataFiltered = gdpData[!is.na(gdpData$Rank),]
    ccListGDP <- sort(gdpDataFiltered$CountryCode)
    ccListEDU <- sort(eduData$CountryCode)
    ccList <- intersect(ccListGDP,ccListEDU)
    message(length(ccList))
    gdpDataSorted <- gdpDataFiltered[order(gdpDataFiltered$Rank,decreasing=TRUE),]
    gdpDataSorted[13,]
}

q4 <- function() {
    gdpData <- read.csv("C:/Users/WIN 8/OneDrive/myCoursera/JohnsHopkins/Data Science Specialization/Data/getdata_data_GDP-CLEAN.csv")
    eduData <- read.csv("C:/Users/WIN 8/OneDrive/myCoursera/JohnsHopkins/Data Science Specialization/Data/getdata_data_EDSTATS_Country.csv")
    gdpDataFiltered = gdpData[!is.na(gdpData$Rank) && !is.na(gdpData$GDP),]
    mergedData = merge(gdpDataFiltered,eduData,by.x="CountryCode",by.y="CountryCode",all=TRUE)
    df <- sqldf("select Income_Group, avg(Rank) from mergedData group by Income_Group")
    df
}

q5 <- function() {
    gdpData <- read.csv("C:/Users/WIN 8/OneDrive/myCoursera/JohnsHopkins/Data Science Specialization/Data/getdata_data_GDP-CLEAN.csv")
    eduData <- read.csv("C:/Users/WIN 8/OneDrive/myCoursera/JohnsHopkins/Data Science Specialization/Data/getdata_data_EDSTATS_Country.csv")
    gdpDataFiltered = gdpData[!is.na(gdpData$Rank) && !is.na(gdpData$GDP),]
    mergedData = merge(gdpDataFiltered,eduData,by.x="CountryCode",by.y="CountryCode",all=TRUE)
    qnt <- quantile(mergedData$Rank,probs=c(0,0.2,0.4,0.6,0.8),na.rm=TRUE)
    mergedData$qntIndex <- findInterval(mergedData$Rank,qnt)
    eduMelt <- melt(mergedData,id=c("Rank","Income.Group"),measure.vars=c("qntIndex"))
    sqldf("select * from eduMelt where value = 1 and Income_Group = 'Lower middle income' order by Rank,Income_Group,value")
}

