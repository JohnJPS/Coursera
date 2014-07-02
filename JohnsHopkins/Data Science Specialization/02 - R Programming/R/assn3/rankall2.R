rankall2 <- function(outcome,num="best")
{
    # Helper to pick the name from num
    getname <- function(name,num)
    {
        # Default only works with strings
        switch(num,  "best" =return(name[1]),								
                      "worst"= return(tail(name,1)),
                       stop("Invalid num string"))
        if (num > length(name)) return(NA) else return(name[num])   
   }
   
   # Read in the frame
   df <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
   
   # Map outcome to columns
   validoutcomes <- list("heart attack"=11,"heart failure"=17,"pneumonia"=23)

   # Not a valid outcome
   if (is.null(ocColumn <- validoutcomes[[outcome]])) {  stop("invalid outcome") }

   # Slice the columns and name them
   hospitals <- data.frame(Name=df[,2], State=df[,7], Outcome=df[,ocColumn], stringsAsFactors=FALSE)
   
   # Order the data and omit NAs
   hospitals <- na.omit( hospitals[with(hospitals,order(State,Outcome,Name)),])
   
   # Get the hospital names for each state
   hospital <- tapply(hospitals$Name,hospitals$State,getname,num) 
   
   # Return hospital/state in a frame 
   data.frame(hospital,state=names(hospital))
}
