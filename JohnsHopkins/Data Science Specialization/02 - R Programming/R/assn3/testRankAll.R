testRanks <- function() {
    
    Sys.time() -> start1
    rankall1("heart attack")
    print (Sys.time()-start1) 

    Sys.time() -> start2
    rankall2("heart attack")
    print (Sys.time()-start2) 
    
}
