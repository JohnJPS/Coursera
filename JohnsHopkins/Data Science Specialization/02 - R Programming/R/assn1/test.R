htSum <- function(ht) {
    s = 0
    for (i in 5:length(ht)) {
        s = s + as.numeric(substr(ht[[i]],29,32))
    }
    s
}