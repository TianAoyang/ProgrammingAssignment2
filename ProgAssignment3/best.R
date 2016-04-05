best <- function(state, outcome) {
    ## Read outcome data
    f <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    all_state <- unique(f$State)
    if(!state %in% all_state)
        stop("invalid state")
    if(!outcome %in% c("heart attack","heart failure","pneumonia"))
        stop("invalid outcome")
    
    ## Return hospital name in that state with lowest 30-day death
    f <- f[c(2, 7, 11, 17, 23)]
    names(f) <- c("name", "state", "heart attack", "heart failure", "pneumonia")
    
    f <- f[f$state == state & f[outcome] != "Not Available",]
    rate_hos <- which.min(f[, outcome])
    
    ## rate
    f[rate_hos,]$name
}