rankhospital <- function(state, outcome, num = "best"){
    ## Read outcome data
    f <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    all_state <- unique(f$State)
    if(!state %in% all_state)
        stop("invalid state")
    if(!outcome %in% c("heart attack","heart failure","pneumonia"))
        stop("invalid outcome")
    
    ## Return hospital name in that state with the given rank
    c_ndata <- NULL
    {
        if(outcome == "heart attack"){
            c_ndata <- 11
        }
        else if(outcome == "heart failure"){
            c_ndata <- 17
        }
        else if(outcome == "pneumonia"){
            c_ndata <- 23
        }
    }
    
    f <- f[c(2, 7, c_ndata)]
    names(f) <- c("name", "state", outcome)
    
    f <- f[f$state == state & f[outcome] != "Not Available",]
    f[, 3] <- as.numeric(f[, 3])
    f <- cbind(f[order(f[outcome],f[,1]),],c(1:nrow(f)))
    names(f) <- c("name", "state", "Rate", "Rank")
    

    if(num == "best"){
        num <- 1
    }else if(num == "worst")
        num <- nrow(f)

    
    
    ## 30-day death rate
    f$name[num]
    
}