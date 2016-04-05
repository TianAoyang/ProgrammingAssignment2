rankall <- function(outcome, num = "best") {
    ## Read outcome data
    f <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if(!outcome %in% c("heart attack","heart failure","pneumonia"))
        stop("invalid outcome")
    
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
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    f_rank <- data.frame()
    f <- f[c(2, 7, c_ndata)]
    names(f) <- c("name", "state", outcome)
    
    f <- f[f[outcome] != "Not Available",]
    f[, 3] <- as.numeric(f[, 3])
    
    f <- cbind(f[order(f[outcome], f[,1]),],c(1:nrow(f)))
    names(f) <- c("name", "state", "Rate", "Rank")
    ##names(f_rank) <- names(f)
    
    all_state <- unique(f$state)
    for(i in all_state){
        f_tmp <- f[f$state == i,]
        
        if(num == "best"){
            f_rank <- rbind(f_rank, f_tmp[1, 1:ncol(f_tmp)])
        }else if(num == "worst"){
            f_rank <- rbind(f_rank, f_tmp[nrow(f_tmp), 1:ncol(f_tmp)])
        }else if(num < nrow(f_tmp)){
            f_rank <- rbind(f_rank, f_tmp[num, 1:ncol(f_tmp)])
        }else{
            f_rank <- rbind(f_rank, list(NA, i, NA, NA))
        }
            
    }
    names(f_rank) <- names(f)
    
    f_rank[order(f_rank$state),]
}
