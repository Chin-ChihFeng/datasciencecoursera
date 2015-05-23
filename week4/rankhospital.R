rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        dataset <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
        dataset <- dataset[,c(2,7,11,17,23)]
        colnames(dataset) <- c("Hospital.Name", "State", "heart attack", "heart failure", "pneumonia")
        ## Check that state and outcome are valid
        split.list <- split(dataset, dataset$State)
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        if(length(dataset[[outcome]]) != 0) {
                if(length(split.list[[state]][,outcome]) != 0) {
                        outcome.order <- order(as.numeric(split.list[[state]][,outcome]),split.list[[state]][,"Hospital.Name"], na.last = NA)
                        sortbyorder <- data.frame()
                        j <- as.numeric(match(TRUE,names(dataset) %in% outcome))
                        for(i in 1:length(outcome.order)) {
                                sortbyorder <- rbind(sortbyorder, split.list[[state]][outcome.order[i],c(1,j)])
                        }
                        Rank <- rank(as.numeric(sortbyorder[[outcome]]), ties.method = "first")
                        sortbyorder <- cbind(sortbyorder, Rank)
                        if(num == "best") {
                                head(sortbyorder[,1],1)
                        }else if(num == "worst") { 
                                tail(sortbyorder[,1],1)
                        }else {
                                sortbyorder[num,1]
                        }
                        
                }else {
                        stop("invalid state")
                }
        }else {
                stop("invalid outcome")
        }
}