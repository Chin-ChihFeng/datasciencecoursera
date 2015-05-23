best <- function(state, outcome) {
        ## Read outcome data
        dataset <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
        dataset <- na.omit(dataset)
        dataset <- dataset[,c(2,7,11,17,23)]
        colnames(dataset) <- c("Hospital.Name", "State", "heart attack", "heart failure", "pneumonia")
        ## Check that state and outcome are valid
        split.list <- split(dataset, dataset$State)
        ## Return hospital name in that state with lowest 30-day death
        ## ratel
        if(length(dataset[[outcome]]) != 0) {
                if(length(split.list[[state]][,outcome]) != 0) {
                        min <- min(as.numeric(split.list[[state]][,outcome], na.rm = TRUE))
                        n <- length(split.list[[state]][,outcome])
                        for(i in 1:n) {
                                if(is.element(min, as.numeric(split.list[[state]][i,outcome]))) { 
                                        # print(as.character(split.list[[state]][i,"Hospital.Name"]))
                                        result <- split.list[[state]][i,"Hospital.Name"]
                                }
                        }
                                        print(result)
                }else {
                        stop("invalid state")
                }
        }else {
                stop("invalid outcome")
        }              
}

