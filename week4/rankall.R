rankall <- function(outcome, num = "best") {
## Read outcome data
dataset <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
# dataset <- na.omit(dataset)
dataset <- dataset[,c(2,7,11,17,23)]
colnames(dataset) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
split.list <- split(dataset, dataset$state)
## Check that state and outcome are valid
## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name
sortbyorder <- data.frame()
        for(i in 1:length(split.list)) {
                outcome.order <- order(as.numeric(split.list[[i]][,outcome]),split.list[[i]][,"hospital"], na.last = NA)
                b <- length(outcome.order)
                if(num == "best") {
                        num <- c(1)
                        sortbyorder <- rbind(sortbyorder, split.list[[i]][outcome.order[1],c(1,2)])
                }else if(num == "worst") {
                        # num <- c(b)
                        sortbyorder <- rbind(sortbyorder, split.list[[i]][outcome.order[c(b)],c(1,2)])
                }else {
                        num <- c(num)
                        sortbyorder <- rbind(sortbyorder, split.list[[i]][outcome.order[num],c(1,2)])
                }
                #sortbyorder <- rbind(sortbyorder, split.list[[i]][outcome.order[num],c(1,2)])
                sortbyorder[i,2] <- c(unique(split.list[[i]][,"state"]))
        }
                sortbyorder
}