corr <- function(directory, threshold = 0) {
        source("complete.R")
        file.name <- list.files(directory, full.names = TRUE)
        total.result <- complete(directory)
        conbine.file <- data.frame()
        getid <- total.result$id
        for(i in getid) {
                dataset <- read.csv(file.name[i])
                remove.na <- complete.cases(dataset[["sulfate"]], dataset[["nitrate"]])
                result <- dataset[remove.na, ]
                crr <- cor(result$sulfate, result$nitrate)
                conbine.file <- rbind(conbine.file, crr)
        }
                x <- as.matrix(sapply(conbine.file, as.numeric))
                colnames(x) <- c("crr")
                final.data.frame <- cbind(total.result, x )
                final.data.frame$crr[final.data.frame$nobs > threshold]
}