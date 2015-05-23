complete <- function(directory, id = 1:332) {
       if(file.exists(directory)) {
                # print('Yes, the directory exist.')
                file.name <- list.files(directory, full.names = TRUE)
                conbine.file <- data.frame()
                for(i in id) {
                        dataset <- read.csv(file.name[i])
                        remove.na <- complete.cases(dataset[["sulfate"]], dataset[["nitrate"]])
                        result <- dataset[remove.na, ]
                        if(nrow(result) == 0) {
                                result.1 <- c(id = unique(dataset[["ID"]]), nobs = 0) 
                        }else {
                                result.1 <- c(id = unique(result[["ID"]]), nobs = nrow(result))
                        }
                        # result.1 <- c(id = unique(result[["ID"]]), nobs = nrow(result))
                        conbine.file <- rbind(conbine.file, result.1)
                }
                
                colnames(conbine.file) <- c("id", "nobs")
                conbine.file
        }else {
                print('Sorry, the directory is not exist. Please check the directory is locate under your home path.')
        }
}