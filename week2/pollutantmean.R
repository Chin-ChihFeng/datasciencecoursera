pollutantmean <- function(directory, pollutant, id = 1:332) {
        if(file.exists(directory)) {
                # print('Yes, the directory exist.')
                file.name <- list.files(directory, full.names = TRUE)
                conbine.file <- data.frame()
                for(i in id) {
                        dataset <- read.csv(file.name[i])
                        conbine.file <- rbind(conbine.file, dataset)
                }
                
                # remove.na <- complete.cases(conbine.file[[pollutant]])
                # result <- mean(conbine.file[[pollutant]][remove.na])
                result <- mean(conbine.file[[pollutant]], na.rm = TRUE)
                result <- round(result, 3)
                result
        }else {
                print('Sorry, the directory is not exist. Please check the directory is locate under your home path.')
        }
}