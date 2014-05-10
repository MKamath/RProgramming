corr <- function(directory, threshold = 0) {
        total <- complete(directory)
        acceptable <- total[,"nobs"] > threshold
        corrected <- total[acceptable,]
        if (nrow(corrected) > 0) {
                fileID <- corrected[,"id"]
                path_name <- paste("C:\\ ... \\data\\", directory, "\\", sep = "")
                filenames <- list.files(path_name, pattern="*.csv",)
                filepath <- file.path(paste(path_name, filenames[fileID], sep=""))
                output <- numeric(length(fileID))
                for(i in 1:length(fileID)) {
                        myData <- read.table(filepath[i], header = TRUE, sep = ",")
                        extract <- myData[complete.cases(myData),]
                        acceptable <- nrow(extract) > threshold
                        if(acceptable) {
                                output[i] <- cor(extract[,"sulfate"], extract[,"nitrate"])
                        }
                }
        } else {
                
                output <- numeric(0)
        }
        output
}