complete <- function(directory, id = 1:332) {
        path_name <- paste("C:\\ ... \\data\\", directory, "\\", sep = "")
        filenames <- list.files(path_name, pattern="*.csv",)
        filepath <- file.path(paste(path_name, filenames[id], sep=""))
        nobs <- numeric(length(id))
        for(i in 1:length(id)) {
                myData <- read.table(filepath[i], header = TRUE, sep = ",")
                nobs[i] <- nrow(myData[complete.cases(myData),])
        }
        myDat <- data.frame(id, nobs)
        myDat
}