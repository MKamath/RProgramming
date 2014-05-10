pollutantmean <- function(directory, pollutant, id = 1:332) {
        path_name <- paste("C:\\ ... \\data\\", directory, "\\", sep = "")
        filenames <- list.files(path_name, pattern="*.csv",)
        sums <- numeric(length(id))
        no_of_rows <- numeric(length(id))
        filepath <- file.path(paste(path_name, filenames[id], sep=""))
        for(i in 1:length(id)) {
                myData <- read.table(filepath[i], header = TRUE, sep = ",")
                extract <- myData[complete.cases(myData[,pollutant]),][,pollutant]
                sums[i] <- sum(extract)
                no_of_rows[i] <- length(extract)
        }
        means <- sum(sums)/sum(no_of_rows)
        round(means, 3)
}