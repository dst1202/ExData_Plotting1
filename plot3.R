plot3 <- function() {
        ## check file exists
        if(!file.exists("exdata_data_household_power_consumption.zip")) {
                stop("data not in working directory")
        }
        ## number of lines to skip
        nskip <- ((6 * 60) + 37 + (15 * 1440) + (31 * 1440))
        ## create connection to zip file
        dataFile <- unz(description = "exdata_data_household_power_consumption.zip", 
                        filename = "household_power_consumption.txt")
        ## read data
        powerData <- read.table(file = dataFile, skip = nskip, nrows = 2880, 
                                sep = ";", na.strings = "?", header = FALSE)
        ## create connection to zip file
        dataFile <- unz(description = "exdata_data_household_power_consumption.zip", 
                        filename = "household_power_consumption.txt")
        ## get colnames
        pNames <- read.table(file = dataFile, nrows = 2, sep = ";", 
                             na.strings = "?", header = TRUE)
        
        ## replace date and time columns with a datetime object
        powerData <- cbind(strptime(paste(powerData$V1, powerData$V2), 
                                    format = "%d/%m/%Y %H:%M:%S"), 
                           powerData[,-(c(1,2))])
        
        ## update column names
        colnames(powerData) <- c("datetime",names(pNames[,-c(1,2)]))
        

        ##plot3
        png(file = "plot3.png", type = "cairo")
        plot(powerData$datetime, powerData$Sub_metering_1, type = "l", 
             xlab = "", ylab = "Energy sub metering")
        lines(powerData$datetime, powerData$Sub_metering_2, type = "l", 
              col = "red")
        lines(powerData$datetime, powerData$Sub_metering_3, type = "l", 
              col = "blue")
        legend("topright", legend = c("Sub_metering_1","Sub_metering_2",
                                      "Sub_metering_3") , 
               lwd = 1, col = c("black","red", "blue"))
        dev.off()
}
