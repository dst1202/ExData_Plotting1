plot1 <- function() {
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

        ## Plot 1
        png(file = "plot1.png") ## open png device, default is 480*480 as required
        hist(powerData$Global_active_power, col = "red", 
             xlab = "Global Active Power (kilowatts)", 
             main = "Global Active Power")
        dev.off()
}