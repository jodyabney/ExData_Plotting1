######################################################################
## Exploratory Data Analysis: Project 1  - Plot 4                   ##
## Coursera: John Hopkins Bloomberg School of Public Health         ##
## Coursera Specialization: John Hopkins Data Science               ##
## Course: Exploratory Data Analysis                                ##
## Author: Jody P. Abney                                            ##
######################################################################

makePlot4 <- function() {
    ## Reads the "Household Power Consumption" data file (text format)
    ## from the current working directory and creates plot4.png file
    ## in the current working directory. Plot4.png consist of 4 plots 
    ## in a single plot
    
    ## Read in the entire file from the current working directory using the 
    ## header row for colum names with a semicolon separation character and
    ## using "? as the NA string
    proj1_data_whole <- read.table("household_power_consumption.txt", 
                                   header = TRUE, 
                                   sep = ";", 
                                   na.strings = "?")
    message("data file read complete")
    
    ## Subset the data set for the two dates of interest (2007-02-01 and 2007-02-02)
    ## then combine the two dates into a single dataset
    proj1_data01 <- subset(proj1_data_whole, proj1_data_whole$Date == "1/2/2007")
    proj1_data02 <- subset(proj1_data_whole, proj1_data_whole$Date == "2/2/2007")
    proj1_data <- rbind(proj1_data01, proj1_data02)
    message("data subset complete")    
    
    ## Combine the $Date and $Time columns to actual date/time content
    ## in new column $DateTime
    dates <- proj1_data$Date
    times <- proj1_data$Time
    proj1_data$DateTime <- paste(dates,times)
    proj1_data$DateTime <- strptime(proj1_data$DateTime, 
                                    format = "%d/%m/%Y %H:%M:%S")
    message("DateTime column changed to actual date/time content")
    
    ## Set up the PNG device for saving the Plot1 histogram to the current
    ## working directory
    png(file = "plot4.png", width = 480, height = 480)
    message("png device opened")
    
    ## Create the 2x2 matrix for the plots
    par(mfrow=c(2,2))
    
    ## Create the "Plot1" line plot in the [1,1] position
    plot(x = proj1_data$DateTime, 
                  y = proj1_data$Global_active_power, 
                  type = "l",
                  main = NULL, 
                  col = "black", 
                  xlab = "", 
                  ylab = "Global Active Power (kilowatts)")
    
    ## Create the "voltage" plot in the [1,2] position
    plot(x = proj1_data$DateTime, 
         y = proj1_data$Voltage, 
         type = "l",
         main = NULL, 
         col = "black", 
         xlab = "datetime", 
         ylab = "Voltage")
    
    ## Create the "Plot3" line plot in the [2,1] position
    plot(x = proj1_data$DateTime, 
         y = proj1_data$Sub_metering_1,
         type = "l",
         main = NULL, 
         col = "black",
         xlab = "", 
         ylab = "Energy sub metering",
         ylim = range(proj1_data$Sub_metering_1,
                      proj1_data$Sub_metering_2,
                      proj1_data$Sub_metering_3))
    par(new=TRUE)
    plot(x = proj1_data$DateTime, 
         y = proj1_data$Sub_metering_2,
         type = "l",
         main = NULL, 
         col = "red",
         xlab = "", 
         ylab = "Energy sub metering",
         ylim = range(proj1_data$Sub_metering_1,
                      proj1_data$Sub_metering_2,
                      proj1_data$Sub_metering_3))
    par(new=TRUE)
    plot(x = proj1_data$DateTime, 
         y = proj1_data$Sub_metering_3,
         type = "l",
         main = NULL, 
         col = "blue",
         xlab = "", 
         ylab = "Energy sub metering",
         ylim = range(proj1_data$Sub_metering_1,
                      proj1_data$Sub_metering_2,
                      proj1_data$Sub_metering_3))
    par(new=FALSE)
    legend("topright", 
           bty = "n",
           c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           lty=c(1,1,1),
           lwd=c(1.0,1.0,1.0),
           col=c("black", "blue","red"))
    
    ## Create the "Global_reactive_power" plot in the [2,2] position
    plot(x = proj1_data$DateTime, 
         y = proj1_data$Global_reactive_power, 
         type = "l",
         main = NULL, 
         col = "black", 
         xlab = "datetime", 
         ylab = "Global_reactive_power")
    
    message("plot completed")
    
    ## Close the PNG device to finsh writing the Plot1 histogram file to the
    ## current working direcory
    dev.off()
    message("png device closed")
    
}