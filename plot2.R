######################################################################
## Exploratory Data Analysis: Project 1  - Plot 2                   ##
## Coursera: John Hopkins Bloomberg School of Public Health         ##
## Coursera Specialization: John Hopkins Data Science               ##
## Course: Exploratory Data Analysis                                ##
## Author: Jody P. Abney                                            ##
######################################################################

makePlot2 <- function() {
## Reads the "Household Power Consumption" data file (text format)
## from the current working directory and creates plot2.png file
## in the current working directory

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

    ## Set up the PNG device for saving the Plot2 histogram to the current
    ## working directory
    png(file = "plot2.png", width = 480, height = 480)
    message("png device opened")
    
    ## Create the Plot2 line plot
    plot2 <- plot(x = proj1_data$DateTime, 
                  y = proj1_data$Global_active_power, 
                  type = "l",
                  main = NULL, 
                  col = "black", 
                  xlab = "", 
                  ylab = "Global Active Power (kilowatts)")
    message("plot completed")
    
    ## Close the PNG device to finsh writing the Plot1 histogram file to the
    ## current working direcory
    dev.off()
    message("png device closed")
    
}