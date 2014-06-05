######################################################################
## Exploratory Data Analysis: Project 1  - Plot 1                   ##
## Coursera: John Hopkins Bloomberg School of Public Health         ##
## Coursera Specialization: John Hopkins Data Science               ##
## Course: Exploratory Data Analysis                                ##
## Author: Jody P. Abney                                            ##
######################################################################

makePlot1 <- function() {
## Reads the "Household Power Consumption" data file (text format)
## from the current working directory and creates Plot1.png file
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
    
    ## Change the $Date column to actual date content
    proj1_data$Date <- as.Date(proj1_data$Date, format = "%d/%m/%Y")
    message("date column changed to actua date content")

    ## Set up the PNG device for saving the Plot1 histogram to the current
    ## working directory
    png(file = "plot1.png", width = 480, height = 480)
    message("png device opened")
    
    ## Create the Plot1 histogram
    plot1 <- hist(proj1_data$Global_active_power, 
                  main = "Global Active Power", 
                  col = "red", 
                  xlab = "Global Active Power (kilowatts)", 
                  ylab = "Frequency")
    message("plot completed")
    
    ## Close the PNG device to finsh writing the Plot1 histogram file to the
    ## current working direcory
    dev.off()
    message("png device closed")
    
}