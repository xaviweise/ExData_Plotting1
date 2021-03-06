# Plotting function 3. 
# Inputs: path and filename with data set
# Output: graph of the data using base plotting system in .png in working directory
plot3 <- function(path, filename)
{
  #Read data for the two dates
  fullfile = paste(path, "/", sep = "")
  fullfile = paste(fullfile, filename, sep = "")
  input <- read.table(fullfile, sep = ";", header = TRUE, na.strings = "?")
  
  date_01 <- input[input$Date == "1/2/2007",]
  date_02 <- input[input$Date == "2/2/2007",]
  
  #Take the three sub metering
  total_met_1 <- c(date_01$Sub_metering_1, date_02$Sub_metering_1)
  total_met_2 <- c(date_01$Sub_metering_2, date_02$Sub_metering_2)
  total_met_3 <- c(date_01$Sub_metering_3, date_02$Sub_metering_3)
  
  #Create data for time merging dates and times and converting to R time class
  time_01 <- paste(date_01$Date, date_01$Time)
  time_02 <- paste(date_02$Date, date_02$Time)
  
  total_time <- c(time_01, time_02)  
  total_time <- strptime(total_time, "%d/%m/%Y %H:%M:%S")
  
  #Build plot in plot3.png (the default is 480x480)
  png(file = "plot3.png")
  
  plot(total_time, total_met_1, type = "n", xlab = "", ylab = "Energy sub metering")
  points(total_time, total_met_2, type = "n")
  points(total_time, total_met_3, type = "n")
  
  lines(total_time, total_met_1)
  lines(total_time, total_met_2, col = "red")
  lines(total_time, total_met_3, col = "blue")
  
  #add legend in the corner
  legend("topright", lty = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  dev.off()
}