# Plotting function 4. 
# Inputs: path and filename with data set
# Output: graph of the data using base plotting system in .png in working directory
plot4 <- function(path, filename)
{
  #Read data for the two dates
  fullfile = paste(path, "/", sep = "")
  fullfile = paste(fullfile, filename, sep = "")
  input <- read.table(fullfile, sep = ";", header = TRUE, na.strings = "?")
  
  date_01 <- input[input$Date == "1/2/2007",]
  date_02 <- input[input$Date == "2/2/2007",]
  
  #Take Global Active Power for the two days
  total_active_pwr <- c(date_01$Global_active_power, date_02$Global_active_power)
  
  #Take the three sub metering
  total_met_1 <- c(date_01$Sub_metering_1, date_02$Sub_metering_1)
  total_met_2 <- c(date_01$Sub_metering_2, date_02$Sub_metering_2)
  total_met_3 <- c(date_01$Sub_metering_3, date_02$Sub_metering_3)
  
  #Take Voltage
  total_voltage <- c(date_01$Voltage, date_02$Voltage)
  
  #Take Global Reactive Power
  total_reactive_pwr <- c(date_01$Global_reactive_power, date_02$Global_reactive_power)
  
  #Create data for time merging dates and times and converting to R time class
  time_01 <- paste(date_01$Date, date_01$Time)
  time_02 <- paste(date_02$Date, date_02$Time)
  
  total_time <- c(time_01, time_02)  
  total_time <- strptime(total_time, "%d/%m/%Y %H:%M:%S")
  
  #Build plot in plot4.png (the default is 480x480)
  png(file = "plot4.png")
  
  #multiple plots
  par(mfrow = c(2,2))

  #plot 1
  plot(total_time, total_active_pwr, type = "n", xlab = "", ylab = "Global Active Power")
  lines(total_time, total_active_pwr)
  
  #plot 2
  plot(total_time, total_voltage, type = "n", xlab = "datetime", ylab = "Voltage")
  lines(total_time, total_voltage)
  
  #plot 3
  plot(total_time, total_met_1, type = "n", xlab = "", ylab = "Energy sub metering")
  points(total_time, total_met_2, type = "n")
  points(total_time, total_met_3, type = "n")
  lines(total_time, total_met_1)
  lines(total_time, total_met_2, col = "red")
  lines(total_time, total_met_3, col = "blue")
  legend("topright", lty = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), bty = "n")
 
  #plot 4
  plot(total_time, total_reactive_pwr, type = "n", xlab = "datetime", ylab = "Global_reactive_power")
  lines(total_time, total_reactive_pwr)
  
  dev.off()
}