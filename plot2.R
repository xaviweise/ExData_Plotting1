plot2 <- function(path, filename)
{
  #Read data for the two dates
  fullfile = paste(path, "/", sep = "")
  fullfile = paste(fullfile, filename, sep = "")
  input <- read.table(fullfile, sep = ";", header = TRUE, na.strings = "?")
  
  date_01 <- input[input$Date == "1/2/2007",]
  date_02 <- input[input$Date == "2/2/2007",]
  
  #Take Global Active Power for the two days
  total_active_pwr <- c(date_01$Global_active_power, date_02$Global_active_power)
  
  #Create data for time merging dates and times and converting to R time class
  time_01 <- paste(date_01$Date, date_01$Time)
  time_02 <- paste(date_02$Date, date_02$Time)
  
  total_time <- c(time_01, time_02)  
  total_time <- strptime(total_time, "%d/%m/%Y %H:%M:%S")
  
  #Build plot in plot2.png (the default is 480x480)
  png(file = "plot2.png")
  plot(total_time, total_active_pwr, type = "n", xlab = "", ylab = "Global Active Power (kilowatts)")
  lines(total_time, total_active_pwr)
  dev.off()

}




