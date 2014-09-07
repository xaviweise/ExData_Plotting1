# Plotting function 1. 
# Inputs: path and filename with data set
# Output: graph of the data using base plotting system in .png in working directory
plot1 <- function(path, filename)
{
  #Read data for the two dates
  fullfile = paste(path, "/", sep = "")
  fullfile = paste(fullfile, filename, sep = "")
  input <- read.table(fullfile, sep = ";", header = TRUE, na.strings = "?")
    
  date_01 <- input[input$Date == "1/2/2007",]
  date_02 <- input[input$Date == "2/2/2007",]
  
  #Take Global Active Power for the two days
  total_active_pwr <- c(date_01$Global_active_power, date_02$Global_active_power)
  
  #Build histogram in plot1.png (the default is 480x480)
  png(file = "plot1.png")
  hist(total_active_pwr, xlab = "Global Active Power (kilowatts)", col = "red", main = "Global Active Power")
  dev.off()

}



