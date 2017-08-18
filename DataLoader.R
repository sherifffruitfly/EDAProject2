# load data for use in other scripts' plots

download <- download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",destfile = "temp")
unzip("temp")
unlink("temp")

loadData <- function(filename="household_power_consumption.txt"
                     , startdate=as.Date("2007-02-01")
                     , enddate=as.Date("2007-02-02")
                     , clearenv=FALSE
                     )
{
  if(clearenv == TRUE) {
    rm(list = ls())
    message("Environment cleared")
  }
  
  data <- read.table("household_power_consumption.txt", sep = ";", header = TRUE, na.strings = "?")
  data$Date <- as.Date(data$Date, "%d/%m/%Y")
  data$datetime <- strptime(paste(data$Date, data$Time), "%Y-%m-%d %H:%M:%S")
  data.filtered <- subset(data, Date >= startdate & Date <= enddate)
  
  file.remove("household_power_consumption.txt")
  
  return(data.filtered)
}

