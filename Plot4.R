# Multiple independent graphs over 2/1/2007-2/2/2007

source("DataLoader.R")
plotdata <- loadData()

png("Plot4.png")

par(mfrow=c(2,2))
par(mar=c(4,4,4,4))

plot(plotdata$datetime, plotdata$Global_active_power
     , type = "l"
     , xlab = ""
     , ylab = "Global Active Power (kilowatts)"
     )

plot(plotdata$datetime, plotdata$Voltage
     , type = "l"
     , xlab = "datetime"
     , ylab = "Voltage"
     )

plot(plotdata$datetime, plotdata$Sub_metering_1
     , type = "l"
     , xlab = ""
     , ylab = "Energy sub metering"
     )
points(plotdata$datetime, plotdata$Sub_metering_2
       , type = "l"
       , col = "red"
       )
points(plotdata$datetime, plotdata$Sub_metering_3
       , type = "l"
       , col = "blue"
       )
legend("topright"
       , lty = 1
       , col = c("black", "red", "blue")
       , legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
       , bty = "n"
       , cex = 0.8
       )

plot(plotdata$datetime, plotdata$Global_reactive_power
     , type = "l"
     , xlab = "datetime"
     , ylab = "Global_reactive_power"
     )

dev.off()
