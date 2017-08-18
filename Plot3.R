# Sub metering plots over 2/1/2007-2/2/2007

source("DataLoader.R")
plotdata <- loadData()

png("Plot3.png")

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
       )

dev.off()