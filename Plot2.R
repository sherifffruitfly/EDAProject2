# Line graph of GAP over 2/1/2007-2/2/2007

source("DataLoader.R")
plotdata <- loadData()

png("Plot2.png")

plot(plotdata$datetime, plotdata$Global_active_power
    , xlab = ""
    , ylab = "Global Active Power (kilowatts)"
    , type="l"
)

dev.off()