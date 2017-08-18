# Histogram of GAP for 2/1/2007-2/2/2007

source("DataLoader.R")
plotdata <- loadData()

png("Plot1.png")

hist(plotdata$Global_active_power
     , col = "red"
     , xlab = "Global Active Power (kilowatts)"
     , ylab = "Frequency"
     , main = "Global Active Power"
     , breaks = 15
     , ylim = c(0,1200)
     , xlim = c(0, 6)
     , axes=FALSE
)
axis(side=1, at=c(0, 2, 4, 6))
axis(side=2, at=c(0, 200, 400, 600, 800, 1000, 1200))

dev.off()
