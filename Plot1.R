# Histogram of GAP for 2/1/2007-2/2/2007

library("maps")
library("mapproj")
data(county.fips)

numbreaks <- 5
startcolor <- "white"
midcolor <- "orange"
endcolor <- "red"
colfunc <- colorRampPalette(c(startcolor, midcolor, endcolor))
colors = colfunc(numbreaks)

source("DataLoader.R")
pd <- loadPollutionData(verbose=TRUE, joinTables = FALSE)

emissionsum <- aggregate(pd$pol$Emissions, by=list(pd$pol$year, pd$pol$fips), FUN=sum)
# group1=year, group2=fips, x=emissions

emissionsum$logsum <- log(1 + emissionsum$x)
emissionsum$fips <- as.numeric(emissionsum$Group.2)
emissionsum$colorbuckets <- as.numeric(cut(emissionsum$logsum, breaks=numbreaks))
#emissionsum$colorbuckets <- as.numeric(cut(emissionsum$x, breaks=numbreaks))
colsmatched <- emissionsum$colorbuckets[match(county.fips$fips, emissionsum$fips)]

#png("Plot1.png")
plot.new()
map("county"
    , col = colors[colsmatched]
    , fill = TRUE
    , resolution = 0
    , lty = 0
    , projection = "polyconic")
map("state"
    , col = "white"
    , fill = FALSE
    , add = TRUE
    , lty = 1
    , lwd = 0.2
    , projection = "polyconic")
leg.txt <- as.character(c(1:numbreaks))
legend("bottom"
       , legend=as.character(leg.txt)
       #, horiz = TRUE
       , fill = colors
       , ncol=min(10, numbreaks)) #gives max 10 legend columns before wrapping to another row
title("log(PM25 Emissions) Across The US By County")

#map("world", c("hawaii"), boundary = TRUE, col=8, add = TRUE, fill=TRUE )
#map("world", c("USA:Alaska"), boundary = TRUE, col='orange', add = TRUE, fill=TRUE )

#dev.off()
