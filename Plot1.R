# Histogram of GAP for 2/1/2007-2/2/2007

library("maps")
library("mapproj")
data(county.fips)
colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")

source("DataLoader.R")
pd <- loadPollutionData(verbose=TRUE, joinTables = FALSE)

emissionsum <- aggregate(pd$pol$Emissions, by=list(pd$pol$year, pd$pol$fips), FUN=sum)
# group1=year, group2=fips, x=emissions

emissionsum$logsum <- log(emissionsum$x)
emissionsum$fips <- as.numeric(emissionsum$Group.2)
emissionsum$colorbuckets <- as.numeric(cut(log(1+ emissionsum$x), breaks=7))

colsmatched <- emissionsum$colorbuckets[match(county.fips$fips, emissionsum$fips)]

map("county"
    , col = colors[colsmatched]
    , fill = TRUE
    , resolution = 0
    , lty = 0
    , projection = "polyconic")
map("world", c("hawaii"), boundary = TRUE, col=8, add = TRUE, fill=TRUE )
map("world", c("USA:Alaska"), boundary = TRUE, col='orange', add = TRUE, fill=TRUE )

#png("Plot1.png")



dev.off()
