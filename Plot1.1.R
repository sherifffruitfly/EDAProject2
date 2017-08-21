library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
data("county.regions")

source("DataLoader.R")
message("loading data")
pd <- loadPollutionData(verbose=FALSE, joinTables = FALSE)

message("aggregating by year/county")
emissionsum <- aggregate(pd$pol$Emissions, by=list(pd$pol$year, pd$pol$fips), FUN=sum)
emissionsum$value <- log(1 + emissionsum$x) # log(1+emissions)
#emissionsum$value <- emissionsum$x # emissions actual value
emissionsum$region <- as.numeric(emissionsum$Group.2)

# get unique region values for choropeth call
# year values: 1999, 2002, 2005, 2008
plotYear <- 2002
message("fitlering for ", plotYear)
emissionsum <- emissionsum[emissionsum$Group.1 == plotYear,]

# get rid of rows that don't map to fips values in choropeth
message("tossing unmappable regions")
emissionsum <- merge(emissionsum, county.regions, by="region")

message("constructing plot")
choro = CountyChoropleth$new(emissionsum)
choro$title = paste("                                 PM25 Emissions Across The US By County, ", plotYear)
choro$ggplot_scale = scale_fill_brewer(name="Emissions", palette=7, drop=FALSE)
choro$render()