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

# year values: 1999, 2002, 2005, 2008
plotYear <- 1999

# get rid of rows that don't map to fips values in choropeth
message("tossing unmappable regions")
emissionsum <- merge(emissionsum, county.regions, by="region")

message("constructing plot")
choro = CountyChoropleth$new(emissionsum[emissionsum$Group.1 == plotYear,])
choro$title = bquote("                                 " ~ PM[2.5] ~ "Emissions Across The U.S. By County," ~ .(plotYear))
choro$ggplot_scale = scale_fill_brewer(name="Emissions", palette=7, drop=FALSE)
choro$warn <- FALSE
choro$set_num_colors(9)
choro$render()
