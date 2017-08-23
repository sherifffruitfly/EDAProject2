source("DataLoader.R")
message("loading data")
if(nrow(pd) != 6497651){
  pd <- loadPollutionData(verbose=FALSE, joinTables = TRUE)
}

message("aggregating by year")
emissionsum <- aggregate(pd$Emissions, by=list(pd$year, pd$type), FUN=sum)
names(emissionsum) <- c("Year", "Type", "Emissions")
matrixemissions <- as.matrix(as.data.frame.matrix(xtabs(Emissions/1000000 ~ Type + Year, emissionsum)))

plot.new()
plotcolors <- c("#f7fcb9", "#7fcdbb", "#bcbddc", "sandybrown")
barplot(matrixemissions
        , main=bquote(PM[2.5] ~ "Emissions For The U.S. By Year")
        , col=plotcolors
        , xlab="Year"
        , ylab="Emissions (millions of tons)"
        )
legend("topright"
       , legend=rownames(matrixemissions)
       , title="Type"
       , fill=plotcolors
       )
