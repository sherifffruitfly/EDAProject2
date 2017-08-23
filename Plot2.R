# bar plot of emissions in Baltimore CIty, Maryland by year

source("DataLoader.R")

if(nrow(pd) != 13624){
  message("loading data")
  pd <- loadPollutionData(verbose=FALSE, joinTables = TRUE)
  pd <- pd[pd$fips_N == 24510,]
}

message("aggregating by year")
emissionsum <- aggregate(pd$Emissions, by=list(pd$year, pd$type), FUN=sum)
names(emissionsum) <- c("Year", "Type", "Emissions")
matrixemissions <- as.matrix(as.data.frame.matrix(xtabs(Emissions ~ Type + Year, emissionsum)))

plot.new()
plotcolors <- c("#f7fcb9", "#7fcdbb", "#bcbddc", "sandybrown")
myplot <- barplot(matrixemissions
            , main=bquote(PM[2.5] ~ "Emissions For Baltimore City, MD By Year")
            , col=plotcolors
            , xlab="Year"
            , ylab="Emissions (tons)"
            )
text(myplot
     , colSums(matrixemissions)+75
     , format(round(colSums(matrixemissions), digits=1), nsmall=1, big.mark=",")
     , xpd=TRUE
     , cex=.7
     , col="black"
     )
legend("topright"
       , legend=rownames(matrixemissions)
       , title="Type"
       , fill=plotcolors
       , cex=.7
        )
# need to shift regression line back to 0=1999
xvals <- as.numeric(colnames(matrixemissions)) - min(as.numeric(colnames(matrixemissions)))
mymodel <- lm(colSums(matrixemissions) ~ xvals)
abline(mymodel
      , col="gray40"
      , lty=5
      , lwd=2
      )
text(4.3, 2600
     , paste("m = ", round(mymodel$coefficients[2], digits=2))
     , col="gray40"
     )

