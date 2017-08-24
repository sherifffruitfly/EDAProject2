# ggplot2 for baltimore city md emissions changes by type year to year

library(grid)
library("ggplot2")
source("DataLoader.R")

if(nrow(pd) != 13624){
  message("loading data")
  pd <- loadPollutionData(verbose=FALSE, joinTables = TRUE)
  pd <- pd[pd$fips_N == 24510,]
}

message("aggregating by year")
emissionsum <- aggregate(pd$Emissions, by=list(pd$year, pd$type), FUN=sum)
names(emissionsum) <- c("Year", "Type", "Emissions")


myplot <- ggplot(emissionsum
             , aes(interaction(Year, Type), Emissions, fill=Type)) + 
        geom_bar(width = 0.5
                 , position = position_dodge(width=.1)
                 , stat="identity"
        ) + 
        coord_cartesian(ylim = c(0, 2500), expand = FALSE) +
        annotate(geom = "text", x = seq_len(nrow(emissionsum)), y = -40, label = emissionsum$Year, size = 3) +
        annotate(geom = "text", x = 2.5 + 4 * (0:3), y = -130, label = unique(emissionsum$Type), size = 4) +
        theme_bw() +
        theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        theme(legend.position = "none")


# make a grob, turn off clipping
myplot2 <- ggplot_gtable(ggplot_build(myplot))
myplot2$layout$clip[myplot2$layout$name == "panel"] <- "off"
grid::grid.draw(myplot2)