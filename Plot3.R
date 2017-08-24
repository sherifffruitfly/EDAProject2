# ggplot2 for baltimore city md emissions changes by type year to year

library("scales")
library(data.table)
library(grid)
library("ggplot2")
source("DataLoader.R")

if(nrow(pd) != 13624){
  message("loading data")
  pd <- loadPollutionData(verbose=FALSE, joinTables = TRUE)
  pd <- pd[pd$fips_N == 24510,]
}

message("aggregating by year/type")
emissionsum <- aggregate(pd$Emissions, by=list(pd$year, pd$type), FUN=sum)
names(emissionsum) <- c("Year", "Type", "Emissions")

# add in year over year column
temp <- split(emissionsum, emissionsum$Type)
temp <- lapply(temp, function(y) transform(y, new.col=c(NA,Emissions[-1]/Emissions[-nrow(y)]-1)))
emissionsum <- rbindlist(temp)
names(emissionsum)[4] <- "YoY"

# plot
myplot <- ggplot(emissionsum
             , aes(interaction(Year, Type), Emissions)) + 
          geom_bar(aes(fill=Type)
                   , width = 0.5
                   #, position = "dodge"
                   , stat="identity"
          ) + 
          coord_cartesian(ylim = c(0, 2500), expand = FALSE) +
          annotate(geom = "text", x = seq_len(nrow(emissionsum)), y = -.02 * max(emissionsum$Emissions), label = emissionsum$Year, size = 3) +
          annotate(geom = "text", x = 2.5 + 4 * (0:3), y = -.06 * max(emissionsum$Emissions), label = unique(emissionsum$Type), size = 3.5) +
          theme(panel.background = element_rect(fill="gray", color = "gray")) +
          theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
                axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()) +
          theme(legend.position = "none") +
          geom_text(aes(label = ifelse(is.na(emissionsum$YoY)
                                       , format(round(emissionsum$Emissions, 1), nsmall=1, big.mark=",")
                                       , ifelse(emissionsum$YoY >= 0
                                                , paste("+", percent(emissionsum$YoY), sep="")
                                                , percent(emissionsum$YoY)
                                                )
                                      )
                        , vjust=-1)
                    , size=2.5
                    , col = ifelse(is.na(emissionsum$YoY)
                                   , "black"
                                   , ifelse(emissionsum$YoY > 0, "darkred", "darkgreen")
                                  )
                    ) + 
          labs(y = "Emissions (tons)") +
          theme(plot.title = element_text(hjust = 0.5)) + 
          ggtitle("Emissions Changes For Baltimore City, MD By Type, 1999-2008")


# make a grob, turn off clipping
myplot2 <- ggplot_gtable(ggplot_build(myplot))
myplot2$layout$clip[myplot2$layout$name == "panel"] <- "off"
grid::grid.draw(myplot2)