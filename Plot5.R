# ggplot2 for baltmore city county, MD motor vehicle sources emissions changes year to year

library("scales")
library(data.table)
library(grid)
library("ggplot2")
source("DataLoader.R")

if(nrow(pd) != -1){
  message("loading data")
  pd <- loadPollutionData(verbose=FALSE, joinTables = TRUE)
  pd <- pd[grepl("(V|v)ehicle", pd$SCC.Level.Two),]
  pd <- pd[pd$fips == "24510",]
}

message("aggregating by year")
emissionsum <- aggregate(pd$Emissions, by=list(pd$year), FUN=sum)
names(emissionsum) <- c("Year", "Emissions")

# add in year over year column
message("calculating year over year column")
emissionsum <- transform(emissionsum, new.col=c(NA,Emissions[-1]/Emissions[-nrow(emissionsum)]-1))
names(emissionsum)[3] <- "YoY"

# plot
message("plotting")
myplot <- ggplot(emissionsum
                 , aes(Year, Emissions)) + 
  geom_bar(width = 1
           , position = "dodge"
           , stat="identity"
           , color="lightgoldenrod4"
           , fill="lightgoldenrod4"
  ) + 
  scale_x_continuous(breaks = emissionsum$Year, 
                     labels = emissionsum$Year, 
                     limits = c(1998,2009)) + 
  scale_y_continuous(label= function(x) {ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scientific_format()(x)))))} ) +
  theme(panel.background = element_rect(fill="gray", color = "gray")) +
  theme(legend.position = "none") +
  geom_text(aes(label = ifelse(is.na(emissionsum$YoY)
                               , paste(format(round(emissionsum$Emissions, 1), nsmall=1, big.mark=","), " tons", sep="")
                               , ifelse(emissionsum$YoY >= 0
                                        , paste("+", round((emissionsum$YoY)*100,digits=1), "%", sep="")
                                        , paste(round((emissionsum$YoY)*100,digits=1), "%", sep="")
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
  ggtitle("Motor Vehicle Emissions Changes For Baltimore City County, MD, 1999-2008")


# make a grob, turn off clipping
myplot2 <- ggplot_gtable(ggplot_build(myplot))
myplot2$layout$clip[myplot2$layout$name == "panel"] <- "off"
grid::grid.draw(myplot2)