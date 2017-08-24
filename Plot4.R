# ggplot2 for US coal combustion-related emissions changes by type year to year

library("scales")
library(data.table)
library(grid)
library("ggplot2")
source("DataLoader.R")

if(nrow(pd) != 53400){
  message("loading data")
  pd <- loadPollutionData(verbose=FALSE, joinTables = TRUE)
  pd <- pd[grepl("(C|c)oal", pd$Short.Name),]
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
  ggtitle("Coal Combustion-Related Emissions Changes For The U.S., 1999-2008")


# make a grob, turn off clipping
myplot2 <- ggplot_gtable(ggplot_build(myplot))
myplot2$layout$clip[myplot2$layout$name == "panel"] <- "off"
grid::grid.draw(myplot2)