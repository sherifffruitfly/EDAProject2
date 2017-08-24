# ggplot2 for baltmore city county, MD motor vehicle sources emissions changes year to year

library("scales")
library(data.table)
library(grid)
library("ggplot2")
source("DataLoader.R")

if(nrow(pd) != 2721){
  message("loading data")
  pd <- loadPollutionData(verbose=FALSE, joinTables = TRUE)
  pd <- pd[grepl("(V|v)ehicle", pd$SCC.Level.Two),]
  pd <- pd[pd$fips == "24510" | pd$fips == "06037",]
}

message("aggregating by year/county")
emissionsum <- aggregate(pd$Emissions, by=list(pd$year, pd$County), FUN=sum)
names(emissionsum) <- c("Year", "County", "Emissions")

# add in year over year column
message("calculating year over year column")

temp <- split(emissionsum, emissionsum$County)
temp <- Filter(function(x) dim(x)[1] > 0, temp)
temp <- lapply(temp, function(y) transform(y, new.col=c(NA,Emissions[-1]/Emissions[1]-1)))
emissionsum <- rbindlist(temp)
names(emissionsum)[4] <- "YoY"

# plot
message("plotting")
myplot <- ggplot(emissionsum
                 , aes(interaction(Year, County), Emissions)) + 
  geom_bar(aes(fill=County)
           , width = 0.5
           #, position = "dodge"
           , stat="identity"
  ) + 
  coord_cartesian(ylim = c(0, 1.1*max(emissionsum$Emissions)), expand = FALSE) +
  annotate(geom = "text", x = seq_len(nrow(emissionsum)), y = -.02 * max(emissionsum$Emissions), label = emissionsum$Year, size = 3) +
  annotate(geom = "text", x = 2.5 + 4 * (0:(length(unique(emissionsum$County))-1)), y = -.06 * max(emissionsum$Emissions), label = unique(emissionsum$County), size = 3.5) +
  scale_y_continuous(label= function(x) {ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scientific_format()(x)))))} ) +
  theme(panel.background = element_rect(fill="gray", color = "gray")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  theme(legend.position = "none") +
  geom_text(aes(label = ifelse(is.na(emissionsum$YoY)
                               , paste(format(round(emissionsum$Emissions, 1), nsmall=1, big.mark=","), " tons", sep="")
                               , ifelse(emissionsum$YoY >= 0
                                        , paste("+", round((emissionsum$YoY)*100,digits=1), "%", sep="")
                                        , paste(round((emissionsum$YoY)*100,digits=1), "%", sep="")
                                        )
                              )
                , vjust=-1
                )
            , size=2.5
            , col = ifelse(is.na(emissionsum$YoY)
                           , "black"
                           , ifelse(emissionsum$YoY > 0, "darkred", "darkgreen")
            )
            ) + 
  labs(y = "Emissions (tons)") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Change In Motor Vehicle Emissions: \nBaltimore City County vs Los Angeles County, 1999-2008") +
  annotate(geom = "text"
           , x = 4.5
           , y = -.15 * max(emissionsum$Emissions)
           , label = "(Red/green bar labels indicate *cumulative* % change)"
           , size = 3
           )
  
# make a grob, turn off clipping
myplot2 <- ggplot_gtable(ggplot_build(myplot))
myplot2$layout$clip[myplot2$layout$name == "panel"] <- "off"
grid::grid.draw(myplot2)