# This file performs the required ETL for calling scripts' plot needs

loadPollutionData <- function(startYear=NA
                     , endYear=NA
                     , clearenv=FALSE
                     , deleteDataFiles=FALSE
                     , countyFIPS=NA
                     , countyName=NA
                     , verbose=FALSE
                     )
{
  # includes
  library("lubridate")
  library("stringr")
  library("devtools")

  
  # Data used
  # summarySCC_PM25.rds             - actual pollution data (from class site zip)
  # Source_Classification_Code.rds  - pollution sources lookup/dimension table (from class site zip)
  # national_county.txt             - fips codes lookup table () - see bottom of this file for more info
    
  
  # clean environment if the user asked for it
  if(clearenv) {
    rm(list = ls())
    if(verbose) {message("Environment cleared")}
  }
  

  # download data if it isn't already in place
  if (!file.exists("summarySCC_PM25.rds") & !file.exists("Source_Classification_Code.rds")) {
    if (verbose) {message("pollution data files not found - downloading")}
    download <- download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", destfile = "temp.zip")
    unzip("temp.zip")
    unlink("temp.zip")
    if (verbose) {message("pollution data downloaded & unzipped")}
  }
  if (!file.exists("national_county.txt")) {
    if (verbose) {message("FIPS codes not found - downloading")}
    download <- download.file("https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", destfile = "national_county.txt")
    if (verbose) {message("FIPS data downloaded & unzipped")}
  }

  # initialize return list
  pollutiondata <- list(pol = NA, pol.source = NA, pol.fips = NA)
  
  
  # pollution fact table
  if (verbose) {message("loading pollutiondata$pol")}
  pollutiondata$pol <- readRDS("summarySCC_PM25.rds")
  if (!is.na(startYear) & !is.na(endYear) & startYear <= endYear) {
    pollutiondata$pol <- subset(pollutiondata$pol, year >= startYear & year <= endYear) 
  }
  pollutiondata$pol$type <- as.factor(pollutiondata$pol$type)
  pollutiondata$pol$SCC <- as.factor(pollutiondata$pol$SCC)
  #pollutiondata$pol$Pollutant <- as.factor(pollutiondata$pol$Pollutant)
  if (verbose) {message("pollutiondata$pol dataframe populated")}
  
  # pollution source lookup table
  if (verbose) {message("loading pollutiondata$pol.source")}
  pollutiondata$pol.source <- readRDS("Source_Classification_Code.rds")
  if (verbose) {message("pollution$pol.source dataframe populated")}
  
  # fips county codes lookup table
  if (verbose) {message("loading pollutiondata$pol.fips")}
  pollutiondata$pol.fips <- read.table("national_county.txt", sep=",", quote=NULL, comment="", header=FALSE)
  names(pollutiondata$pol.fips) <- c("State", "State_Code", "County_Code", "County", "Class")
  pollutiondata$pol.fips$FIPS_full <- paste(with_options(c(scipen = 999), str_pad(as.character(pollutiondata$pol.fips$State_Code), 2, pad = "0"))
                                            , with_options(c(scipen = 999), str_pad(as.character(pollutiondata$pol.fips$County_Code), 3, pad = "0"))
                                            , sep=""
                                            )
  if (verbose) {message("pollution$pol.fips dataframe populated")}
  
  
  # delete data files if the user requested
  if(deleteDataFiles) {
    if (verbose) {message("deleting data files")}
    file.remove("summarySCC_PM25.rds")
    file.remove("Source_Classification_Code.rds")
    file.remove("national_county.txt")
    if (verbose) {message("data files deleted")}
  }
  
  # returns list of 3 components:
  # pd$pol - main fact dataset
  # pd$pol.source - pollution sources lookup data
  # pd$pol.fips   - US county FIPS code lookup data
  # FINAL VERSION I MAY WISH TO PERFORM TABLE JOINS ALONG WITH FILTERING
  # AND RETURN ONLY 1 TABLE
  return(pollutiondata)
}


# FIPS code basic info, from https://www.census.gov/geo/reference/codes/cou.html
# 
# Field Name 	Field Description 	Example
# FIPS Class Codes
# H1:  identifies an active county or statistically equivalent entity that does not qualify under subclass C7 or H6.
# H4:  identifies a legally defined inactive or nonfunctioning county or statistically equivalent entity that does not qualify under subclass H6.
# H5:  identifies census areas in Alaska, a statistical county equivalent entity.
# H6:  identifies a county or statistically equivalent entity that is areally coextensive or governmentally consolidated with an incorporated place, part of an incorporated place, or a consolidated city.
# C7:  identifies an incorporated place that is an independent city; that is, it also serves as a county equivalent because it is not part of any county, and a minor civil division (MCD) equivalent because it is not part of any MCD.
# STATE 	State Postal Code 	FL
# STATEFP 	State FIPS Code 	12
# COUNTYFP 	County FIPS Code 	011
# COUNTYNAME 	County Name and Legal/Statistical Area Description 	Broward County
# CLASSFP 	FIPS Class Code 	H1
