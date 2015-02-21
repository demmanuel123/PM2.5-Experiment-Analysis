###############################################
## Problem statement to analyse 
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
# from 1999 to 2008? Use the base plotting system to make a plot answering this question
###############################################
## Analysis Approach 
# Base plot graphs have been drawn to 
# understand the trend of Total PM2.5 Emission in the timeline in the city of Baltimore 
###############################################
## Analysis Result 
# There is a significant decrease seen in the total PM2.5 emission from 1999 to 2008. 
# Interestingly 3 years just before 2005 it had shown a increasing trend which was controlled
# and brought back to control. 
###############################################

library(rJava)
library(data.table)
library(grDevices)
library(dplyr)
setwd("C://Personal//Coursera//CleaningData//EDA")

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


NEI_tbl <- tbl_df(NEI)
SCC_tbl <- tbl_df(SCC)

## Subsetting to Baltimore specific data , Summarizing the Sum of emissions per year
table_baltimore <- filter(NEI_tbl,fips == "24510") %>% group_by(year) %>% summarise(sum(Emissions))
names(table_baltimore)[2] <- "TotalEmission"


# Initialize to PNG file 
png("plot2.png",width=10,height=10,units="in",res=1200)
# Plot as a line  with X axis as Year and Y as Sum

plot( table_baltimore$year, table_baltimore$TotalEmission ,
      ,ylab="Total Emission "
      ,xlab=" Year"
      ,type="l"
      ,col="black"
      ,xlim=c(1998, 2010))
points( table_baltimore$year
        ,table_baltimore$TotalEmission
        ,col="red")
#axis(1, dm$Date, format(dm$Date, "%b %d"), cex.axis = .7)
title(main = "Decrease seen in total emission value from \n 1999 to 2008 in Baltimore")
#xyplot()

dev.off() #device off