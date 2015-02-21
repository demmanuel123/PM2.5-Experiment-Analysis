###############################################
## Problem statement to analyse 
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all
# sources for each of the years 1999, 2002, 2005, and 2008.
###############################################
## Analysis Approach 
# Base graphs have been drawn to 
# understand the trend of Total PM2.5 Emission in the timeline 
###############################################
## Analysis Result 
# PM 2.5 emission has seen decrease in total value from 1999 to 2008
###############################################

library(rJava)
library(data.table)
library(grDevices)
library(dplyr)
## Ensure to set right working directory setwd("<>")

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEI_tbl <- tbl_df(NEI)
SCC_tbl <- tbl_df(SCC)

## Group by year and find the total Emission and Mean Emission in a year. 
## Mean emission is just for analysing the trend , its not added to this plot

## Summarize sum and mean data for Emissions by year
table <- group_by(NEI_tbl,year) %>% summarise(sum(Emissions),mean(Emissions))
names(table)[2] <- "TotalEmission"
names(table)[3] <- "MeanEmission"

# Initialize to PNG file 
png("plot1.png",width=10,height=10,units="in",res=1200)
# Plot as a line  with X axis as Year and Y as Sum
par(mfrow=c(1,1))

with(table, {
plot( table$year, table$TotalEmission ,
      ,ylab="Total Emission "
      ,xlab=" Year"
      ,type="l"
      ,col="black"
      ,xlim=c(1998, 2010)
)
points( table$year
       ,table$TotalEmission
       ,col="red")

})

title(main = "Decreasing trend seen in total emission value \n from 1999 to 2008")


#plot(table$year,table$MeanEmission)

dev.off() #device off