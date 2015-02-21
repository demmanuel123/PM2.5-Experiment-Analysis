###############################################
## Problem statement to analyse 
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
# which of these four sources have seen decreases in emissions from 1999-2008 
# for Baltimore City? Which have seen increases in emissions from 1999-2008?
# Use the ggplot2 plotting system to make a plot answer this question.
###############################################
## Analysis Approach 
# GGplot graphs have been drawn to 
# understand the trend of Total PM2.5 Emission in the timeline and category vise
###############################################
## Analysis Result 
# Non Road and Non Point sources of emission have shown drastic decrease
# On Road source of emission has shown only slight decrease
# Point source of emission has shown slight increase 
###############################################


library(rJava)
library(data.table)
library(grDevices)
library(dplyr)
library(ggplot2)
library(labeling)

## Ensure to set right working directory setwd("<>")

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


NEI_tbl <- tbl_df(NEI)
SCC_tbl <- tbl_df(SCC)

#Filter by Baltimore FIPS Code
NEI_tbl_Baltimore <- filter(NEI_tbl,fips == "24510") %>%   group_by(year,type) %>% summarise(sum(Emissions))
names(NEI_tbl_Baltimore)[3] <- "TotalEmission"

png("plot3.png",width=10,height=10,units="in",res=1200)

##Plotting total values : Plot 
BaltimoreTotalStats <- ggplot(NEI_tbl_Baltimore,aes(year,TotalEmission)) +
  geom_line(aes(color=type),alpha=1/2,size=2) + facet_grid(type ~.) +labs(x=expression("Year"), y= expression("PM2.5 Emissions")) + theme_grey() + labs(title ="Year Vise , total PM2.5 Distribution in Baltimore")
print(BaltimoreTotalStats)
dev.off()

###############################################
#### Secondary graph : For cross verifying the results 
###############################################

NEI_tbl_Baltimore <- filter(NEI_tbl,fips == "24510") %>%   group_by(year,type) 
png("plot3.png",width=10,height=10,units="in",res=1200) # Initialize to PNG file 

## ggplot 2 Graph plot3.png - This is a secondary graph , it extra="error"shows the scatter plot across timeline based on the 4 'type' variables
Baltimore_TypeVise_Statistics <- 
  ggplot(NEI_tbl_Baltimore,aes(year,Emissions)) +
  geom_line(aes(color=type),alpha=1/2,size=2) + facet_grid(type ~.) +labs(x=expression("Year"), y= expression("PM2.5 Emissions")) + theme_grey() + labs(title ="Year Vise , Category Vise PM2.5 Distribution")

print(Baltimore_TypeVise_Statistics) # Save to PNG file
dev.off()

###############################################