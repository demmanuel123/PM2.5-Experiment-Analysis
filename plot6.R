###############################################
## Problem statement to analyse 
# Compare emissions from motor vehicle sources in Baltimore City with emissions
# from motor vehicle sources in Los Angeles County, California (fips == "06037").
# Which city has seen greater changes over time in motor vehicle emissions?
###############################################
## Analysis Approach 
# GGplot graphs have been drawn to 
# understand the trend of Total PM2.5 Emission in the timeline in the 2 cities
###############################################
## Analysis Result 
#California shows a slight increase while Baltimore shows a marginal decrese
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

#searching for names starting with [Cc]Vehicle and [Oo]nroad
SCC_id <- SCC_tbl[grepl(" [Vv]eh",SCC_tbl$Short.Name)| grepl("[On]nroad",SCC_tbl$Short.Name),]


table <- filter(NEI_tbl,fips %in% c("24510","06037") & SCC %in% SCC_id$SCC) %>% group_by(year,fips) %>% summarise(sum(Emissions))
names(table)[2] <- "FIPS"
names(table)[3] <- "TotalEmission"

# Initialize to PNG file 
png("plot6.png",width=10,height=10,units="in",res=1200)

#Total PM2.5 -Year Vise Distribution in Baltimore and California.
ComparisonStats <- ggplot(table,aes(year,TotalEmission)) +
  geom_point(aes(color=FIPS),alpha=1/2,size=2) + labs(x=expression("Year"), y= expression("PM2.5 Emissions")) + theme_grey() + labs(title ="Total PM2.5 -Year Vise Distribution in \n Baltimore and California.California shows a slight increase \nBaltimore shows a marginal decrese") + geom_smooth(aes(color=FIPS),method="lm")
print(ComparisonStats)
dev.off()