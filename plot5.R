###############################################
## Problem statement to analyse 
# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
###############################################
## Analysis Approach 
# GGplot graphs have been drawn to 
# understand the trend of Total / mean and median PM2.5 Emission in the timeline in the city
###############################################
## Analysis Result 
# Baltimore shows a decresing trend
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
SCC_id <- SCC_tbl[grepl(" [Vv]eh",SCC_tbl$Short.Name)| grepl("[Mm]oto",SCC_tbl$Short.Name),]

##Subsetting based on Vehicle and Onroad Combustion related data 
table <- filter(NEI_tbl, fips %in% c("24510"), SCC %in% SCC_id$SCC) %>% group_by(year)  %>% summarise(sum(Emissions),mean(Emissions),median(Emissions))
names(table)[2] <- "TotalSumEmission"
names(table)[3] <- "MeanEmission"
names(table)[4] <- "MedianEmission"

# Initialize to PNG file 
png("plot5.png",width=10,height=10,units="in",res=1200)

#Total PM2.5 -Year Vise Distribution in Baltimore 
g <- ggplot(table,aes(x=table$year,y=table$TotalSumEmission))

Baltimore_Stats <- 
  g +  geom_line(aes(colour=table$TotalEmission,colour="TotalEmission"),method="lm") + 
  geom_line(aes(y=table$MedianEmission,colour="MedianEmission"),method="lm") + 
  geom_point(aes(y=table$MeanEmission,colour="MeanEmission"),method="lm") + 
  geom_smooth(aes(color=year),method="lm") +
  labs(x=expression("Year"), y= expression("PM2.5 Emissions")) + theme_grey() + labs(title ="Total PM2.5 -Year Vise Distribution in Baltimore.") + 
  scale_colour_manual("",breaks = c("TotalEmission", "MedianEmission", "MeanEmission"),values = c("red", "green", "blue")) 
print(ComparisonStats)
dev.off()