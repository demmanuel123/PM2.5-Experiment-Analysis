
library(rJava)
library(data.table)
library(grDevices)
library(dplyr)
library(ggplot2)
library(labeling)
setwd("C://Personal//Coursera//CleaningData//EDA")

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Across the United States, how have emissions 
#from coal combustion-related sources changed from 1999-2008?


NEI_tbl <- tbl_df(NEI)
SCC_tbl <- tbl_df(SCC)


#searching for names starting with [Cc]oal [Cc]omb ( Coal Combustion) 
SCC_id <- SCC_tbl[grepl(" [Cc]oal",SCC_tbl$Short.Name)&grepl("[Cc]omb",SCC_tbl$Short.Name),]

##Subsetting based on Coal Combustion related data 
table <- filter(NEI_tbl,SCC %in% SCC_id$SCC) %>% group_by(year) 
# Initialize to PNG file 

png("plot4.png",width=10,height=10,units="in",res=1200)

##Plot of FIPS ( across US) Vs trend of Emissions . 
g <- ggplot(table,aes(table$fips,(table$Emissions)))
CoalCombStatistic <- 
  g+geom_point(aes(color=year),alpha=1/2) + facet_grid(year~.)+labs(x=expression("Country Code FIPS"), y= expression("PM2.5 Emissions")) + theme_bw()+ labs(title ="Country Code Vs PM2.5 Emissions per year")
print(CoalCombStatistic)
dev.off() #device off

## Plotting Sum of Total Emission vs year 
table <- table %>%  summarise(sum(Emissions))
names(table)[2] <- "TotalEmission"

png("plot4_1.png",width=10,height=10,units="in",res=1200)
##Plot of Total Emissions ( across US)  
g <- ggplot(table,aes(year,TotalEmission))
USWideTotalEmissions <- g +   geom_line() + labs(x=expression("Year"), y= expression("Total PM2.5 Emissions")) + theme_grey() + labs(title ="Year Vise -total PM2.5 Distribution")
print(USWideTotalEmissions)  
dev.off()
