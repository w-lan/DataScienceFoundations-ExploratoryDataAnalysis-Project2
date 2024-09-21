# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
# variable, which of these four sources have seen decreases in emissions 
# from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

# Load packages
library(ggplot2)
library(dplyr)
library(scales)

# Retrieve data
setwd('~/Documents/Software/R/Data Science Foundations Using R/Exploratory Data Analysis/Project2')
path <- getwd()
url <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
file_path <- file.path(path, 'data.zip', sep = '/')

# Download and unzip only if necessary
if(!file.exists(file.path(path, 'Source_Classification_Code.rds')) & 
   !file.exists(file.path(path, 'summarySCC_PM25.rds'))) {
  download.file(url, file_path)
  unzip(zipfile = file_path, exdir = path)
}

SCC <- data.table::as.data.table(x = readRDS(file = 'Source_Classification_Code.rds'))
NEI <- data.table::as.data.table(x = readRDS(file = 'summarySCC_PM25.rds'))

# Baltimore City, Maryland
Baltimore_NEI <- NEI[fips == '24510', 
                     Emissions := lapply(.SD, as.numeric, na.rm = TRUE), 
                     .SDcols = c("Emissions"), 
                     by = year]  

# Summarise: total emissions per year and type
Baltimore_total_emissions <- Baltimore_NEI %>%
  group_by(year, type) %>%
  summarise(Emissions = sum(Emissions, na.rm = TRUE))

g <-ggplot(Baltimore_total_emissions, aes(x = year, y = Emissions / 1e6)) +
    geom_point() +    
    geom_smooth(method = "loess") +  
    facet_grid(.~type) +             # Separate by source type
    labs(x = 'Year', y = expression('Total PM'[2.5]*' Emissions (Million Tonnes)')) +
    labs(title = expression('Total PM'[2.5]*' Emissions in Baltimore City (1999-2008) by Source Type')) +
    scale_y_continuous(labels = label_number(accuracy = 0.1)) + 
    theme_bw()

ggsave(filename = "Plot3.png", plot = g, width = 7, height = 4, dpi = 300)