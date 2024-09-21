# How have emissions from motor vehicle sources changed from 1999–2008 in 
# Baltimore City?


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

# Subset Baltimore City fips code and motor vehicle SCC from NEI data
vehicle_fltr <- grepl('vehicle', SCC[, SCC.Level.Two], ignore.case = TRUE)
vehicle_SCC <- SCC[vehicle_fltr, SCC]
vehicle_NEI <- NEI[fips == '24510' & SCC %in% vehicle_SCC]

# Summarise: total vehicle emissions per year 
vehicle_emissions <- vehicle_NEI %>%
  group_by(year) %>%
  summarise(Emissions = sum(Emissions, na.rm = TRUE))

g <-ggplot(vehicle_emissions, aes(x = year, y = Emissions)) +
  geom_point(color = 'blue') +    
  geom_smooth(method = 'loess', color = 'blue') +  
  labs(x = 'Year', y = expression('Total PM'[2.5]*' Emissions (Tonnes)')) +
  labs(title = expression('Total PM'[2.5]*' Vehicle Emissions in Baltimore City per Year')) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = label_number(accuracy = 1), 
                     limits = c(0, max(vehicle_emissions$Emissions) * 1.1), 
                     expand = c(0, 0)) +  # Start y-axis from 0
  theme_bw()

ggsave(filename = "Plot5.png", plot = g, width = 6, height = 4, dpi = 300)