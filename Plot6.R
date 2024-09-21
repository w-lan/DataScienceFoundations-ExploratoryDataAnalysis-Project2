# Compare emissions from motor vehicle sources in Baltimore City (fips == "24510")
# with emissions from motor vehicle sources in 
# Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?


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

# Subset Baltimore City and Los Angeles County fips codes 
# along with motor vehicle SCC from NEI data
vehicle_fltr <- grepl('vehicle', SCC[, SCC.Level.Two], ignore.case = TRUE)
vehicle_SCC <- SCC[vehicle_fltr, SCC]
cities_NEI <- NEI[(fips %in% c('24510', '06037')) & SCC %in% vehicle_SCC]

# Summarise: total vehicle emissions per year 
vehicle_emissions <- cities_NEI %>%
  group_by(year, fips) %>%
  summarise(Emissions = sum(Emissions, na.rm = TRUE))

g <-ggplot(vehicle_emissions, aes(x = year, y = Emissions, color = fips)) +
  geom_point() +    
  geom_smooth(method = "loess") +  
  labs(x = 'Year', y = expression('Total PM'[2.5]*' Emissions (Tonnes)'), color = 'City') +
  labs(title = expression('Total PM'[2.5]*' Vehicle Emissions in Baltimore City and LA County per Year')) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = label_number(accuracy = 1), 
                     limits = c(0, max(vehicle_emissions$Emissions) * 1.1), 
                     expand = c(0, 0)) +  # Start y-axis from 0
  scale_color_manual(values = c('24510' = 'blue', '06037' = 'green'), 
                     labels = c('Los Angeles County', 'Baltimore City')) +  # Assign custom colors and labels
  theme_bw()

ggsave(filename = "Plot6.png", plot = g, width = 7, height = 4, dpi = 300)