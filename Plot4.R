# Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999–2008?

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

# Subset coal / combustion from NEI data
combustion_fltr <- grepl('comb', SCC[, SCC.Level.One], ignore.case = TRUE)
coal_fltr <- grepl('coal', SCC[, SCC.Level.Four], ignore.case = TRUE)
coal_combustion_SCC <- SCC[combustion_fltr & coal_fltr, SCC]
coal_combustion_NEI <- NEI[NEI[, SCC] %in% coal_combustion_SCC]

# Summarise: total coal combustion emissions per year 
coal_combustion_emissions <- coal_combustion_NEI %>%
  group_by(year) %>%
  summarise(Emissions = sum(Emissions, na.rm = TRUE))

g <-ggplot(coal_combustion_emissions, aes(x = year, y = Emissions / 1e5)) +
  geom_point() +    
  geom_smooth(method = "loess") +  
  labs(x = 'Year', y = expression('Total PM'[2.5]*' Emissions (10^5 Tonnes)')) +
  labs(title = expression('Total PM'[2.5]*' Emissions per Year')) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = label_number(accuracy = 0.2), 
                     limits = c(0, max(coal_combustion_emissions$Emissions / 1e5) * 1.1), 
                     expand = c(0, 0)) +  # Start y-axis from 0
  theme_bw()
print(g)
# ggsave(filename = "Plot4.png", plot = g, width = 6, height = 4, dpi = 300)
