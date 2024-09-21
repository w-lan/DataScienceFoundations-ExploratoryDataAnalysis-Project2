# Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to 
# make a plot answering this question.

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

NEI[, Emissions := lapply(.SD, as.numeric), .SDcols = c("Emissions")]  
total_NEI <- NEI[fips == '24510', 
                 lapply(.SD, sum, na.rm = TRUE), 
                 .SDcols = c('Emissions'), 
                 by = year]

png(filename='Plot2.png')

barplot(total_NEI[, Emissions], 
        names = total_NEI[, year], 
        main = expression('Total PM'[2.5]*' Emissions per Year in Baltimore City, Maryland'),
        xlab = 'Year', 
        ylab = expression('PM'[2.5]*' Emissions (tonnes)'))

dev.off()