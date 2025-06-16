# This R program generates output for Coursera's Course Project 2 of
# Exploratory Data Analysis
# This program uses the base plotting system to answer the following question:
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.


# load ggplot2
library(tidyverse)

# set file path

neipath <- "exdata_data_NEI_data/Source_Classification_Code.rds"
sccpath <- "exdata_data_NEI_data/summarySCC_PM25.rds"


# check if file exists then load

if (file.exists(sccpath)) {
  SCC <- readRDS(sccpath)
  message("file loaded successfully!")
  str(SCC)
} else {
  warning("File does not exists in ", sccpath )
}


if (file.exists(neipath)) {
  nei <- readRDS(neipath)
  message("file loaded successfully!")
  str(nei)
} else {
  warning("File does not exists in ", neipath )
}

# subset to Baltimore data
baltimore <- SCC %>% filter(fips=="24510")

# aggregate the total emissions
total_emission_by_yr <- baltimore %>%
  group_by(year, type) %>%
  summarize(Emissions = sum(Emissions)) %>%
  ungroup() # this removes grouping structure after summarizing

png("plot3.png")

# create plot using ggplot
ggplot(total_emission_by_yr, aes(x = year, y = Emissions, color = type, group = type)) +
  geom_line() + # Add lines for each type
  geom_point() + # Add points for each year/type combination
  labs(title = "Total PM2.5 Emissions in Baltimore City by Type (1999-2008)",
       x = "Year",
       y = "Total PM2.5 Emissions (tons)") +
  scale_y_continuous(labels = scales::comma) + # Format y-axis with commas
  theme_minimal() # Use a minimal theme for better aesthetics

dev.off()