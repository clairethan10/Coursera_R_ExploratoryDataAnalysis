# This R program generates output for Coursera's Course Project 2 of
# Exploratory Data Analysis
# This program uses the base plotting system to answer the following question:
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

library(tidyverse)

neipath <- "exdata_data_NEI_data/Source_Classification_Code.rds"
sccpath <- "exdata_data_NEI_data/summarySCC_PM25.rds"

# load data after checking if files exist

if (file.exists(sccpath)) {
  SCC <- readRDS(sccpath)
  message("file loaded successfully")
  str(SCC)
} else {
  warning("file is not found")
}

if (file.exists(neipath)) {
  nei <- readRDS(neipath)
  message("File loaded successfully")
  str(nei)
} else {
  warning("file is not found")
}

# find the word coal in EI.Sector

library(stringr)

coal <- nei %>% filter(str_detect(EI.Sector, "Coal"))

coal %>% select(EI.Sector, SCC) %>% print()

# print using ggplot to see if these combined coal related sources change over time

# merge two data using inner join
merged <- coal %>% inner_join(SCC, by="SCC")


# aggregate the total emissions
total_emission_by_yr <- merged %>%
  group_by(year) %>%
  summarize(Emissions = sum(Emissions))

png("plot4.png")

# create plot using ggplot
ggplot(total_emission_by_yr, aes(x = year, y = Emissions)) +
  geom_line() + # Add lines for each type
  geom_point() + # Add points for each year/type combination
  labs(title = "Total emissions from coal combustion-related sources",
       x = "Year",
       y = "Total PM2.5 Emissions (tons)") +
  scale_y_continuous(labels = scales::comma) + # Format y-axis with commas
  theme_minimal() # Use a minimal theme for better aesthetics

dev.off()