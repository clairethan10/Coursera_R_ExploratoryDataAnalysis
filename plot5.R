# # This R program generates output for Coursera's Course Project 2 of Exploratory Data Analysis
# This program uses the base plotting system to answer the following question:
#
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?


library(tidyverse)

neipath <- "exdata_data_NEI_data/Source_Classification_Code.rds"
sccpath <- "exdata_data_NEI_data/summarySCC_PM25.rds"

### load the data if exists

if (file.exists(neipath)){
  nei <- readRDS(neipath)
  message("File is successfully loaded")
  str(nei)
} else {
  warning("file is not found")
}

if (file.exists(sccpath)){
  SCC <- readRDS(sccpath)
  message("File is successfully loaded")
  str(SCC)
} else {
  warning("file is not found")
}


##### keep the dataset with the word Mobile in EI.Sector variable

library(stringr)

table(nei$EI.Sector)

mobile <- nei %>% filter(str_detect(EI.Sector, "Mobile"))

mobile %>% select(EI.Sector, SCC) %>% print()

##### merge two dataset between mobile data and scc

merged <- mobile %>% inner_join(SCC, by="SCC")

#### subset baltimore

baltimore <- subset(merged, fips=="24510")

### get aggregate total emission

total_emission_by_yr <- baltimore %>%
  group_by(year) %>%
  summarize(Emissions = sum(Emissions))


### plot the data to compare

png('plot5.png')

ggplot(total_emission_by_yr, aes(x=year, y=Emissions)) +
  geom_line() +
  geom_point() +
  labs(title = "Total emissions from vehicles in Baltimore over time",
       x = "year",
       y = "Total PM2.5 emissions in tons") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

dev.off()