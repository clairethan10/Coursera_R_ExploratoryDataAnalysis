# # This R program generates output for Coursera's Course Project 2 of Exploratory Data Analysis
# This program uses the base plotting system to answer the following question:
#
# Compare emissions from motor vehicle sources in Baltimore City with emissions
# from motor vehicle sources in Los Angeles County, California (
# fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

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

#### subset to los angeles and baltimore

twocities <- subset(merged, fips %in% c("24510", "06037"))

### label the cities

twocities$city <- twocities$fips

twocities$city <- factor(twocities$city,
                         levels = c("24510", "06037"),
                         labels = c("Los Angeles", "Baltimore"))


twocities %>% select(city, fips, year) %>% print()

### get aggregate total emission

total_emission_by_yr <- twocities %>%
  group_by(year, city) %>%
  summarize(Emissions = sum(Emissions)) %>%
  ungroup()

### plot the data to compare

png('plot6.png')
ggplot(total_emission_by_yr, aes(x=year, y=Emissions, color=city, group=city)) +
  geom_line() +
  geom_point() +
  labs(title = "Total emissions from mobile between Baltimore and Los Angeles over time",
       x = "year",
       y = "Total PM2.5 emissions in tons") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

dev.off()