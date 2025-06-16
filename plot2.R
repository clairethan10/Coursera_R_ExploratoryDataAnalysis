# This R program generates output for Coursera's Course Project 2 of
# Exploratory Data Analysis
# This program uses the base plotting system to answer the following question:
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510")
#from 1999 to 2008? Use the base plotting system to make a plot answering this question.


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

# have total emission of PM2.5 decreased in the US from 1999 to 2008 ?
# subset the data to Baltimore
Baltimore <- subset(SCC, fips=="24510")

#aggregate emission by year
total_emission_by_yr <- aggregate(Emissions ~ year, data=Baltimore, sum)

# open an pgn file

png("plot2.png", width=800, height=600)

#create an R plot to display total emission by year

with(total_emission_by_yr, {
  plot(year, Emissions, type="b",
       xlab="Year",
       ylab="Total PM2.5 Emissionns in tons",
       main="Total PM 2.5 emissions in Baltimore FIPs=24510 by year",
       col="blue", pch=19, las=0,
       xaxt="n", yaxt="n", # turn off default axis labels
       ylim = c(0,max(Emissions) * 1.1))
}
)

# label x axis tick marks
axis(1, at=total_emission_by_yr$year, labels = total_emission_by_yr$year, las=0)

#label y axis tick marks
pretty_ylim <- pretty(total_emission_by_yr$Emissons)
formatted_labels <- prettyNum(pretty_ylim, big.mark = ",", scientific = FALSE)
axis(2,at=pretty_ylim, labels=formatted_labels, las=2)

# add grid
grid()

dev.off()