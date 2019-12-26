rm(list=ls())
library(dplyr)
library(ggplot2)

#======================================Download dataset ========================================
file_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

# Create data folder 
if (!file.exists('data')){
  dir.create('data')
}

# Download the file if not exist
download.file(file_url, "data/epa.zip")
unzip('data/epa.zip',exdir = 'data/')

#==============================read files===================================================== 
source_classification_data <- readRDS("data/Source_Classification_Code.rds")
SCC_data <- readRDS("data/summarySCC_PM25.rds")

#==============================plot 1: PM25 in kilotons==============================================
total_emissions_by_year <- summarise(group_by(SCC_data, year), Emissions=sum(Emissions))
colors <- c("red", "green", "blue", "yellow")

png("plot1.png")

plt1 <- barplot(height=total_emissions_by_year$Emissions/1000, names=total_emissions_by_year$year, xlab = "years", ylab = "total PM25 emmission", 
                ylim=c(0,8000), main="Total PM25 emmission by kilotons", col = colors)

text(x = plt1, y = round(total_emissions_by_year$Emissions/1000,2), 
     label = round(total_emissions_by_year$Emissions/1000,2), pos = 3, cex = 0.8, col = "black")

dev.off()

#==============================plot 2: PM25 in Baltimore City kilotons==============================================
baltimore_city_emission <- summarise(group_by(filter(SCC_data, fips=="24510"), year), Emissions=sum(Emissions))
colors <- c("red", "green", "blue", "yellow")

png("plot2.png")
plt2 <- barplot(height=baltimore_city_emission$Emissions/1000, names=baltimore_city_emission$year, xlab = "years", ylab = "total PM25 emmission in Baltimore", 
                ylim=c(0,4), main="Total PM25 in Baltimore emmission by kilotons", col = colors)

text(x = plt2, y = round(baltimore_city_emission$Emissions/1000,2), 
     label = round(baltimore_city_emission$Emissions/1000,2), pos = 3, cex = 0.8, col = "black")
dev.off()

#==============================plot 3: PM25 in Baltimore City kilotons by types==============================================
baltimore_emission_by_year<-summarise(group_by(filter(SCC_data, fips == "24510"), year,type), Emissions=sum(Emissions))

png("plot3.png")
plt3 <- ggplot(baltimore_emission_by_year, aes(x=factor(year), y=Emissions, fill=type,label = round(Emissions,2)))
plt3 + geom_bar(stat="identity") + facet_grid(. ~ type) + xlab("year") + ylab(expression("total PM25 in baltimore City by kilotons")) +
  ggtitle("PM25 emission in Baltimore City by types") + geom_label(aes(fill = type), colour = "white", fontface = "bold")

dev.off()

#==============================plot 4: PM25 in US by coal==============================================
coal_source <- grepl("Fuel Comb.*Coal", source_classification_data$EI.Sector)
coal_source_data <- source_classification_data[coal_source, ]

coal_emission_data <- SCC_data[(SCC_data$SCC %in% coal_source_data$SCC), ]

coal_emission_data_by_year <- summarise(group_by(coal_emission_data, year), Emissions=sum(Emissions))

png("plot4.png")

plt4 <- ggplot(coal_emission_data_by_year, aes(x=factor(year), y=Emissions/1000,fill=year, label = round(Emissions/1000,2)))

plt4 + geom_bar(stat="identity") + xlab("year") + ylab(expression("PM25 emission in US by coal")) + ggtitle("Emissions from coal combustion-related sources in kilotons") + geom_label(aes(fill = year), colour = "white", fontface = "bold")
dev.off()


#==============================plot 5: PM25 in Baltimore by vehicle moto==============================================
baltimore_emission_by_vehicle <- SCC_data[(SCC_data$fips=="24510" & SCC_data$type=="ON-ROAD"), ]
baltimore_emission_by_vehicle_data <- summarise(group_by(baltimore_emission_by_vehicle, year), Emissions=sum(Emissions))

png("plot5.png")
plt5 <- ggplot(baltimore_emission_by_vehicle_data, aes(x=factor(year), y=Emissions,fill=year, label = round(Emissions,2)))

plt5 + geom_bar(stat="identity") + xlab("years") + ylab(expression("PM25 by tons")) + 
  ggtitle("Emissions from motor vehicle sources in Baltimore City") + geom_label(aes(fill = year),colour = "white", fontface = "bold")
dev.off()


#==============================plot 6: PM25 in Baltimore by motors with Los Angeles==============================================
baltimore_emission_by_motos <- summarise(group_by(SCC_data[(SCC_data$fips=="24510" & SCC_data$type=="ON-ROAD"), ], year), Emissions=sum(Emissions))
LA_emission_by_motos <- summarise(group_by(SCC_data[(SCC_data$fips=="06037" & SCC_data$type=="ON-ROAD"), ], year), Emissions=sum(Emissions))


baltimore_emission_by_motos$City <- "BALTIMORE_CITY"
LA_emission_by_motos$City <- "LOS_ANGELES_CITY"

both_cities_data <- rbind(baltimore_emission_by_motos, LA_emission_by_motos)

png("plot6.png")

plt6 <- ggplot(both_cities_data, aes(x=factor(year), y=Emissions, fill=City, label = round(Emissions,2)))
plt6 + geom_bar(stat="identity") + facet_grid(City~., scales="free") + ylab(expression("PM25 emissions in tons")) + xlab("years") +
  ggtitle("Motor vehicle emission in Baltimore and Los Angeles in tons") + geom_label(aes(fill = City),colour = "white", fontface = "bold")

dev.off()
