# WDI & Flint

# Downloading the data
WDIurl <- "http://databank.worldbank.org/data/download/WDI_csv.zip"
download.file(WDIurl, "./WDI Data/WDI_csv.zip")
unzip("WDI_csv.zip", exdir = "WDI1 Data")
list.files("./WDI Data")
file.remove("./WDI Data/WDI_csv.zip")
WDIdata <- read.csv("WDI Data/WDI_Data.csv")
names(WDIdata)
head(WDIdata, 3)

##  Creating the 2014 water & income subset
CountryName <- WDIdata[,"Country.Name"]
CountryCode <- WDIdata[,"Country.Code"]
IndicatorName <- WDIdata[,"Indicator.Name"]
IndicatorCode <- WDIdata[,"Indicator.Code"]
Year2014 <- WDIdata[, "X2014"]
WDI2014 <- data.frame(col1=CountryName, col2=CountryCode,
                      col3=IndicatorName, col4 = IndicatorCode, col5=Year2014)
Names2014 <- c("Country", "Country_Code", "Indicator","Indicator_Code", "2014_Values")
colnames(WDI2014) <- Names2014
H2Odata14 <- WDI2014[grep("*water*", WDI2014$Indicator), ]
H2Odata14$Indicator
H2Odata <- H2Odata14[grep("^Improved*", H2Odata14$Indicator), ]
head(H2Odata, 3)

GDPdata14 <- WDI2014[grep("^GDP*", WDI2014$Indicator), ]
GDPdata <- GDPdata14[grep("*current*", GDPdata14$Indicator),]
GDPdata <- GDPdata[grep("*capita*", GDPdata$Indicator),]
GDPdata <- GDPdata[grep("US", GDPdata$Indicator),]
head(GDPdata, 3)

##  Melting with "reshape"
library(reshape2)
H2Omelt14 <- dcast(H2Odata, Country ~ Indicator_Code, value.var = '2014_Values')
head(H2Omelt14)
NamesH2O <- c("Country_or_Area", "H2O_Rural_Percent", "H2O_Urban_Percent", "H2O_Total_Percent")
colnames(H2Omelt14) <- NamesH2O

GDPmelt14 <- dcast(GDPdata, Country ~ Indicator_Code, value.var = '2014_Values')
head(GDPmelt14)
NamesGDP <- c("Country_or_Area", "GDP_per_capita_in_current_USD")
colnames(GDPmelt14) <- NamesGDP

Data14NAS <- merge(H2Omelt14, GDPmelt14, by = "Country_or_Area")
head(Data14NAS)
Data14 <- subset(Data14NAS, !is.na(Data14NAS$GDP_per_capita_in_current_USD))
head(Data14)
names(Data14)
shortColNames <- c("Country", "H2O_Rur", "H2O_Urb", "H2O_Tot", "GDP_Cap")
colnames(Data14) <- shortColNames

# Creating Total, Urban and Rural Data Subsets
Data14total <- Data14[, c(1, 4, 5)]
rownames(Data14total) <- Data14total[,1]
Data14total <- Data14total[,-1]
Data14total <- na.omit(Data14total)

Data14rural <- Data14[, c(1, 2, 5)]
rownames(Data14rural) <- Data14rural[,1]
Data14rural <- na.omit(Data14rural)
Data14rural <- Data14rural[,-1]

Data14urban <- Data14[, c(1, 3, 5)]
rownames(Data14urban) <- Data14urban[,1]
Data14urban <- na.omit(Data14urban)
Data14urban <- Data14urban[,-1]

# Plotting the data

png("H2Ohires.png", width=960, height=480)
par(mfrow=c(1,3))
with(Data14total, smoothScatter(H2O_Tot, log10(GDP_Cap),
                                colramp = colorRampPalette(c("white", blues9)),
                                xlab = "% of Total Population",
                                ylab = "GDP per capita log 10",
                                col = "green", pch = 21))
with(Data14urban, smoothScatter(H2O_Urb, log10(GDP_Cap),
                                colramp = colorRampPalette(c("white", blues9)),
                                xlab = "% of Urban Population",
                                ylab = "",
                                main = "National income (GDP per capita) and clean water access",
                                col = "yellow", pch = 21))
with(Data14rural, smoothScatter(H2O_Rur, log10(GDP_Cap),
                                colramp = colorRampPalette(c("white", blues9)),
                                xlab = "% of Rural Popluation",
                                ylab = "",
                                col = "orange", pch = 21))
dev.off()