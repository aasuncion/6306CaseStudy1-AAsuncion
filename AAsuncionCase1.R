# Load required pacakges
library(plyr)

# Loading original source data
setwd("/Users/albertasuncion_mac/Documents/SMU/6306 - Intro to DS/rWD/")
fedstats <- read.csv(file="getdata%2Fdata%2FEDSTATS_Country.csv",header=TRUE)
gdp <- read.csv(file="getdata%2Fdata%2FGDP.csv",header=TRUE,skip=3)   # skipped to 4 line for header

# Examine dataframes and tidy data
dim(fedstats)
head(fedstats)

dim(gdp)
head(gdp)
gdp <- gdp[c("X","Ranking","Economy","US.dollars.")] # retain only columns with valid data
gdp <- rename(gdp, c("X" = "CountryCode", "US.dollars." = "GDP")) #use plyr package to rename variables
head(gdp)

## Question 1:Merge the datasets based upon the Country Code variable, count matching country codes
fedstats2 <- merge(fedstats, gdp, by="CountryCode")
dim(fedstats2)

## Question 2:Sort by GDP (ascending), display 13th country
fedstats2.sorted <- fedstats2[c("CountryCode", "GDP")]
fedstats2.sorted$GDP_new <- as.numeric(gsub(",","",fedstats2.sorted$GDP)) #converts the GDP field to numeric by first removing commas
arrange(fedstats2.sorted,GDP_new) #sorts the new numeric field
fedstats2.sorted [13,]  #displays the 13th in the order


