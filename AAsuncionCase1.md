Introduction:

Horace Mann once said that "education ... beyond all other devices of human origin, is a great equalizer of the conditions of men". The Organization for Economic Co-operation and Development (OECD) holds as its mission the promotion of policies that will improve the economic and social well-being of people around the world.

In this study we want to find the correlation between GDP and the relative income levels of educators as an indicator of the importance education. Do weatlhier economies place a higher importance on education? Is the reverse true as well, where poorer countries are forced to perhaps put education as a lower priority, thus perpetuating the conditions of poorer states.

This research was developed in R and available in Github.

Load Required Packages

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(scales)
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(gtools)
library(data.table)
```

    ## -------------------------------------------------------------------------

    ## data.table + dplyr code now lives in dtplyr.
    ## Please library(dtplyr)!

    ## -------------------------------------------------------------------------

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

Load and Review Data

``` r
##Set working directory, load data into dataframes
setwd("/Users/albertasuncion_mac/Documents/SMU/6306 - Intro to DS/rWD/")
fedstats <- read.csv(file="getdata%2Fdata%2FEDSTATS_Country.csv",header=TRUE)
##Skipping 2 empty rows from original file
gdp <- read.csv(file="getdata%2Fdata%2FGDP.csv",header=TRUE,skip=3)
## Remove NA columns
gdp <- gdp[-c(3,6:10)]
## Rename columns, 1st column in particular in order to prepare for dta merge
names(gdp) <- c("CountryCode","Ranking","Country","GDP")
```

``` r
head(gdp)
```

``` r
head(fedstats)
```

Merge data based on the country shortcode

``` r
df_merged <- merge(fedstats, gdp, by="CountryCode")
head(df_merged)
```

Answers to Questions:

1.  Number of matching IDs - After performing a merge on the CountryCode column, a total of 224 rows were created in the dataframe df\_merged. This was arrived at by executing a dim() step as shown here.

``` r
dim(df_merged)
```

    ## [1] 224  34

1.  Sort on GDP, ascending order and find the 13th record - A new dataframe, df\_merged.sorted, is created from df\_merged. Transform the Ranking column to numeric using a new column Ranking2. Based on the Ranking2 column, create a subset for only those records with valid Rankings. To prepare for sorting GDP, the column needed to be converted to numeric and assigned to a new column called df\_merged.sorted$GDP2. The conversion involved removing the "," from the values using gsub() and converting to a numeric class using as.numneric. The dataframe is then sorted using arrange() from the "dplyr" package.

``` r
#Transform Ranking column to numeric
df_merged$Ranking2 <- as.numeric(as.character(df_merged$Ranking))

#Create a new dataframe for records with valid rankings
df_merged.sorted <- subset(df_merged, df_merged$Ranking2>0)
df_merged.sorted$GDP2 <- as.numeric(gsub(",","",df_merged.sorted$GDP))
df_merged.sorted <- arrange(df_merged.sorted,GDP2)
```

The 13th ranked country in terms of GDP is KNA, St. Kitts and Nevis, found by referencing row index 13 of df\_merged.sorted.

``` r
df_merged.sorted[13,1:2]
```

    ##    CountryCode           Long.Name
    ## 13         KNA St. Kitts and Nevis

1.  Average GDP rankings of High income: OECD and High income nonOECD

{r RankingNumeric} \#converting the df\_merged.sorted*R**a**n**k**i**n**g**c**o**l**u**m**n**t**o**n**u**m**e**r**i**c**a**n**d**c**r**e**a**t**i**n**g**a**n**e**w**v**a**r**i**a**b**l**e**R**a**n**k**i**n**g*2*f**o**r**t**h**e**t**r**a**n**s**f**o**r**m**e**d**c**o**l**u**m**n**d**f*<sub>*m*</sub>*e**r**g**e**d*.*s**o**r**t**e**d*Ranking2 &lt;- as.numeric(as.character(df\_merged.sorted$Ranking))

Using the numeric column Ranking2, 2 1-column vectors are created using the subset function to create the "High income: OECD" and the "High income: nonOECD" groups. The means were taken of the df\_merged.sorted$Ranking2 column to arrive at the respective means. The mean ranks are:

High Income OECD: 32.97

High Income nonOECD: 91.91

``` r
#create vectors for "High income: OECD" and "High income: nonOECD" groups
HighIncomeOECD <- subset(df_merged.sorted, df_merged.sorted$Income.Group == "High income: OECD")
HighIncomeNonOECD <- subset(df_merged.sorted, df_merged.sorted$Income.Group == "High income: nonOECD")
#calculate the means of the HighIncomeOECD and HighIncomeNonOECD vectors, disregarding NA
Avg_HighIncomeOECD <- mean(HighIncomeOECD$Ranking2, na.rm=TRUE)
Avg_HighIncomeNonOECD <- mean(HighIncomeNonOECD$Ranking2, na.rm=TRUE)
Avg_HighIncomeOECD
```

    ## [1] 32.96667

``` r
Avg_HighIncomeNonOECD
```

    ## [1] 91.91304

1.  Plot of GDP by Income Group -

``` r
a <- ggplot(subset(df_merged.sorted, Income.Group=="High income: OECD"), aes(y=reorder(CountryCode,GDP2),x=GDP2)) + geom_point(aes(color=Income.Group)) + geom_point(colour="tomato") + theme(axis.text=element_text(size=6)) + scale_x_continuous(name="GDP", labels=dollar) + ylab("Country") + theme(legend.position = "none") + ggtitle("High Income: OECD") + theme(plot.title = element_text(size = 10, face="bold"))
b <- ggplot(subset(df_merged.sorted, Income.Group=="High income: nonOECD"), aes(y=reorder(CountryCode,GDP2),x=GDP2)) + geom_point(aes(color=Income.Group)) + geom_point(colour="steelblue") + theme(axis.text=element_text(size=6)) + scale_x_continuous(name="GDP", labels=dollar) + ylab("Country") + theme(legend.position = "none") + ggtitle("High Income: nonOECD") + theme(plot.title = element_text(size = 10, face="bold"))
c <- ggplot(subset(df_merged.sorted, Income.Group=="Upper middle income"), aes(y=reorder(CountryCode,GDP2),x=GDP2)) + geom_point(aes(color=Income.Group)) + geom_point(colour="seagreen") + theme(axis.text=element_text(size=6)) + scale_x_continuous(name="GDP", labels=dollar) + ylab("Country") + theme(legend.position = "none") + ggtitle("Upper Middle Income") + theme(plot.title = element_text(size = 10, face="bold"))
d <- ggplot(subset(df_merged.sorted, Income.Group=="Lower middle income"), aes(y=reorder(CountryCode,GDP2),x=GDP2)) + geom_point(aes(color=Income.Group)) + geom_point(colour="slateblue") + theme(axis.text=element_text(size=6)) + scale_x_continuous(name="GDP", labels=dollar) + ylab("Country") + theme(legend.position = "none") + ggtitle("Lower Middle Income") + theme(plot.title = element_text(size = 10, face="bold"))
e <- ggplot(subset(df_merged.sorted, Income.Group=="Low income"), aes(y=reorder(CountryCode,GDP2),x=GDP2)) + geom_point(aes(color=Income.Group)) + geom_point(colour="sienna") + theme(axis.text=element_text(size=6)) + scale_x_continuous(name="GDP", labels=dollar) + ylab("Country") + theme(legend.position = "none") + ggtitle("Low Income") + theme(plot.title = element_text(size = 10, face="bold"))
grid.arrange(arrangeGrob(a, b, c, ncol=3, heights = 20))
```

![](AAsuncionCase1_files/figure-markdown_github/Plot2-1.png)

``` r
grid.arrange(arrangeGrob(d, e , ncol=3, heights = 20))
```

![](AAsuncionCase1_files/figure-markdown_github/unnamed-chunk-2-1.png)

1.  Summary statistics of GDP by Income Group

``` r
#Use the sorted data frame
#Filter records where GDP is NA

df_merged.sorted.nonNAGDP <- df_merged.sorted %>% filter(!is.na(GDP2))

#Create a grouping by Income Group

df_merged.sorted.nonNAGDP.grouped <- group_by(df_merged.sorted.nonNAGDP,Income.Group)

#Summarize GDP by Income Group, and summarize basic statistics

options(dplyr.width=Inf) #options forces all columns to appear in the output
summarise(df_merged.sorted.nonNAGDP.grouped,countries=n(),totGDP=dollar(sum(GDP2,na.rm=TRUE)),minGDP=dollar(min(GDP2,na.rm=TRUE)),maxGDP=dollar(max(GDP2,na.rm=TRUE)),meanGDP=dollar(mean(GDP2, na.rm=TRUE)),stddevGDP=dollar(sd(GDP2,na.rm=TRUE)))
```

    ## # A tibble: 5 × 7
    ##           Income.Group countries      totGDP  minGDP      maxGDP
    ##                 <fctr>     <int>       <chr>   <chr>       <chr>
    ## 1 High income: nonOECD        23  $2,400,046  $2,584    $711,050
    ## 2    High income: OECD        30 $44,517,514 $13,579 $16,244,600
    ## 3           Low income        37    $533,199    $596    $116,355
    ## 4  Lower middle income        54 $13,859,828     $40  $8,227,103
    ## 5  Upper middle income        45 $10,433,153    $228  $2,252,664
    ##      meanGDP  stddevGDP
    ##        <chr>      <chr>
    ## 1   $104,350   $165,334
    ## 2 $1,483,917 $3,070,464
    ## 3 $14,410.78 $20,473.09
    ## 4   $256,663 $1,139,620
    ## 5   $231,848   $476,872

1.  Table of income groups showing number of countries per quartile (5)

``` r
#Add a column for the quartile and create a dafaframe with the quartile column

df_merged.sorted.quartile <- setDT(df_merged.sorted)[, quartile := cut(df_merged.sorted$Ranking2, quantile(df_merged.sorted$Ranking2, probs=0:5/5), include.lowest=TRUE, labels=FALSE)]

#Create a grouping by Income Group + Quartile

df_merged.sorted.quartile.grouped <- group_by(df_merged.sorted.quartile,Income.Group,quartile)

#Aggregate number of countries

CountriesByIncomeQuartile <- aggregate(data=df_merged.sorted.quartile, df_merged.sorted.quartile$GDP2 ~ df_merged.sorted.quartile$Income.Group + df_merged.sorted.quartile$quartile, FUN=length)
CountriesByIncomeQuartile
```

    ##    df_merged.sorted.quartile$Income.Group
    ## 1                    High income: nonOECD
    ## 2                       High income: OECD
    ## 3                     Lower middle income
    ## 4                     Upper middle income
    ## 5                    High income: nonOECD
    ## 6                       High income: OECD
    ## 7                              Low income
    ## 8                     Lower middle income
    ## 9                     Upper middle income
    ## 10                   High income: nonOECD
    ## 11                      High income: OECD
    ## 12                             Low income
    ## 13                    Lower middle income
    ## 14                    Upper middle income
    ## 15                   High income: nonOECD
    ## 16                      High income: OECD
    ## 17                             Low income
    ## 18                    Lower middle income
    ## 19                    Upper middle income
    ## 20                   High income: nonOECD
    ## 21                             Low income
    ## 22                    Lower middle income
    ## 23                    Upper middle income
    ##    df_merged.sorted.quartile$quartile df_merged.sorted.quartile$GDP2
    ## 1                                   1                              4
    ## 2                                   1                             18
    ## 3                                   1                              5
    ## 4                                   1                             11
    ## 5                                   2                              5
    ## 6                                   2                             10
    ## 7                                   2                              1
    ## 8                                   2                             13
    ## 9                                   2                              9
    ## 10                                  3                              8
    ## 11                                  3                              1
    ## 12                                  3                              9
    ## 13                                  3                             11
    ## 14                                  3                              8
    ## 15                                  4                              4
    ## 16                                  4                              1
    ## 17                                  4                             16
    ## 18                                  4                              9
    ## 19                                  4                              8
    ## 20                                  5                              2
    ## 21                                  5                             11
    ## 22                                  5                             16
    ## 23                                  5                              9

The total number of countries of Low Middle Income and in the first quantile group is 5.

Conclusion:

It is no surprise that among the countries with the largest economies also have the highest paid educators. However, educators are paid relatively well in some countries with the smallest economies (refer to the High Income: nonOECD chart).
