Reproducible Research: Peer Assessment2
========================================================
Created by Trent Lin sep.21 2014

# Impact of Severe Weather Events on Public Health and Economy in the United State

## Synonpsis
In this project we aim to analyze the impact of differenct weather events on public health and economy
damage based on the storm database collected from the U.S. National Oceanic and Atmospheric Administration's (NOAA) from 1950 - 2011. We will use fatalities, injuries, property and crop damage to decide which types of event are most harmful to the population health and economy. We found that convection(Tornado, Wind) are the most harmful to people health. Storm(THUNDERSTORM, Winter STORM) and Winter(ICE, HAIL, SNOW) are the most harmful to economics.

## Basic Setting

```r
echo = TRUE # always make code visible
library(ggplot2)
library(plyr)
```
# Data Processing
## Load Data and convert data type

```r
StormData <- read.csv("repdata-data-StormData.csv.bz2",header= TRUE,sep=",")
```

```
## Warning: 無法開啟檔案 'repdata-data-StormData.csv.bz2' ：No such file or
## directory
```

```
## Error: 無法開啟連結
```

```r
StormData$EVTYPE <- as.character(StormData$EVTYPE)
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData$PROPDMGEXP <- as.character(StormData$PROPDMGEXP)
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData$CROPDMGEXP <- as.character(StormData$CROPDMGEXP)
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData$FATALITIES <- as.numeric(as.character(StormData$FATALITIES))
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData$INJURIES <- as.numeric(as.character(StormData$INJURIES))
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData$PROPDMG <- as.numeric(as.character(StormData$PROPDMG))
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData$CROPDMG <- as.numeric(as.character(StormData$CROPDMG))
```

```
## Error: 找不到物件 'StormData'
```

```r
dim(StormData)
```

```
## Error: 找不到物件 'StormData'
```
## Srink the DataSet
Due to Dataset is too huge and may slow down the process. 
We exclude the data which fatalities = 0 or injuries = 0 or propdmg = 0 or cropdmg = 0

```r
StormData <- StormData[StormData$FATALITIES > 0|StormData$INJURIES > 0|StormData$PROPDMG >0|StormData$CROPDMG >0,]
```

```
## Error: 找不到物件 'StormData'
```

```r
dim(StormData)
```

```
## Error: 找不到物件 'StormData'
```
## Classify Dataset
Due to EVTYPE has a lot of error and issue. We use grepl() to classify dataset in convection,cylones,
flood,lighting,marine,storm,temperature, winter eight category.

```r
convention <- c("?.*TORNADO|TORNDAO|WIND|TSTM?.*")
lighting <- c("?.*LIGHTING|LIGHTN|LIGNTNING?.*")
storm <- c("?.*THUNDERSTORM|STORM?.*")
temperature <-c("?.*COLD|HEAT|WARM|TEMPERATURE|THERMIA?.*")
flood <-c("?.*FLOOD|RISING|STREAM FLD?.*")
marine <-c("?.*COASTAL|TSUNAMI|CURRENT|SWELLS|TIDE|WAVE|SEAS|SURF|HIGH WATER?.*")
cylones <-c("?.*CYCLONE|HURRICANE|TYPHOON?.*")
winter <-c("?.*WINT|ICE|HAIL|AVALAN|SLEET|SNOW|FREEZ|BLIZZ|FROST|GLAZE|MIXED?.*")
StormData[grepl(convention,StormData$EVTYPE,ignore.case=TRUE),"category"]="Convection"
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData[grepl(lighting,StormData$EVTYPE,ignore.case=TRUE),"category"]="Lighting"
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData[grepl(storm,StormData$EVTYPE,ignore.case=TRUE),"category"]="Storm"
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData[grepl(temperature,StormData$EVTYPE,ignore.case=TRUE),"category"]="Temperature"
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData[grepl(flood,StormData$EVTYPE,ignore.case=TRUE),"category"]="Flood"
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData[grepl(marine,StormData$EVTYPE,ignore.case=TRUE),"category"]="Marine"
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData[grepl(cylones,StormData$EVTYPE,ignore.case=TRUE),"category"]="Cylones"
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData[grepl(winter,StormData$EVTYPE,ignore.case=TRUE),"category"]="Winter"
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData[is.na(StormData$category),"category"]="Others"
```

```
## Error: 找不到物件 'StormData'
```
## Make property damage and crop damage available to calculate
Because there are different unit type in PROPDMGEXP and CROPDMGEXP. We create four new columns PROPDMGCOV, NEWPROPDMG, CROPDMGCOV, NEWCROPDMG for unit conversion and damage calculation.

```r
StormData$PROPDMGCOV <- 1
```

```
## Error: 找不到物件 'StormData'
```

```r
#StormData[grepl("h | H", StormData$PROPDMGEXP, ignore.case = TRUE),]
StormData[grepl("h|H", StormData$PROPDMGEXP, ignore.case = TRUE),"PROPDMGCOV"] = 100
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData[grepl("k|K", StormData$PROPDMGEXP, ignore.case = TRUE),"PROPDMGCOV"] = 1000
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData[grepl("m|M", StormData$PROPDMGEXP, ignore.case = TRUE),"PROPDMGCOV"] = 1e+06
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData[grepl("b|B", StormData$PROPDMGEXP, ignore.case = TRUE),"PROPDMGCOV"] = 1e+09
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData$NEWPROPDMG <- (StormData$PROPDMG)*(StormData$PROPDMGCOV)
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData$CROPDMGCOV <- 1
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData[grepl("h|H", StormData$CROPDMGEXP, ignore.case = TRUE),"CROPDMGCOV"] = 100
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData[grepl("k|K", StormData$CROPDMGEXP, ignore.case = TRUE),"CROPDMGCOV"] = 1000
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData[grepl("m|M", StormData$CROPDMGEXP, ignore.case = TRUE),"CROPDMGCOV"] = 1e+06
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData[grepl("b|B", StormData$CROPDMGEXP, ignore.case = TRUE),"CROPDMGCOV"] = 1e+09
```

```
## Error: 找不到物件 'StormData'
```

```r
StormData$NEWCROPDMG <- (StormData$CROPDMG)*(StormData$CROPDMGCOV)
```

```
## Error: 找不到物件 'StormData'
```
# Result
## Which types of events are most harmful to polulation health ?
we create new column peopledmg to combine fataliteis and injuries. Then draw a picture to show
the relationship between weather events and polulation health

```r
StormData$peopledmg <- StormData$FATALITIES + StormData$INJURIES
```

```
## Error: 找不到物件 'StormData'
```

```r
ggplot(StormData, aes(category,peopledmg))+geom_bar(stat = "identity", colour = "green", fill = "green", width = 0.5)+
  labs(title= "The Total Number of People Damage ", x="Event Type", y="Number of People Damage ")
```

```
## Error: 找不到物件 'StormData'
```
## Conclusion1
As the picture we found that convetion(Tornado, Wind) are the most harmful to people health.

## Which type of event are most harmful to economics
we create new column economicsdmg to calculate the total economics damage which combine property damage and crop damage. Then draw a picture to showthe relationship between weather events and economics damage.

```r
StormData$economicsdmg <- ((StormData$NEWPROPDMG + StormData$NEWCROPDMG)/1e+09)
```

```
## Error: 找不到物件 'StormData'
```

```r
ggplot(StormData, aes(category,economicsdmg))+geom_bar(stat = "identity", colour = "blue", fill = "blue", width = 0.5)+
  labs(title= "The Total Number of Economics Damage ", x="Event Type", y="Number of Economics Damage (USD$Billion)")
```

```
## Error: 找不到物件 'StormData'
```

## Conclusion2
As the picture we found that Storm(THUNDERSTORM, Winter STORM) and Winter(ICE, HAIL, SNOW) are the  most harmful to economics.
