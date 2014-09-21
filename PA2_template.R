#Basic settings
echo = TRUE # always make code visible
#options(scipen = 1)# Turn off scientific notations for numbers
library(ggplot2)
library(plyr)
#Load Data, mapping as many field to numeric fields as possible.
#We are not converting the date at this points because we don't need
#the date in our analysis
StormData <- read.csv("repdata-data-StormData.csv",header= TRUE,sep=",")
StormData$EVTYPE <- as.character(StormData$EVTYPE)
StormData$PROPDMGEXP <- as.character(StormData$PROPDMGEXP)
StormData$CROPDMGEXP <- as.character(StormData$CROPDMGEXP)
StormData$FATALITIES <- as.numeric(as.character(StormData$FATALITIES))
StormData$INJURIES <- as.numeric(as.character(StormData$INJURIES))
StormData$PROPDMG <- as.numeric(as.character(StormData$PROPDMG))
StormData$CROPDMG <- as.numeric(as.character(StormData$CROPDMG))
dim(StormData)
#StormData is too huge, try to shrink the Dataset
StormData <- StormData[StormData$FATALITIES > 0|StormData$INJURIES > 0|StormData$PROPDMG >0|StormData$CROPDMG >0,]
dim(StormData)
#class(StormData$FATALITIES)
#Data Explore and find the data distribution
dim(StormData)
summary(StormData$PROPDMGEXP)
summary(StormData$CROPDMGEXP)
summary(StormData$EVTYPE)
#Shrink Dataset, the initial set has 902,297 observations. Since we need
#to find the harmful of population health and the damage of economic.
#we only keep the datasets which FATALITIES >0 or INJURIES >0 or PROPDMG >0
#or CROPDMG >0
#ShrinkData <- StormData[StormData$FATALITIES > 0 | StormData$INJURIES > 0
#| StormData$PROPDMG > 0 | StormData$CROPDMG > 0,]
#dim(ShrinkData)

#Due to the EVTYPE contain a lot of errors and issues. We will add a new column category that
#Classify by the category as used by the NCDC
#convection,storm, extreme temperature, flood, marine, tropical cyclone, winter, lighting, others

#The PROPDMGEXP and CROPDMGEXP fields need some conversion before we can do math on them.
#We add two extra columns PROPDMGCOV and CROPDMGCOV
StormData$PROPDMGCOV <- 1
#StormData[grepl("h | H", StormData$PROPDMGEXP, ignore.case = TRUE),]
StormData[grepl("h|H", StormData$PROPDMGEXP, ignore.case = TRUE),"PROPDMGCOV"] = 100
StormData[grepl("k|K", StormData$PROPDMGEXP, ignore.case = TRUE),"PROPDMGCOV"] = 1000
StormData[grepl("m|M", StormData$PROPDMGEXP, ignore.case = TRUE),"PROPDMGCOV"] = 1e+06
StormData[grepl("b|B", StormData$PROPDMGEXP, ignore.case = TRUE),"PROPDMGCOV"] = 1e+09
StormData$NEWPROPDMG <- (StormData$PROPDMG)*(StormData$PROPDMGCOV)
#head(StormData$NEWPROPDMG)
head(StormData, n=2)

StormData$CROPDMGCOV <- 1
StormData[grepl("h|H", StormData$CROPDMGEXP, ignore.case = TRUE),"CROPDMGCOV"] = 100
StormData[grepl("k|K", StormData$CROPDMGEXP, ignore.case = TRUE),"CROPDMGCOV"] = 1000
StormData[grepl("m|M", StormData$CROPDMGEXP, ignore.case = TRUE),"CROPDMGCOV"] = 1e+06
StormData[grepl("b|B", StormData$CROPDMGEXP, ignore.case = TRUE),"CROPDMGCOV"] = 1e+09
StormData$NEWCROPDMG <- (StormData$CROPDMG)*(StormData$CROPDMGCOV)
#head(StormData$NEWPROPDMG)
head(StormData, n=2)

#Add New Column category to classify EVTYPE
convention <- c("?.*TORNADO|TORNDAO|WIND|TSTM?.*")
lighting <- c("?.*LIGHTING|LIGHTN|LIGNTNING?.*")
storm <- c("?.*THUNDERSTORM|STORM?.*")
temperature <-c("?.*COLD|HEAT|WARM|TEMPERATURE|THERMIA?.*")
flood <-c("?.*FLOOD|RISING|STREAM FLD?.*")
marine <-c("?.*COASTAL|TSUNAMI|CURRENT|SWELLS|TIDE|WAVE|SEAS|SURF|HIGH WATER?.*")
cylones <-c("?.*CYCLONE|HURRICANE|TYPHOON?.*")
winter <-c("?.*WINT|ICE|HAIL|AVALAN|SLEET|SNOW|FREEZ|BLIZZ|FROST|GLAZE|MIXED?.*")
StormData[grepl(convention,StormData$EVTYPE,ignore.case=TRUE),"category"]="Convection"
StormData[grepl(lighting,StormData$EVTYPE,ignore.case=TRUE),"category"]="Lighting"
StormData[grepl(storm,StormData$EVTYPE,ignore.case=TRUE),"category"]="Storm"
StormData[grepl(temperature,StormData$EVTYPE,ignore.case=TRUE),"category"]="Temperature"
StormData[grepl(flood,StormData$EVTYPE,ignore.case=TRUE),"category"]="Flood"
StormData[grepl(marine,StormData$EVTYPE,ignore.case=TRUE),"category"]="Marine"
StormData[grepl(cylones,StormData$EVTYPE,ignore.case=TRUE),"category"]="Cylones"
StormData[grepl(winter,StormData$EVTYPE,ignore.case=TRUE),"category"]="Winter"
StormData[is.na(StormData$category),"category"]="Others"

# Result which type of events are most harmful with respect to population health
StormData$peopledmg <- StormData$FATALITIES + StormData$INJURIES
#par(mfrow = c(3,1))
#ggplot(StormData, aes(category,FATALITIES))+geom_bar(stat = "identity", colour = "green", fill = "green", width = 0.5)+
#labs(title= "The Total Number of Fatalities ", x="Event Type", y="Number of Fatalities ")
#ggplot(StormData, aes(category,INJURIES))+geom_bar(stat = "identity", colour = "green", fill = "green", width = 0.5)+
#labs(title= "The Total Number of Injuries ", x="Event Type", y="Number of Injuries ")
ggplot(StormData, aes(category,peopledmg))+geom_bar(stat = "identity", colour = "green", fill = "green", width = 0.5)+
  labs(title= "The Total Number of People Damage ", x="Event Type", y="Number of People Damage ")
# Calculate which types of events are most harmful with respect to population health ?
StormData$economicsdmg <- ((StormData$NEWPROPDMG + StormData$NEWCROPDMG)/1e+09)
ggplot(StormData, aes(category,economicsdmg))+geom_bar(stat = "identity", colour = "blue", fill = "blue", width = 0.5)+
  labs(title= "The Total Number of Economics Damage ", x="Event Type", y="Number of Economics Damage (USD$Billion)")
#fatalitiesdata <- aggregate(FATALITIES ~ EVTYPE,data=StormData,sum)
#worstfatalities <- tail(fatalitiesdata[order(fatalitiesdata$FATALITIES), ], 5)
#fatalitiesdata <- aggregate(FATALITIES ~ EVTYPE,data=StormData,sum)
#worstfatalities <- tail(fatalitiesdata[order(fatalitiesdata$FATALITIES),],5)
#barplot(worstfatalities$FATALITIES, names.arg=worstfatalities$EVTYPE, xlab="Fatalities", horiz=TRUE, las=1)

#sortHelper <- function(fieldName, top = 10, dataset= StormData){
#index <- which(colnames(dataset) == fieldName)
#field <- aggregate(dataset[,index],by=list(dataset$EVTYPE),FUN="sum")
#names(field) <- c("EVTYPE", fieldName)
#field <- arrange(field, field[,2],decreasing = T)
#field <- head(field, n = top)
#field <- within(field, EVTYPE <- factor(x=EVTYPE, levels=field$EVTYPE))
#return(field)
#}
#fatalities <- sortHelper("FATALITIES", dataset = StormData)
#injuries <- sortHelper("INJURIES", dataset = StormData)
#healthdata <- aggregate(FATALITIES+INJURIES ~ EVTYPE,data=StormData,sum)
#worsthealth <- head(healthdata[order(healthdata$"FATALITIES + INJURIES",decreasing = TRUE), ], 5)
#barplot(worsthealth$"FATALITIES + INJURIES", xlab="Overall health hazards", names.arg=worsthealth$EVTYPE, horiz=FALSE, las=1)
#head(healthdata
#head(worsthealth)

#head(WorstFatalities)
#order(FatalitiesData$Fatalities, decreasing = TRUE)
#WorstFatalities <- tail(FatalitiesData[order(FatalitiesData$Fatalities), ], 5)
#IncidentData <- aggregate(x=ShrinkData$incidenct, by=list(Event=ShrinkData$EVTYPE),FUN =sum)
#names(IncidentData)[2] <- "incident"
#head(IncidentData, n= 6, na.omit = TRUE)
#dim(IncidentData)