library(dplyr)

stormData <- read.csv("./data/repdata_data_StormData.csv.bz2")
damageData <- select(stormData, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>% 
     filter((!is.na(FATALITIES) & FATALITIES > 0) | (!is.na(INJURIES) & INJURIES > 0) | (is.na(PROPDMG) | PROPDMG > 0) 
            | (is.na(CROPDMG) | CROPDMG > 0))


damageData$PROPDMGEXP <- as.character(damageData$PROPDMGEXP)
damageData$CROPDMGEXP <- as.character(damageData$CROPDMGEXP)

## Convert Exponentials from letters to numbers.
damageData[(damageData$PROPDMGEXP == "H" | damageData$PROPDMGEXP == "h"), ]$PROPDMGEXP <- 2
damageData[(damageData$PROPDMGEXP == "K" | damageData$PROPDMGEXP == "k"), ]$PROPDMGEXP <- 3
damageData[(damageData$PROPDMGEXP == "M" | damageData$PROPDMGEXP == "m"), ]$PROPDMGEXP <- 6
damageData[(damageData$PROPDMGEXP == "B" | damageData$PROPDMGEXP == "b"), ]$PROPDMGEXP <- 9
damageData[damageData$PROPDMGEXP == "-", ]$PROPDMGEXP <- 0
damageData[damageData$PROPDMGEXP == "+", ]$PROPDMGEXP <- 0
damageData[damageData$PROPDMGEXP == "", ]$PROPDMGEXP <- 0
damageData[damageData$CROPDMGEXP == "?", ]$CROPDMGEXP <- 0

damageData[(damageData$CROPDMGEXP == "K" | damageData$CROPDMGEXP == "k"), ]$CROPDMGEXP <- 3
damageData[(damageData$CROPDMGEXP == "M" | damageData$CROPDMGEXP == "m"), ]$CROPDMGEXP <- 6
damageData[(damageData$CROPDMGEXP == "B" | damageData$CROPDMGEXP == "b"), ]$CROPDMGEXP <- 9
damageData[damageData$CROPDMGEXP == "", ]$CROPDMGEXP <- 0

damageData$PROPDMGEXP <- as.numeric(damageData$PROPDMGEXP)
damageData$CROPDMGEXP <- as.numeric(damageData$CROPDMGEXP)

damageData <- mutate(damageData, PROPDMGVAL = PROPDMG*10^PROPDMGEXP, CROPDMGVAL=CROPDMG*10^CROPDMGEXP)

##Plot Fatalities and Injuries values
## Group by event types and summarise totals
humanDamageDataFat <- group_by(damageData, EVTYPE) %>% summarise(TOTAL_FATALITIES = sum(FATALITIES)) %>%
     arrange(desc(TOTAL_FATALITIES)) %>% head(20)

humanDamageDataInj <- group_by(damageData, EVTYPE) %>% summarise(TOTAL_INJURIES = sum(INJURIES)) %>%
     arrange(desc(TOTAL_INJURIES)) %>% head(20)


par(mfrow = c(1, 2), mar = c(12, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
barplot(humanDamageDataFat$TOTAL_FATALITIES, las = 3, names.arg = humanDamageDataFat$EVTYPE, main = "Top 20 Highest Fatalities", 
        ylab = "Number of fatalities", col = "red")
barplot(humanDamageDataInj$TOTAL_INJURIES, las = 3, names.arg = humanDamageDataInj$EVTYPE, main = "Top 20 Highest Injuries", 
        ylab = "Number of injuries", col = "blue")

##plot economic values
## Group by event types
## Group by event types and summarise totals
economicDamageDataProp <- group_by(damageData, EVTYPE) %>% summarise(TOTAL_PROPDMGVAL = sum(PROPDMGVAL)) %>%
     arrange(desc(TOTAL_PROPDMGVAL)) %>% head(20)

economicDamageDataCrop <- group_by(damageData, EVTYPE) %>% summarise(TOTAL_CROPDMGVAL = sum(CROPDMGVAL)) %>%
     arrange(desc(TOTAL_CROPDMGVAL)) %>% head(20)


par(mfrow = c(1, 2), mar = c(12, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
barplot(economicDamageDataProp$TOTAL_PROPDMGVAL/1000000, las = 3, names.arg = economicDamageDataProp$EVTYPE, main = "Top 20 greatest property costs", 
        ylab = "Property costs (millions)", col = "red")
barplot(economicDamageDataCrop$TOTAL_CROPDMGVAL/1000000, las = 3, names.arg = humanDamageDataInj$EVTYPE, main = "Top 20 greatest crop costs", 
        ylab = "Crop costs (millions)", col = "blue")


