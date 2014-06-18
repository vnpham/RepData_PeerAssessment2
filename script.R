#################################################################
# Author: Vinh N. Pham
# Purpose: script to clean up and process StormData
#################################################################

#----------------------------------------------------------------
ReadData <- function() {
   dataSource <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
   dataZip <- "StormData.csv.bz2"

   if (!file.exists(dataZip)) {
      download.file(dataSource, dataZip)
   }

   read.csv(dataZip, header=TRUE, strip.white = TRUE, na.strings = c("NA","","?"))
}

rawData <- ReadData()


#----------------------------------------------------------------
Exp2Numeric <- function(x) {
   if (is.na(x)) 0
   else if (x %in% c(0:9)) as.numeric(x)
   else if (x == 'H' | x == 'h') 10^2
   else if (x == 'K' | x == 'k') 10^3
   else if (x == 'M' | x == 'm') 10^6
   else if (x == 'B' | x == 'b') 10^9
   else 0
}

#----------------------------------------------------------------
ComputeDamage <- function(data) {
      mapply(FUN=function(x,y) {x*Exp2Numeric(y)}, data$PROPDMG, data$PROPDMGEXP) +
      mapply(FUN=function(x,y) {x*Exp2Numeric(y)}, data$CROPDMG, data$CROPDMGEXP)
}


rawData$DAMAGE <- ComputeDamage(rawData)
rawData$EVTYPE[is.na(rawData$EVTYPE)] <- "OTHER"   # adjust one case

#----------------------------------------------------------------
InitialRemap <- function(x) {
   require(stringr)   
   result <- toupper(str_trim(x))   # make it all capital
   
   result <- gsub('&',replacement="/", result)   # substitute '/' for '&'   
   result <-  gsub('\\\\', replacement="/", result)   # substitute '/' for '\'
   result <- gsub('\\sAND\\s',replacement="/", result) # substitute '/' for 'AND'
   result <- gsub(';',replacement="/", result)   # replace '/' for ';'
   result <- gsub('\\s*\\/\\s*',replacement="/", result)   # remove space on either sides of '/'
   
   result <- gsub('\\s*-\\s*',replacement="-", result)   # remove space on either sides of '-'
   ## remove all character '-' that doesn't separate terms so that the only character
   ## that separate terms are '/'
   result <- gsub('BLOW-OUT',replacement="BLOWOUT", result)
   result <- gsub('HURRICANE-GENERATED',replacement="HURRICANE", result)
   result <- gsub('LAKE-EFFECT',replacement="LAKE EFFECT", result)
   result <- gsub('LATE-SEASON',replacement="LATE SEASON", result)
   result <- gsub('NON-',replacement="NON", result)
   result <- gsub('LATE-SEASON',replacement="LATE SEASON", result)
   result <- gsub('-',replacement="/", result)
   
   result <- gsub('\\s+{2}',replacement=" ", result)   # remove redundant space
   result <- gsub('[()]',replacement="", result)

   result <- sub('\\W+$',replacement="", result)   # remove all non-word at the end
                  
   
   result <- gsub('\\bSNOWFALL\\b', replacement="SNOW", result)
   result <- gsub('\\bRAINFALL\\b', replacement="RAIN", result)
   result <- gsub('\\bASHFALL\\b', replacement="ASH", result)
   result <- gsub('\\bFLOODING\\b', replacement="FLOOD", result)
   
   result <- gsub('\\bTEMPERATURES\\b', replacement="TEMPERATURE", result)
   result <- gsub('\\bTIDES\\b', replacement="TIDE", result)
   result <- gsub('\\bFIRES\\b', replacement="FIRE", result)
   result <- gsub('\\bFUNNELS\\b', replacement="FUNNEL", result)
   result <- gsub('\\bCONDITIONS\\b', replacement="CONDITION", result)
   result <- gsub('\\bWINDSS\\b', replacement="WIND", result)
   result <- gsub('\\bWINDS\\b', replacement="WIND", result)
   result <- gsub('\\bCHILLS\\b', replacement="CHILL", result)
   result <- gsub('\\bJAMS\\b', replacement="JAM", result)
   result <- gsub('\\bFLOODS\\b', replacement="FLOOD", result)
   result <- gsub('\\bCLOUDS\\b', replacement="CLOUD", result)
   result <- gsub('\\bSTORMS\\b', replacement="STORM", result)
   result <- gsub('\\b\\bSEAS\\b', replacement="SEA", result)
   result <- gsub('\\bSHOWERS\\b', replacement="SHOWER", result)
   result <- gsub('\\bTREES\\b', replacement="TREE", result)
   result <- gsub('\\bSQUALLS\\b', replacement="SQUALL", result)
   result <- gsub('\\bTORNADOS\\b', replacement="TORNADO", result)
   result <- gsub('\\bTORNADOES\\b', replacement="TORNADO", result)
   result <- gsub('\\bWATERSPOUTS\\b', replacement="WATERSPOUT", result)
   result <- gsub('\\bPELLETS\\b', replacement="PELLET", result)
   result <- gsub('\\bLIGHTS\\b', replacement="LIGHT", result)
   result <- gsub('\\bEFFECTS\\b', replacement="EFFECT", result)
   result <- gsub('\\bSLIDES\\b', replacement="SLIDE", result)
   result <- gsub('\\bROADS\\b', replacement="ROAD", result)
   result <- gsub('\\bWAVES\\b', replacement="WAVE", result)
   result <- gsub('\\bFLURRIES\\b', replacement="FLURRY", result)
   result <- gsub('\\bSWELLS\\b', replacement="SWELL", result)
   result <- gsub('\\bGUSTS\\b', replacement="GUST", result)
   result <- gsub('\\bCURRENTS\\b', replacement="CURRENT", result)
   result <- gsub('\\bTEMPS\\b', replacement="TEMPERATURE", result)
   result <- gsub('\\bRAINS\\b', replacement="RAIN", result)
   result <- gsub('\\bSNOWS\\b', replacement="SNOW", result)
   result <- gsub('\\bADVISORIES\\b', replacement="ADVISORY", result)
   result <- gsub('\\bLANDSLIDES\\b', replacement="LANDSLIDE", result)
   result <- gsub('\\bTHUNDERSTORMS\\b', replacement="THUNDERSTORM", result)
   result <- gsub('\\bMUDSLIDES\\b', replacement="MUDSLIDE", result)
   result <- gsub('\\bHAILSTORMS\\b', replacement="HAILSTORM", result)
   result <- gsub('\\bWILDFIRES\\b', replacement="WILDFIRE", result)
   result <- gsub('\\bTHUNDERSTORMWINDS\\b', replacement="THUNDERSTORM WIND", result)
   
   result <- gsub('^SUMMARY.*', replacement="OTHER", result)
   result <- gsub('^NONE$', replacement="OTHER", result)
   
   result <- gsub('\\bWINS\\b', replacement="WIND", result)
   result <- gsub('W INDS', replacement="WIND", result)
   result <- gsub('\\bWND\\b', replacement="WIND", result)
   result <- gsub('\\bWINTRY\\b', replacement="WINTER", result)
   result <- gsub('\\bWINTERY\\b', replacement="WINTER", result)
   
}


rawData$TYPE <- sapply(levels(rawData$EVTYPE)[rawData$EVTYPE],InitialRemap)

#----------------------------------------------------------------
SeparateEventType <- function(data) {   
   evtype <- data$TYPE

   require(reshape2)
   evtype <- colsplit(evtype,"/",c("TYPE1","TYPE2","TYPE3"))
   evtype$TYPE2[evtype$TYPE2==""] <- NA
   evtype$TYPE3[evtype$TYPE3==""] <- NA

   data2 <- data[,c("REFNUM", "BGN_DATE", "FATALITIES","INJURIES", "DAMAGE")]
   data2 <- cbind(data2, evtype)

   data2 <- melt(data2, measure.vars=c("TYPE1", "TYPE2", "TYPE3"),
                 value.name="TYPE",
                 na.rm=TRUE)
   data2$variable <- NULL
   row.names(data2) <- NULL
   
   data2   # return new clean data
}

cleanData <- SeparateEventType(rawData)

#----------------------------------------------------------------
# make the raw map file
CreateRawMap <- function(data) {
   rawMapFile <- "RawMap.txt"
   evnames <- unique(sort(data$TYPE))
   write(evnames, rawMapFile)
}

CreateRawMap(cleanData)

#----------------------------------------------------------------
DetailRemap <- function(data) {
   remap <- read.table("remap.txt", fill=TRUE,header=TRUE,sep=">",
                       colClasses=c("character","character"),strip.white=TRUE)
   
   RemapEventName <- function(x) {
      res <- remap$new[remap$original==x]
      ifelse(res=="",x,res)
   }
   sapply(data$TYPE,RemapEventName)
   
}

cleanData$TYPE <- DetailRemap(cleanData)

#----------------------------------------------------------------
# only to create "EventTypeCode.txt" file which is the basis
# for the MyEventType.md (rearrange in similar categories rather
# than alphabetically).
CreateEventTypeCodeFile <- function(data) {
   typeCodeFile <- "EventTypeCode.txt"
   typeCode <- unique(sort(data$TYPE))
   write(typeCode, typeCodeFile)
}

CreateEventTypeCodeFile(cleanData)


#----------------------------------------------------------------
cleanData$TYPE <- factor(cleanData$TYPE)
   
################ SUMMARIES ######################################
CreateSummaryData <- function(data) {
   aggregate(data[3:5], by=data["TYPE"],sum)
}

summaryData <- CreateSummaryData(cleanData)

#----------------------------------------------------------------
fatalities <- (head(summaryData[order(summaryData$FATALITIES, decreasing=TRUE),
                                c("TYPE", "FATALITIES")],n=10))

barplot(fatalities[,2], col=rainbow(10), legend=fatalities[,1],
        main="Fatalities Report")


#----------------------------------------------------------------
injuries <- (head(summaryData[order(summaryData$INJURIES, decreasing=TRUE),
                                c("TYPE", "INJURIES")],n=10))

barplot(injuries[,2], col=rainbow(10), legend=injuries[,1],
        main="Injuries Report")

#----------------------------------------------------------------
damages <- (head(summaryData[order(summaryData$DAMAGE, decreasing=TRUE),
                                c("TYPE", "DAMAGE")],n=10))
barplot(damages[,2], col=rainbow(10), legend=damages[,1],
        main="Damage Report",
        ylab="Cost in Dollars")
