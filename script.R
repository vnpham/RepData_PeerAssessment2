

dataSource <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
dataZip <- "StormData.csv.bz2"

if (!file.exists(dataZip)) {
   download.file(dataSource, dataZip)
}

data1 <- read.csv(dataZip, header=TRUE, strip.white = TRUE, na.strings = c("NA","","?"))


Exp2Numeric <- function(x) {
   if (is.na(x)) 0
   else if (x %in% c(0:9)) as.numeric(x)
   else if (x == 'H' | x == 'h') 10^2
   else if (x == 'K' | x == 'k') 10^3
   else if (x == 'M' | x == 'm') 10^6
   else if (x == 'B' | x == 'b') 10^9
   else 0
}

data1$PROPDMGVAL <- mapply(FUN=function(x,y) {x*Exp2Numeric(y)}, data1$PROPDMG, data1$PROPDMGEXP)
data1$CROPDMGVAL <- mapply(FUN=function(x,y) {x*Exp2Numeric(y)}, data1$CROPDMG, data1$CROPDMGEXP)

require(stringr)
# data1$EVTYPE <- toupper(str_trim(levels(data1$EVTYPE)[data1$EVTYPE]))  # temporary

InitialRemap <- function(x) {
   result <- toupper(str_trim(x))   # make it all capital
   result <- gsub('&',replacement="/", result)   # substitute '/' for '&'
   result <-  gsub('\\\\', replacement="/", result)   # substitute '/' for '\'
   result <- gsub('\\sAND\\s',replacement="/", result) # substitute '/' for 'AND'
   result <- gsub('\\s*\\/\\s*',replacement="/", result)   # remove space on either sides of '/'
   result <- gsub('\\s*-\\s*',replacement="-", result)   # remove space on either sides of '-'
   result <- gsub('\\s+{2}',replacement=" ", result)   # remove redundant space
   result <- gsub('[()]',replacement="", result)
   result <- gsub(';',replacement="/", result)
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
   
   result <- gsub('^SUMMARY\\.*', replacement="OTHER", result)
   result <- gsub('^NONE$', replacement="OTHER", result)
   
   result <- gsub('\\bWINS\\b', replacement="WIND", result)
   result <- gsub('W INDS', replacement="WIND", result)
   result <- gsub('\\bWND\\b', replacement="WIND", result)
   result <- gsub('\\bWINTRY\\b', replacement="WINTER", result)
   result <- gsub('\\bWINTERY\\b', replacement="WINTER", result)
   
}

# data1$EVTYPE <- sapply(levels(data1$EVTYPE)[data1$EVTYPE],InitialRemap)
evtype <- sapply(levels(data1$EVTYPE)[data1$EVTYPE],InitialRemap)