# packages
library(here)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)

########################
### CDEC Water Years ###
########################
WaterYr <- read_excel("CDEC Water Year Hydrologic Classification Indices.xlsx", 
                      sheet = "Sheet3")

#########################################
### Drought group-defined Water Years ###
#########################################
DroughtYr <- read.csv("yearassignments.csv")
colnames(DroughtYr)[1] <- "WY"

################################
# Color Scheme to match others #
################################

# colors from Rosy/drought group
pal_drought <- c( "D" = "#FDE333", "N" = "#53CC67","W" = "#00588B")
pal_yrtype <- c( "Critical" = "#FDE333", "Dry" = "#53CC67", "Below Normal" = "#009B95","Above Normal" = "#00588B", "Wet" = "#4B0055")
pal_yrtype2 <- c( "C" = "#FDE333", "D" = "#53CC67", "BN" = "#009B95","AN" = "#00588B", "W" = "#4B0055")

# use theme_bw()

###################################
# DJFMP data from EDI Data Portal #
###################################

# use R script from EDI Data Portal
# OR read in manually after download

# for this analysis, need only dt2 and dt3 from their script

#######
# dt2 #
#######
# file from "https://pasta.lternet.edu/package/data/eml/edi/244/9/4cf98db173a16731bcbb2d37ad656538" 
dt2 <-read.csv("2002-2021_DJFMP_trawl_fish_and_water_quality_data.csv",header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Location",     
                 "RegionCode",     
                 "StationCode",     
                 "SampleDate",     
                 "SampleTime",     
                 "MethodCode",     
                 "GearConditionCode",     
                 "WeatherCode",     
                 "DO",     
                 "WaterTemp",     
                 "Turbidity",     
                 "Secchi",     
                 "SpecificConductance",     
                 "TowNumber",     
                 "SamplingDirection",     
                 "TowDuration",     
                 "FlowDebris",     
                 "SiteDisturbance",     
                 "AlternateSite",     
                 "SeineLength",     
                 "SeineWidth",     
                 "SeineDepth",     
                 "FlowmeterStart",     
                 "FlowmeterEnd",     
                 "FlowmeterDifference",     
                 "Volume",     
                 "OrganismCode",     
                 "IEPFishCode",     
                 "CommonName",     
                 "MarkCode",     
                 "StageCode",     
                 "Expression",     
                 "ForkLength",     
                 "RaceByLength",     
                 "TagCode",     
                 "RaceByTag",     
                 "ArchivalID",     
                 "SpecialStudyID",     
                 "GeneticID",     
                 "Probability1",     
                 "GeneticID2",     
                 "Probability2",     
                 "SexGeneID",     
                 "Ots28",     
                 "Lab",     
                 "GeneticTest",     
                 "GeneticModel",     
                 "Count"    ), check.names=TRUE)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$Location)!="factor") dt2$Location<- as.factor(dt2$Location)
if (class(dt2$RegionCode)!="factor") dt2$RegionCode<- as.factor(dt2$RegionCode)
if (class(dt2$StationCode)!="factor") dt2$StationCode<- as.factor(dt2$StationCode)                                   
# attempting to convert dt2$SampleDate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2SampleDate<-as.Date(dt2$SampleDate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2SampleDate) == length(tmp2SampleDate[!is.na(tmp2SampleDate)])){dt2$SampleDate <- tmp2SampleDate } else {print("Date conversion failed for dt2$SampleDate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2SampleDate) 
if (class(dt2$MethodCode)!="factor") dt2$MethodCode<- as.factor(dt2$MethodCode)
if (class(dt2$GearConditionCode)!="factor") dt2$GearConditionCode<- as.factor(dt2$GearConditionCode)
if (class(dt2$WeatherCode)!="factor") dt2$WeatherCode<- as.factor(dt2$WeatherCode)
if (class(dt2$DO)=="factor") dt2$DO <-as.numeric(levels(dt2$DO))[as.integer(dt2$DO) ]               
if (class(dt2$DO)=="character") dt2$DO <-as.numeric(dt2$DO)
if (class(dt2$WaterTemp)=="factor") dt2$WaterTemp <-as.numeric(levels(dt2$WaterTemp))[as.integer(dt2$WaterTemp) ]               
if (class(dt2$WaterTemp)=="character") dt2$WaterTemp <-as.numeric(dt2$WaterTemp)
if (class(dt2$Turbidity)=="factor") dt2$Turbidity <-as.numeric(levels(dt2$Turbidity))[as.integer(dt2$Turbidity) ]               
if (class(dt2$Turbidity)=="character") dt2$Turbidity <-as.numeric(dt2$Turbidity)
if (class(dt2$Secchi)=="factor") dt2$Secchi <-as.numeric(levels(dt2$Secchi))[as.integer(dt2$Secchi) ]               
if (class(dt2$Secchi)=="character") dt2$Secchi <-as.numeric(dt2$Secchi)
if (class(dt2$SpecificConductance)=="factor") dt2$SpecificConductance <-as.numeric(levels(dt2$SpecificConductance))[as.integer(dt2$SpecificConductance) ]               
if (class(dt2$SpecificConductance)=="character") dt2$SpecificConductance <-as.numeric(dt2$SpecificConductance)
if (class(dt2$TowNumber)=="factor") dt2$TowNumber <-as.numeric(levels(dt2$TowNumber))[as.integer(dt2$TowNumber) ]               
if (class(dt2$TowNumber)=="character") dt2$TowNumber <-as.numeric(dt2$TowNumber)
if (class(dt2$SamplingDirection)!="factor") dt2$SamplingDirection<- as.factor(dt2$SamplingDirection)
if (class(dt2$TowDuration)=="factor") dt2$TowDuration <-as.numeric(levels(dt2$TowDuration))[as.integer(dt2$TowDuration) ]               
if (class(dt2$TowDuration)=="character") dt2$TowDuration <-as.numeric(dt2$TowDuration)
if (class(dt2$FlowDebris)!="factor") dt2$FlowDebris<- as.factor(dt2$FlowDebris)
if (class(dt2$SiteDisturbance)!="factor") dt2$SiteDisturbance<- as.factor(dt2$SiteDisturbance)
if (class(dt2$AlternateSite)!="factor") dt2$AlternateSite<- as.factor(dt2$AlternateSite)
if (class(dt2$SeineLength)=="factor") dt2$SeineLength <-as.numeric(levels(dt2$SeineLength))[as.integer(dt2$SeineLength) ]               
if (class(dt2$SeineLength)=="character") dt2$SeineLength <-as.numeric(dt2$SeineLength)
if (class(dt2$SeineWidth)=="factor") dt2$SeineWidth <-as.numeric(levels(dt2$SeineWidth))[as.integer(dt2$SeineWidth) ]               
if (class(dt2$SeineWidth)=="character") dt2$SeineWidth <-as.numeric(dt2$SeineWidth)
if (class(dt2$SeineDepth)=="factor") dt2$SeineDepth <-as.numeric(levels(dt2$SeineDepth))[as.integer(dt2$SeineDepth) ]               
if (class(dt2$SeineDepth)=="character") dt2$SeineDepth <-as.numeric(dt2$SeineDepth)
if (class(dt2$FlowmeterStart)=="factor") dt2$FlowmeterStart <-as.numeric(levels(dt2$FlowmeterStart))[as.integer(dt2$FlowmeterStart) ]               
if (class(dt2$FlowmeterStart)=="character") dt2$FlowmeterStart <-as.numeric(dt2$FlowmeterStart)
if (class(dt2$FlowmeterEnd)=="factor") dt2$FlowmeterEnd <-as.numeric(levels(dt2$FlowmeterEnd))[as.integer(dt2$FlowmeterEnd) ]               
if (class(dt2$FlowmeterEnd)=="character") dt2$FlowmeterEnd <-as.numeric(dt2$FlowmeterEnd)
if (class(dt2$FlowmeterDifference)=="factor") dt2$FlowmeterDifference <-as.numeric(levels(dt2$FlowmeterDifference))[as.integer(dt2$FlowmeterDifference) ]               
if (class(dt2$FlowmeterDifference)=="character") dt2$FlowmeterDifference <-as.numeric(dt2$FlowmeterDifference)
if (class(dt2$Volume)=="factor") dt2$Volume <-as.numeric(levels(dt2$Volume))[as.integer(dt2$Volume) ]               
if (class(dt2$Volume)=="character") dt2$Volume <-as.numeric(dt2$Volume)
if (class(dt2$OrganismCode)!="factor") dt2$OrganismCode<- as.factor(dt2$OrganismCode)
if (class(dt2$IEPFishCode)!="factor") dt2$IEPFishCode<- as.factor(dt2$IEPFishCode)
if (class(dt2$CommonName)!="factor") dt2$CommonName<- as.factor(dt2$CommonName)
if (class(dt2$MarkCode)!="factor") dt2$MarkCode<- as.factor(dt2$MarkCode)
if (class(dt2$StageCode)!="factor") dt2$StageCode<- as.factor(dt2$StageCode)
if (class(dt2$Expression)!="factor") dt2$Expression<- as.factor(dt2$Expression)
if (class(dt2$ForkLength)=="factor") dt2$ForkLength <-as.numeric(levels(dt2$ForkLength))[as.integer(dt2$ForkLength) ]               
if (class(dt2$ForkLength)=="character") dt2$ForkLength <-as.numeric(dt2$ForkLength)
if (class(dt2$RaceByLength)!="factor") dt2$RaceByLength<- as.factor(dt2$RaceByLength)
if (class(dt2$TagCode)!="factor") dt2$TagCode<- as.factor(dt2$TagCode)
if (class(dt2$RaceByTag)!="factor") dt2$RaceByTag<- as.factor(dt2$RaceByTag)
if (class(dt2$ArchivalID)!="factor") dt2$ArchivalID<- as.factor(dt2$ArchivalID)
if (class(dt2$SpecialStudyID)!="factor") dt2$SpecialStudyID<- as.factor(dt2$SpecialStudyID)
if (class(dt2$GeneticID)!="factor") dt2$GeneticID<- as.factor(dt2$GeneticID)
if (class(dt2$Probability1)=="factor") dt2$Probability1 <-as.numeric(levels(dt2$Probability1))[as.integer(dt2$Probability1) ]               
if (class(dt2$Probability1)=="character") dt2$Probability1 <-as.numeric(dt2$Probability1)
if (class(dt2$GeneticID2)!="factor") dt2$GeneticID2<- as.factor(dt2$GeneticID2)
if (class(dt2$Probability2)=="factor") dt2$Probability2 <-as.numeric(levels(dt2$Probability2))[as.integer(dt2$Probability2) ]               
if (class(dt2$Probability2)=="character") dt2$Probability2 <-as.numeric(dt2$Probability2)
if (class(dt2$SexGeneID)!="factor") dt2$SexGeneID<- as.factor(dt2$SexGeneID)
if (class(dt2$Ots28)!="factor") dt2$Ots28<- as.factor(dt2$Ots28)
if (class(dt2$Lab)!="factor") dt2$Lab<- as.factor(dt2$Lab)
if (class(dt2$GeneticTest)!="factor") dt2$GeneticTest<- as.factor(dt2$GeneticTest)
if (class(dt2$GeneticModel)!="factor") dt2$GeneticModel<- as.factor(dt2$GeneticModel)
if (class(dt2$Count)=="factor") dt2$Count <-as.numeric(levels(dt2$Count))[as.integer(dt2$Count) ]               
if (class(dt2$Count)=="character") dt2$Count <-as.numeric(dt2$Count)

# Convert Missing Values to NA for non-dates

dt2$Location <- as.factor(ifelse((trimws(as.character(dt2$Location))==trimws("NA")),NA,as.character(dt2$Location)))
dt2$RegionCode <- as.factor(ifelse((trimws(as.character(dt2$RegionCode))==trimws("NA")),NA,as.character(dt2$RegionCode)))
dt2$StationCode <- as.factor(ifelse((trimws(as.character(dt2$StationCode))==trimws("NA")),NA,as.character(dt2$StationCode)))
dt2$MethodCode <- as.factor(ifelse((trimws(as.character(dt2$MethodCode))==trimws("NA")),NA,as.character(dt2$MethodCode)))
dt2$GearConditionCode <- as.factor(ifelse((trimws(as.character(dt2$GearConditionCode))==trimws("NA")),NA,as.character(dt2$GearConditionCode)))
dt2$WeatherCode <- as.factor(ifelse((trimws(as.character(dt2$WeatherCode))==trimws("NA")),NA,as.character(dt2$WeatherCode)))
dt2$DO <- ifelse((trimws(as.character(dt2$DO))==trimws("NA")),NA,dt2$DO)               
suppressWarnings(dt2$DO <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$DO))==as.character(as.numeric("NA"))),NA,dt2$DO))
dt2$WaterTemp <- ifelse((trimws(as.character(dt2$WaterTemp))==trimws("NA")),NA,dt2$WaterTemp)               
suppressWarnings(dt2$WaterTemp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$WaterTemp))==as.character(as.numeric("NA"))),NA,dt2$WaterTemp))
dt2$Turbidity <- ifelse((trimws(as.character(dt2$Turbidity))==trimws("NA")),NA,dt2$Turbidity)               
suppressWarnings(dt2$Turbidity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Turbidity))==as.character(as.numeric("NA"))),NA,dt2$Turbidity))
dt2$Secchi <- ifelse((trimws(as.character(dt2$Secchi))==trimws("NA")),NA,dt2$Secchi)               
suppressWarnings(dt2$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Secchi))==as.character(as.numeric("NA"))),NA,dt2$Secchi))
dt2$SpecificConductance <- ifelse((trimws(as.character(dt2$SpecificConductance))==trimws("NA")),NA,dt2$SpecificConductance)               
suppressWarnings(dt2$SpecificConductance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SpecificConductance))==as.character(as.numeric("NA"))),NA,dt2$SpecificConductance))
dt2$TowNumber <- ifelse((trimws(as.character(dt2$TowNumber))==trimws("NA")),NA,dt2$TowNumber)               
suppressWarnings(dt2$TowNumber <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TowNumber))==as.character(as.numeric("NA"))),NA,dt2$TowNumber))
dt2$SamplingDirection <- as.factor(ifelse((trimws(as.character(dt2$SamplingDirection))==trimws("NA")),NA,as.character(dt2$SamplingDirection)))
dt2$TowDuration <- ifelse((trimws(as.character(dt2$TowDuration))==trimws("NA")),NA,dt2$TowDuration)               
suppressWarnings(dt2$TowDuration <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TowDuration))==as.character(as.numeric("NA"))),NA,dt2$TowDuration))
dt2$FlowDebris <- as.factor(ifelse((trimws(as.character(dt2$FlowDebris))==trimws("NA")),NA,as.character(dt2$FlowDebris)))
dt2$SiteDisturbance <- as.factor(ifelse((trimws(as.character(dt2$SiteDisturbance))==trimws("NA")),NA,as.character(dt2$SiteDisturbance)))
dt2$AlternateSite <- as.factor(ifelse((trimws(as.character(dt2$AlternateSite))==trimws("NA")),NA,as.character(dt2$AlternateSite)))
dt2$SeineLength <- ifelse((trimws(as.character(dt2$SeineLength))==trimws("NA")),NA,dt2$SeineLength)               
suppressWarnings(dt2$SeineLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SeineLength))==as.character(as.numeric("NA"))),NA,dt2$SeineLength))
dt2$SeineWidth <- ifelse((trimws(as.character(dt2$SeineWidth))==trimws("NA")),NA,dt2$SeineWidth)               
suppressWarnings(dt2$SeineWidth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SeineWidth))==as.character(as.numeric("NA"))),NA,dt2$SeineWidth))
dt2$SeineDepth <- ifelse((trimws(as.character(dt2$SeineDepth))==trimws("NA")),NA,dt2$SeineDepth)               
suppressWarnings(dt2$SeineDepth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SeineDepth))==as.character(as.numeric("NA"))),NA,dt2$SeineDepth))
dt2$FlowmeterStart <- ifelse((trimws(as.character(dt2$FlowmeterStart))==trimws("NA")),NA,dt2$FlowmeterStart)               
suppressWarnings(dt2$FlowmeterStart <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$FlowmeterStart))==as.character(as.numeric("NA"))),NA,dt2$FlowmeterStart))
dt2$FlowmeterEnd <- ifelse((trimws(as.character(dt2$FlowmeterEnd))==trimws("NA")),NA,dt2$FlowmeterEnd)               
suppressWarnings(dt2$FlowmeterEnd <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$FlowmeterEnd))==as.character(as.numeric("NA"))),NA,dt2$FlowmeterEnd))
dt2$FlowmeterDifference <- ifelse((trimws(as.character(dt2$FlowmeterDifference))==trimws("NA")),NA,dt2$FlowmeterDifference)               
suppressWarnings(dt2$FlowmeterDifference <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$FlowmeterDifference))==as.character(as.numeric("NA"))),NA,dt2$FlowmeterDifference))
dt2$Volume <- ifelse((trimws(as.character(dt2$Volume))==trimws("NA")),NA,dt2$Volume)               
suppressWarnings(dt2$Volume <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Volume))==as.character(as.numeric("NA"))),NA,dt2$Volume))
dt2$OrganismCode <- as.factor(ifelse((trimws(as.character(dt2$OrganismCode))==trimws("NA")),NA,as.character(dt2$OrganismCode)))
dt2$IEPFishCode <- as.factor(ifelse((trimws(as.character(dt2$IEPFishCode))==trimws("NA")),NA,as.character(dt2$IEPFishCode)))
dt2$CommonName <- as.factor(ifelse((trimws(as.character(dt2$CommonName))==trimws("NA")),NA,as.character(dt2$CommonName)))
dt2$MarkCode <- as.factor(ifelse((trimws(as.character(dt2$MarkCode))==trimws("NA")),NA,as.character(dt2$MarkCode)))
dt2$StageCode <- as.factor(ifelse((trimws(as.character(dt2$StageCode))==trimws("NA")),NA,as.character(dt2$StageCode)))
dt2$Expression <- as.factor(ifelse((trimws(as.character(dt2$Expression))==trimws("NA")),NA,as.character(dt2$Expression)))
dt2$ForkLength <- ifelse((trimws(as.character(dt2$ForkLength))==trimws("NA")),NA,dt2$ForkLength)               
suppressWarnings(dt2$ForkLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ForkLength))==as.character(as.numeric("NA"))),NA,dt2$ForkLength))
dt2$RaceByLength <- as.factor(ifelse((trimws(as.character(dt2$RaceByLength))==trimws("NA")),NA,as.character(dt2$RaceByLength)))
dt2$TagCode <- as.factor(ifelse((trimws(as.character(dt2$TagCode))==trimws("NA")),NA,as.character(dt2$TagCode)))
dt2$RaceByTag <- as.factor(ifelse((trimws(as.character(dt2$RaceByTag))==trimws("NA")),NA,as.character(dt2$RaceByTag)))
dt2$ArchivalID <- as.factor(ifelse((trimws(as.character(dt2$ArchivalID))==trimws("NA")),NA,as.character(dt2$ArchivalID)))
dt2$SpecialStudyID <- as.factor(ifelse((trimws(as.character(dt2$SpecialStudyID))==trimws("NA")),NA,as.character(dt2$SpecialStudyID)))
dt2$GeneticID <- as.factor(ifelse((trimws(as.character(dt2$GeneticID))==trimws("NA")),NA,as.character(dt2$GeneticID)))
dt2$Probability1 <- ifelse((trimws(as.character(dt2$Probability1))==trimws("NA")),NA,dt2$Probability1)               
suppressWarnings(dt2$Probability1 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Probability1))==as.character(as.numeric("NA"))),NA,dt2$Probability1))
dt2$GeneticID2 <- as.factor(ifelse((trimws(as.character(dt2$GeneticID2))==trimws("NA")),NA,as.character(dt2$GeneticID2)))
dt2$Probability2 <- ifelse((trimws(as.character(dt2$Probability2))==trimws("NA")),NA,dt2$Probability2)               
suppressWarnings(dt2$Probability2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Probability2))==as.character(as.numeric("NA"))),NA,dt2$Probability2))
dt2$SexGeneID <- as.factor(ifelse((trimws(as.character(dt2$SexGeneID))==trimws("NA")),NA,as.character(dt2$SexGeneID)))
dt2$Ots28 <- as.factor(ifelse((trimws(as.character(dt2$Ots28))==trimws("NA")),NA,as.character(dt2$Ots28)))
dt2$Lab <- as.factor(ifelse((trimws(as.character(dt2$Lab))==trimws("NA")),NA,as.character(dt2$Lab)))
dt2$GeneticTest <- as.factor(ifelse((trimws(as.character(dt2$GeneticTest))==trimws("NA")),NA,as.character(dt2$GeneticTest)))
dt2$GeneticModel <- as.factor(ifelse((trimws(as.character(dt2$GeneticModel))==trimws("NA")),NA,as.character(dt2$GeneticModel)))
dt2$Count <- ifelse((trimws(as.character(dt2$Count))==trimws("NA")),NA,dt2$Count)               
suppressWarnings(dt2$Count <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Count))==as.character(as.numeric("NA"))),NA,dt2$Count))


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Location)
summary(RegionCode)
summary(StationCode)
summary(SampleDate)
summary(SampleTime)
summary(MethodCode)
summary(GearConditionCode)
summary(WeatherCode)
summary(DO)
summary(WaterTemp)
summary(Turbidity)
summary(Secchi)
summary(SpecificConductance)
summary(TowNumber)
summary(SamplingDirection)
summary(TowDuration)
summary(FlowDebris)
summary(SiteDisturbance)
summary(AlternateSite)
summary(SeineLength)
summary(SeineWidth)
summary(SeineDepth)
summary(FlowmeterStart)
summary(FlowmeterEnd)
summary(FlowmeterDifference)
summary(Volume)
summary(OrganismCode)
summary(IEPFishCode)
summary(CommonName)
summary(MarkCode)
summary(StageCode)
summary(Expression)
summary(ForkLength)
summary(RaceByLength)
summary(TagCode)
summary(RaceByTag)
summary(ArchivalID)
summary(SpecialStudyID)
summary(GeneticID)
summary(Probability1)
summary(GeneticID2)
summary(Probability2)
summary(SexGeneID)
summary(Ots28)
summary(Lab)
summary(GeneticTest)
summary(GeneticModel)
summary(Count) 
# Get more details on character variables

summary(as.factor(dt2$Location)) 
summary(as.factor(dt2$RegionCode)) 
summary(as.factor(dt2$StationCode)) 
summary(as.factor(dt2$MethodCode)) 
summary(as.factor(dt2$GearConditionCode)) 
summary(as.factor(dt2$WeatherCode)) 
summary(as.factor(dt2$SamplingDirection)) 
summary(as.factor(dt2$FlowDebris)) 
summary(as.factor(dt2$SiteDisturbance)) 
summary(as.factor(dt2$AlternateSite)) 
summary(as.factor(dt2$OrganismCode)) 
summary(as.factor(dt2$IEPFishCode)) 
summary(as.factor(dt2$CommonName)) 
summary(as.factor(dt2$MarkCode)) 
summary(as.factor(dt2$StageCode)) 
summary(as.factor(dt2$Expression)) 
summary(as.factor(dt2$RaceByLength)) 
summary(as.factor(dt2$TagCode)) 
summary(as.factor(dt2$RaceByTag)) 
summary(as.factor(dt2$ArchivalID)) 
summary(as.factor(dt2$SpecialStudyID)) 
summary(as.factor(dt2$GeneticID)) 
summary(as.factor(dt2$GeneticID2)) 
summary(as.factor(dt2$SexGeneID)) 
summary(as.factor(dt2$Ots28)) 
summary(as.factor(dt2$Lab)) 
summary(as.factor(dt2$GeneticTest)) 
summary(as.factor(dt2$GeneticModel))
detach(dt2)        

#######
# dt3 #
#######
# file from: "https://pasta.lternet.edu/package/data/eml/edi/244/9/147fd5e2c7db15913b2ffa44410dc7f9" 
dt3 <-read.csv("1976-2021_DJFMP_beach_seine_fish_and_water_quality_data.csv",header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Location",     
                 "RegionCode",     
                 "StationCode",     
                 "SampleDate",     
                 "SampleTime",     
                 "MethodCode",     
                 "GearConditionCode",     
                 "WeatherCode",     
                 "DO",     
                 "WaterTemp",     
                 "Turbidity",     
                 "Secchi",     
                 "SpecificConductance",     
                 "TowNumber",     
                 "SamplingDirection",     
                 "TowDuration",     
                 "FlowDebris",     
                 "SiteDisturbance",     
                 "AlternateSite",     
                 "SeineLength",     
                 "SeineWidth",     
                 "SeineDepth",     
                 "FlowmeterStart",     
                 "FlowmeterEnd",     
                 "FlowmeterDifference",     
                 "Volume",     
                 "OrganismCode",     
                 "IEPFishCode",     
                 "CommonName",     
                 "MarkCode",     
                 "StageCode",     
                 "Expression",     
                 "ForkLength",     
                 "RaceByLength",     
                 "TagCode",     
                 "RaceByTag",     
                 "ArchivalID",     
                 "SpecialStudyID",     
                 "GeneticID",     
                 "Probability1",     
                 "GeneticID2",     
                 "Probability2",     
                 "SexGeneID",     
                 "Ots28",     
                 "Lab",     
                 "GeneticTest",     
                 "GeneticModel",     
                 "Count"    ), check.names=TRUE)

#unlink(infile3)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$Location)!="factor") dt3$Location<- as.factor(dt3$Location)
if (class(dt3$RegionCode)!="factor") dt3$RegionCode<- as.factor(dt3$RegionCode)
if (class(dt3$StationCode)!="factor") dt3$StationCode<- as.factor(dt3$StationCode)                                   
# attempting to convert dt3$SampleDate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp3SampleDate<-as.Date(dt3$SampleDate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp3SampleDate) == length(tmp3SampleDate[!is.na(tmp3SampleDate)])){dt3$SampleDate <- tmp3SampleDate } else {print("Date conversion failed for dt3$SampleDate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp3SampleDate) 
if (class(dt3$MethodCode)!="factor") dt3$MethodCode<- as.factor(dt3$MethodCode)
if (class(dt3$GearConditionCode)!="factor") dt3$GearConditionCode<- as.factor(dt3$GearConditionCode)
if (class(dt3$WeatherCode)!="factor") dt3$WeatherCode<- as.factor(dt3$WeatherCode)
if (class(dt3$DO)=="factor") dt3$DO <-as.numeric(levels(dt3$DO))[as.integer(dt3$DO) ]               
if (class(dt3$DO)=="character") dt3$DO <-as.numeric(dt3$DO)
if (class(dt3$WaterTemp)=="factor") dt3$WaterTemp <-as.numeric(levels(dt3$WaterTemp))[as.integer(dt3$WaterTemp) ]               
if (class(dt3$WaterTemp)=="character") dt3$WaterTemp <-as.numeric(dt3$WaterTemp)
if (class(dt3$Turbidity)=="factor") dt3$Turbidity <-as.numeric(levels(dt3$Turbidity))[as.integer(dt3$Turbidity) ]               
if (class(dt3$Turbidity)=="character") dt3$Turbidity <-as.numeric(dt3$Turbidity)
if (class(dt3$Secchi)=="factor") dt3$Secchi <-as.numeric(levels(dt3$Secchi))[as.integer(dt3$Secchi) ]               
if (class(dt3$Secchi)=="character") dt3$Secchi <-as.numeric(dt3$Secchi)
if (class(dt3$SpecificConductance)=="factor") dt3$SpecificConductance <-as.numeric(levels(dt3$SpecificConductance))[as.integer(dt3$SpecificConductance) ]               
if (class(dt3$SpecificConductance)=="character") dt3$SpecificConductance <-as.numeric(dt3$SpecificConductance)
if (class(dt3$TowNumber)=="factor") dt3$TowNumber <-as.numeric(levels(dt3$TowNumber))[as.integer(dt3$TowNumber) ]               
if (class(dt3$TowNumber)=="character") dt3$TowNumber <-as.numeric(dt3$TowNumber)
if (class(dt3$SamplingDirection)!="factor") dt3$SamplingDirection<- as.factor(dt3$SamplingDirection)
if (class(dt3$TowDuration)=="factor") dt3$TowDuration <-as.numeric(levels(dt3$TowDuration))[as.integer(dt3$TowDuration) ]               
if (class(dt3$TowDuration)=="character") dt3$TowDuration <-as.numeric(dt3$TowDuration)
if (class(dt3$FlowDebris)!="factor") dt3$FlowDebris<- as.factor(dt3$FlowDebris)
if (class(dt3$SiteDisturbance)!="factor") dt3$SiteDisturbance<- as.factor(dt3$SiteDisturbance)
if (class(dt3$AlternateSite)!="factor") dt3$AlternateSite<- as.factor(dt3$AlternateSite)
if (class(dt3$SeineLength)=="factor") dt3$SeineLength <-as.numeric(levels(dt3$SeineLength))[as.integer(dt3$SeineLength) ]               
if (class(dt3$SeineLength)=="character") dt3$SeineLength <-as.numeric(dt3$SeineLength)
if (class(dt3$SeineWidth)=="factor") dt3$SeineWidth <-as.numeric(levels(dt3$SeineWidth))[as.integer(dt3$SeineWidth) ]               
if (class(dt3$SeineWidth)=="character") dt3$SeineWidth <-as.numeric(dt3$SeineWidth)
if (class(dt3$SeineDepth)=="factor") dt3$SeineDepth <-as.numeric(levels(dt3$SeineDepth))[as.integer(dt3$SeineDepth) ]               
if (class(dt3$SeineDepth)=="character") dt3$SeineDepth <-as.numeric(dt3$SeineDepth)
if (class(dt3$FlowmeterStart)=="factor") dt3$FlowmeterStart <-as.numeric(levels(dt3$FlowmeterStart))[as.integer(dt3$FlowmeterStart) ]               
if (class(dt3$FlowmeterStart)=="character") dt3$FlowmeterStart <-as.numeric(dt3$FlowmeterStart)
if (class(dt3$FlowmeterEnd)=="factor") dt3$FlowmeterEnd <-as.numeric(levels(dt3$FlowmeterEnd))[as.integer(dt3$FlowmeterEnd) ]               
if (class(dt3$FlowmeterEnd)=="character") dt3$FlowmeterEnd <-as.numeric(dt3$FlowmeterEnd)
if (class(dt3$FlowmeterDifference)=="factor") dt3$FlowmeterDifference <-as.numeric(levels(dt3$FlowmeterDifference))[as.integer(dt3$FlowmeterDifference) ]               
if (class(dt3$FlowmeterDifference)=="character") dt3$FlowmeterDifference <-as.numeric(dt3$FlowmeterDifference)
if (class(dt3$Volume)=="factor") dt3$Volume <-as.numeric(levels(dt3$Volume))[as.integer(dt3$Volume) ]               
if (class(dt3$Volume)=="character") dt3$Volume <-as.numeric(dt3$Volume)
if (class(dt3$OrganismCode)!="factor") dt3$OrganismCode<- as.factor(dt3$OrganismCode)
if (class(dt3$IEPFishCode)!="factor") dt3$IEPFishCode<- as.factor(dt3$IEPFishCode)
if (class(dt3$CommonName)!="factor") dt3$CommonName<- as.factor(dt3$CommonName)
if (class(dt3$MarkCode)!="factor") dt3$MarkCode<- as.factor(dt3$MarkCode)
if (class(dt3$StageCode)!="factor") dt3$StageCode<- as.factor(dt3$StageCode)
if (class(dt3$Expression)!="factor") dt3$Expression<- as.factor(dt3$Expression)
if (class(dt3$ForkLength)=="factor") dt3$ForkLength <-as.numeric(levels(dt3$ForkLength))[as.integer(dt3$ForkLength) ]               
if (class(dt3$ForkLength)=="character") dt3$ForkLength <-as.numeric(dt3$ForkLength)
if (class(dt3$RaceByLength)!="factor") dt3$RaceByLength<- as.factor(dt3$RaceByLength)
if (class(dt3$TagCode)!="factor") dt3$TagCode<- as.factor(dt3$TagCode)
if (class(dt3$RaceByTag)!="factor") dt3$RaceByTag<- as.factor(dt3$RaceByTag)
if (class(dt3$ArchivalID)!="factor") dt3$ArchivalID<- as.factor(dt3$ArchivalID)
if (class(dt3$SpecialStudyID)!="factor") dt3$SpecialStudyID<- as.factor(dt3$SpecialStudyID)
if (class(dt3$GeneticID)!="factor") dt3$GeneticID<- as.factor(dt3$GeneticID)
if (class(dt3$Probability1)=="factor") dt3$Probability1 <-as.numeric(levels(dt3$Probability1))[as.integer(dt3$Probability1) ]               
if (class(dt3$Probability1)=="character") dt3$Probability1 <-as.numeric(dt3$Probability1)
if (class(dt3$GeneticID2)!="factor") dt3$GeneticID2<- as.factor(dt3$GeneticID2)
if (class(dt3$Probability2)=="factor") dt3$Probability2 <-as.numeric(levels(dt3$Probability2))[as.integer(dt3$Probability2) ]               
if (class(dt3$Probability2)=="character") dt3$Probability2 <-as.numeric(dt3$Probability2)
if (class(dt3$SexGeneID)!="factor") dt3$SexGeneID<- as.factor(dt3$SexGeneID)
if (class(dt3$Ots28)!="factor") dt3$Ots28<- as.factor(dt3$Ots28)
if (class(dt3$Lab)!="factor") dt3$Lab<- as.factor(dt3$Lab)
if (class(dt3$GeneticTest)!="factor") dt3$GeneticTest<- as.factor(dt3$GeneticTest)
if (class(dt3$GeneticModel)!="factor") dt3$GeneticModel<- as.factor(dt3$GeneticModel)
if (class(dt3$Count)=="factor") dt3$Count <-as.numeric(levels(dt3$Count))[as.integer(dt3$Count) ]               
if (class(dt3$Count)=="character") dt3$Count <-as.numeric(dt3$Count)

# Convert Missing Values to NA for non-dates

dt3$Location <- as.factor(ifelse((trimws(as.character(dt3$Location))==trimws("NA")),NA,as.character(dt3$Location)))
dt3$RegionCode <- as.factor(ifelse((trimws(as.character(dt3$RegionCode))==trimws("NA")),NA,as.character(dt3$RegionCode)))
dt3$StationCode <- as.factor(ifelse((trimws(as.character(dt3$StationCode))==trimws("NA")),NA,as.character(dt3$StationCode)))
dt3$MethodCode <- as.factor(ifelse((trimws(as.character(dt3$MethodCode))==trimws("NA")),NA,as.character(dt3$MethodCode)))
dt3$GearConditionCode <- as.factor(ifelse((trimws(as.character(dt3$GearConditionCode))==trimws("NA")),NA,as.character(dt3$GearConditionCode)))
dt3$WeatherCode <- as.factor(ifelse((trimws(as.character(dt3$WeatherCode))==trimws("NA")),NA,as.character(dt3$WeatherCode)))
dt3$DO <- ifelse((trimws(as.character(dt3$DO))==trimws("NA")),NA,dt3$DO)               
suppressWarnings(dt3$DO <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$DO))==as.character(as.numeric("NA"))),NA,dt3$DO))
dt3$WaterTemp <- ifelse((trimws(as.character(dt3$WaterTemp))==trimws("NA")),NA,dt3$WaterTemp)               
suppressWarnings(dt3$WaterTemp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$WaterTemp))==as.character(as.numeric("NA"))),NA,dt3$WaterTemp))
dt3$Turbidity <- ifelse((trimws(as.character(dt3$Turbidity))==trimws("NA")),NA,dt3$Turbidity)               
suppressWarnings(dt3$Turbidity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Turbidity))==as.character(as.numeric("NA"))),NA,dt3$Turbidity))
dt3$Secchi <- ifelse((trimws(as.character(dt3$Secchi))==trimws("NA")),NA,dt3$Secchi)               
suppressWarnings(dt3$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Secchi))==as.character(as.numeric("NA"))),NA,dt3$Secchi))
dt3$SpecificConductance <- ifelse((trimws(as.character(dt3$SpecificConductance))==trimws("NA")),NA,dt3$SpecificConductance)               
suppressWarnings(dt3$SpecificConductance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$SpecificConductance))==as.character(as.numeric("NA"))),NA,dt3$SpecificConductance))
dt3$TowNumber <- ifelse((trimws(as.character(dt3$TowNumber))==trimws("NA")),NA,dt3$TowNumber)               
suppressWarnings(dt3$TowNumber <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$TowNumber))==as.character(as.numeric("NA"))),NA,dt3$TowNumber))
dt3$SamplingDirection <- as.factor(ifelse((trimws(as.character(dt3$SamplingDirection))==trimws("NA")),NA,as.character(dt3$SamplingDirection)))
dt3$TowDuration <- ifelse((trimws(as.character(dt3$TowDuration))==trimws("NA")),NA,dt3$TowDuration)               
suppressWarnings(dt3$TowDuration <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$TowDuration))==as.character(as.numeric("NA"))),NA,dt3$TowDuration))
dt3$FlowDebris <- as.factor(ifelse((trimws(as.character(dt3$FlowDebris))==trimws("NA")),NA,as.character(dt3$FlowDebris)))
dt3$SiteDisturbance <- as.factor(ifelse((trimws(as.character(dt3$SiteDisturbance))==trimws("NA")),NA,as.character(dt3$SiteDisturbance)))
dt3$AlternateSite <- as.factor(ifelse((trimws(as.character(dt3$AlternateSite))==trimws("NA")),NA,as.character(dt3$AlternateSite)))
dt3$SeineLength <- ifelse((trimws(as.character(dt3$SeineLength))==trimws("NA")),NA,dt3$SeineLength)               
suppressWarnings(dt3$SeineLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$SeineLength))==as.character(as.numeric("NA"))),NA,dt3$SeineLength))
dt3$SeineWidth <- ifelse((trimws(as.character(dt3$SeineWidth))==trimws("NA")),NA,dt3$SeineWidth)               
suppressWarnings(dt3$SeineWidth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$SeineWidth))==as.character(as.numeric("NA"))),NA,dt3$SeineWidth))
dt3$SeineDepth <- ifelse((trimws(as.character(dt3$SeineDepth))==trimws("NA")),NA,dt3$SeineDepth)               
suppressWarnings(dt3$SeineDepth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$SeineDepth))==as.character(as.numeric("NA"))),NA,dt3$SeineDepth))
dt3$FlowmeterStart <- ifelse((trimws(as.character(dt3$FlowmeterStart))==trimws("NA")),NA,dt3$FlowmeterStart)               
suppressWarnings(dt3$FlowmeterStart <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$FlowmeterStart))==as.character(as.numeric("NA"))),NA,dt3$FlowmeterStart))
dt3$FlowmeterEnd <- ifelse((trimws(as.character(dt3$FlowmeterEnd))==trimws("NA")),NA,dt3$FlowmeterEnd)               
suppressWarnings(dt3$FlowmeterEnd <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$FlowmeterEnd))==as.character(as.numeric("NA"))),NA,dt3$FlowmeterEnd))
dt3$FlowmeterDifference <- ifelse((trimws(as.character(dt3$FlowmeterDifference))==trimws("NA")),NA,dt3$FlowmeterDifference)               
suppressWarnings(dt3$FlowmeterDifference <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$FlowmeterDifference))==as.character(as.numeric("NA"))),NA,dt3$FlowmeterDifference))
dt3$Volume <- ifelse((trimws(as.character(dt3$Volume))==trimws("NA")),NA,dt3$Volume)               
suppressWarnings(dt3$Volume <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Volume))==as.character(as.numeric("NA"))),NA,dt3$Volume))
dt3$OrganismCode <- as.factor(ifelse((trimws(as.character(dt3$OrganismCode))==trimws("NA")),NA,as.character(dt3$OrganismCode)))
dt3$IEPFishCode <- as.factor(ifelse((trimws(as.character(dt3$IEPFishCode))==trimws("NA")),NA,as.character(dt3$IEPFishCode)))
dt3$CommonName <- as.factor(ifelse((trimws(as.character(dt3$CommonName))==trimws("NA")),NA,as.character(dt3$CommonName)))
dt3$MarkCode <- as.factor(ifelse((trimws(as.character(dt3$MarkCode))==trimws("NA")),NA,as.character(dt3$MarkCode)))
dt3$StageCode <- as.factor(ifelse((trimws(as.character(dt3$StageCode))==trimws("NA")),NA,as.character(dt3$StageCode)))
dt3$Expression <- as.factor(ifelse((trimws(as.character(dt3$Expression))==trimws("NA")),NA,as.character(dt3$Expression)))
dt3$ForkLength <- ifelse((trimws(as.character(dt3$ForkLength))==trimws("NA")),NA,dt3$ForkLength)               
suppressWarnings(dt3$ForkLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$ForkLength))==as.character(as.numeric("NA"))),NA,dt3$ForkLength))
dt3$RaceByLength <- as.factor(ifelse((trimws(as.character(dt3$RaceByLength))==trimws("NA")),NA,as.character(dt3$RaceByLength)))
dt3$TagCode <- as.factor(ifelse((trimws(as.character(dt3$TagCode))==trimws("NA")),NA,as.character(dt3$TagCode)))
dt3$RaceByTag <- as.factor(ifelse((trimws(as.character(dt3$RaceByTag))==trimws("NA")),NA,as.character(dt3$RaceByTag)))
dt3$ArchivalID <- as.factor(ifelse((trimws(as.character(dt3$ArchivalID))==trimws("NA")),NA,as.character(dt3$ArchivalID)))
dt3$SpecialStudyID <- as.factor(ifelse((trimws(as.character(dt3$SpecialStudyID))==trimws("NA")),NA,as.character(dt3$SpecialStudyID)))
dt3$GeneticID <- as.factor(ifelse((trimws(as.character(dt3$GeneticID))==trimws("NA")),NA,as.character(dt3$GeneticID)))
dt3$Probability1 <- ifelse((trimws(as.character(dt3$Probability1))==trimws("NA")),NA,dt3$Probability1)               
suppressWarnings(dt3$Probability1 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Probability1))==as.character(as.numeric("NA"))),NA,dt3$Probability1))
dt3$GeneticID2 <- as.factor(ifelse((trimws(as.character(dt3$GeneticID2))==trimws("NA")),NA,as.character(dt3$GeneticID2)))
dt3$Probability2 <- ifelse((trimws(as.character(dt3$Probability2))==trimws("NA")),NA,dt3$Probability2)               
suppressWarnings(dt3$Probability2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Probability2))==as.character(as.numeric("NA"))),NA,dt3$Probability2))
dt3$SexGeneID <- as.factor(ifelse((trimws(as.character(dt3$SexGeneID))==trimws("NA")),NA,as.character(dt3$SexGeneID)))
dt3$Ots28 <- as.factor(ifelse((trimws(as.character(dt3$Ots28))==trimws("NA")),NA,as.character(dt3$Ots28)))
dt3$Lab <- as.factor(ifelse((trimws(as.character(dt3$Lab))==trimws("NA")),NA,as.character(dt3$Lab)))
dt3$GeneticTest <- as.factor(ifelse((trimws(as.character(dt3$GeneticTest))==trimws("NA")),NA,as.character(dt3$GeneticTest)))
dt3$GeneticModel <- as.factor(ifelse((trimws(as.character(dt3$GeneticModel))==trimws("NA")),NA,as.character(dt3$GeneticModel)))
dt3$Count <- ifelse((trimws(as.character(dt3$Count))==trimws("NA")),NA,dt3$Count)               
suppressWarnings(dt3$Count <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Count))==as.character(as.numeric("NA"))),NA,dt3$Count))


# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Location)
summary(RegionCode)
summary(StationCode)
summary(SampleDate)
summary(SampleTime)
summary(MethodCode)
summary(GearConditionCode)
summary(WeatherCode)
summary(DO)
summary(WaterTemp)
summary(Turbidity)
summary(Secchi)
summary(SpecificConductance)
summary(TowNumber)
summary(SamplingDirection)
summary(TowDuration)
summary(FlowDebris)
summary(SiteDisturbance)
summary(AlternateSite)
summary(SeineLength)
summary(SeineWidth)
summary(SeineDepth)
summary(FlowmeterStart)
summary(FlowmeterEnd)
summary(FlowmeterDifference)
summary(Volume)
summary(OrganismCode)
summary(IEPFishCode)
summary(CommonName)
summary(MarkCode)
summary(StageCode)
summary(Expression)
summary(ForkLength)
summary(RaceByLength)
summary(TagCode)
summary(RaceByTag)
summary(ArchivalID)
summary(SpecialStudyID)
summary(GeneticID)
summary(Probability1)
summary(GeneticID2)
summary(Probability2)
summary(SexGeneID)
summary(Ots28)
summary(Lab)
summary(GeneticTest)
summary(GeneticModel)
summary(Count) 
# Get more details on character variables

summary(as.factor(dt3$Location)) 
summary(as.factor(dt3$RegionCode)) 
summary(as.factor(dt3$StationCode)) 
summary(as.factor(dt3$MethodCode)) 
summary(as.factor(dt3$GearConditionCode)) 
summary(as.factor(dt3$WeatherCode)) 
summary(as.factor(dt3$SamplingDirection)) 
summary(as.factor(dt3$FlowDebris)) 
summary(as.factor(dt3$SiteDisturbance)) 
summary(as.factor(dt3$AlternateSite)) 
summary(as.factor(dt3$OrganismCode)) 
summary(as.factor(dt3$IEPFishCode)) 
summary(as.factor(dt3$CommonName)) 
summary(as.factor(dt3$MarkCode)) 
summary(as.factor(dt3$StageCode)) 
summary(as.factor(dt3$Expression)) 
summary(as.factor(dt3$RaceByLength)) 
summary(as.factor(dt3$TagCode)) 
summary(as.factor(dt3$RaceByTag)) 
summary(as.factor(dt3$ArchivalID)) 
summary(as.factor(dt3$SpecialStudyID)) 
summary(as.factor(dt3$GeneticID)) 
summary(as.factor(dt3$GeneticID2)) 
summary(as.factor(dt3$SexGeneID)) 
summary(as.factor(dt3$Ots28)) 
summary(as.factor(dt3$Lab)) 
summary(as.factor(dt3$GeneticTest)) 
summary(as.factor(dt3$GeneticModel))
detach(dt3)               

######################

#####################
# data manipulation #
#####################

# merge the two time periods together
#colnames(dt2)==colnames(dt3)
dataset<-rbind(dt2,dt3)
#remove(dt2,dt3)

# remove all the unnecessary middle columns
#dataset <- dataset[,c("Location","StationCode", "SampleDate",
#                      "SampleTime", "MethodCode", "OrganismCode",       
#                      "IEPFishCode",         "CommonName",          "MarkCode",           
#                      "StageCode",           "Expression",          "ForkLength",         
#                      "RaceByLength",        "TagCode",             "RaceByTag",          
#                      "ArchivalID",          "SpecialStudyID",      "GeneticID",          
#                      "Probability1",        "GeneticID2",          "Probability2",       
#                      "SexGeneID",           "Ots28",               "Lab",                
#                      "GeneticTest",         "GeneticModel",        "Count" )]

# Note that View will truncate some of the columns (but they are still there!)

# Add Water Year
dataset$WY <- year(dataset$SampleDate) + 
  as.numeric(between(month(dataset$SampleDate),10,12))
# Add water year type & drought period year type
dataset <- left_join(dataset,DroughtYr[,c(1:4)], by="WY")

# clip dataset to stations we want: Chipps Island and Sherwood Trawl
# and only trawl data
Trawl <- dataset[((dataset$Location %in% c("Chipps Island","Sherwood Harbor")))&
                   ((dataset$MethodCode %in% c("KDTR","MWTR"))),]
  #Trawl <- dataset[dataset$Location=="Chipps Island",]
  #unique(Trawl[Trawl$MethodCode %in% c("KDTR","MWTR"),]$StationCode)
#unique(Trawl$Location)
#unique(Trawl$MethodCode)

#remove(dataset)

# Species = combine CommonName + RaceByLength to get species/run
Trawl$Species <- as.character(Trawl$CommonName)
Trawl$Species[Trawl$CommonName=="Chinook salmon"] <- paste(
  Trawl$RaceByLength[Trawl$CommonName=="Chinook salmon"],"-run ",
  "Chinook salmon",sep="")

################################################
# creating a dataframe of sample dates         #
# want a row for every species/run of interest #
# for every sample date                        #
################################################
Trawl_sampledates <-Trawl %>%
  group_by(SampleDate) %>%
  dplyr::summarize(
    WY = WY,
    Location = Location,
    StationCode = StationCode,
    SampleDate = SampleDate,
    MethodCode = MethodCode,
  ) 
#remove duplicate rows
Trawl_sampledates <- Trawl_sampledates[!duplicated(Trawl_sampledates),]
#length(Trawl_sampledates$SampleDate)
#length(unique(Trawl_sampledates$SampleDate))

# need this whole df for every species
# add Species
Trawl_sampledates$Species <- character(length(Trawl_sampledates$SampleDate))

# want juvenile NMFS ESA/MSA species
species_list <- c("Fall-run Chinook salmon",
                  "NA-run Chinook salmon",
                  "Winter-run Chinook salmon",
                  "steelhead trout",
                  "LateFall-run Chinook salmon",
                  "Spring-run Chinook salmon")

# create dataframe with a row for each date for each species of interest
for (i in species_list){
  samples <- Trawl_sampledates
  samples$Species <- i
  
  
  if(i==species_list[1]){ # if first in list, create data frame
    interest_samples <- samples
  } else { # if data frame exists, add to it
    interest_samples <- rbind(interest_samples, samples)
  }
  
}
remove(samples)
#add Count to samples dataframe
interest_samples$Count <- 0

#############################################
# combine data w/ dates/species of interest #
#############################################

# clip data to the same columns as my sample/date/species list
# clip to species of interest
Trawl_sums <- Trawl[(Trawl$Species %in% species_list),c(colnames(Trawl_sampledates),"Count")]

# combine the list of all sampledate rows of interest species
# with the list of actual data
Trawl_sums <- rbind(Trawl_sums,interest_samples) # *******

# create a date and species variable
Trawl_sums$DateSpecies <- paste(Trawl_sums$SampleDate,
                                   Trawl_sums$Species)


# sum over species+date
Trawl_summary <- Trawl_sums %>%
  group_by(DateSpecies) %>%
  dplyr::summarize(
    SampleDate = SampleDate,
    WY = WY,
    Location = Location,
    StationCode = StationCode,
    MethodCode = MethodCode,
    Species = Species,
    Catch_sum = sum(Count)
  ) 
#remove duplicate rows
Trawl_summary <-Trawl_summary[!duplicated(Trawl_summary),]

############################# ***need to update
#        FINAL PLOTS        #
#        WR, SR, FR         #
# no late fall or steelhead #
#     timing and lengths    #
#############################

Chinook_all.expanded2 <- Chinook_all.expanded[
  ((Chinook_all.expanded$RaceByLength %in% c("Winter","Fall",
                                             "Spring"))&
     Chinook_all.expanded$WY > 1987),]

# Migration timing
# Sherwood Harbor
ggplot(Chinook_all.expanded2[Chinook_all.expanded2$Location=="Sherwood Harbor",],
       aes(x = wtr_day, y = Species_Run, 
           color = Drought, 
           fill = Drought)) +scale_fill_manual(values=pal_drought) +
  scale_color_manual(values=pal_drought) +
  geom_density_ridges(alpha = .5, scale=0.95) +
  labs(title = "Sherwood Harbor Trawl Migration Timing (1988-2021)", 
       x = "Day of the water year", 
       y = "Run type") + theme_bw()
dev.copy(svg,"C:/Users/Elizabeth.keller/Desktop/DCP/Drought data/FWS DJFMP/plots/Sherwood final timing.svg")
dev.off()
# Chipps Island
ggplot(Chinook_all.expanded2[Chinook_all.expanded2$Location=="Chipps Island",],
       aes(x = wtr_day, y = Species_Run, 
           color = Drought, 
           fill = Drought)) +scale_fill_manual(values=pal_drought) +
  scale_color_manual(values=pal_drought) +
  geom_density_ridges(alpha = .5, scale=0.95) +
  labs(title = "Chipps Island Trawl Migration Timing (1988-2021)", 
       x = "Day of the water year", 
       y = "Run type") + theme_bw()
dev.copy(svg,"C:/Users/Elizabeth.keller/Desktop/DCP/Drought data/FWS DJFMP/plots/Chipps final timing.svg")
dev.off()

# Lengths
Chinook_all.expanded3 <- Chinook_all.expanded2[Chinook_all.expanded2$FL>0,]
# Sherwood Harbor
ggplot(Chinook_all.expanded3[Chinook_all.expanded3$Location=="Sherwood Harbor",],
       aes(x = FL, y = Species_Run, 
           color = Drought, 
           fill = Drought)) +scale_fill_manual(values=pal_drought) +
  scale_color_manual(values=pal_drought) +
  geom_density_ridges(alpha = .5, scale=0.95) +
  labs(title = "Sherwood Harbor Trawl Fish Lengths (1988-2021)", 
       x = "Fork length", 
       y = "Run type") + theme_bw()
dev.copy(svg,"C:/Users/Elizabeth.keller/Desktop/DCP/Drought data/FWS DJFMP/plots/Sherwood final lengths.svg")
dev.off()
# Chipps Island
ggplot(Chinook_all.expanded3[Chinook_all.expanded3$Location=="Chipps Island",],
       aes(x = FL, y = Species_Run, 
           color = Drought, 
           fill = Drought)) +scale_fill_manual(values=pal_drought) +
  scale_color_manual(values=pal_drought) +
  geom_density_ridges(alpha = .5, scale=0.95) +
  labs(title = "Chipps Island Trawl Fish Lengths (1988-2021)", 
       x = "Fork length", 
       y = "Run type") + theme_bw()
dev.copy(svg,"C:/Users/Elizabeth.keller/Desktop/DCP/Drought data/FWS DJFMP/plots/Chipps final lengths.svg")
dev.off()

# winter-run lengths only - both trawls
ggplot(Chinook_all.expanded3[Chinook_all.expanded3$RaceByLength=="Winter"&
                               (Chinook_all.expanded3$Location=="Chipps Island" |
                                  Chinook_all.expanded3$Location=="Sherwood Harbor"),],
       aes(x = FL, y = Location, 
           color = Drought, 
           fill = Drought)) +scale_fill_manual(values=pal_drought) +
  scale_color_manual(values=pal_drought) +
  geom_density_ridges(alpha = .5, scale=0.7) +
  labs(title = "Chipps Island Trawl Winter-run Chinook Lengths (1988-2021)", 
       x = "Fork length", 
       y = "Trawl Location") + theme_bw()
dev.copy(svg,"C:/Users/Elizabeth.keller/Desktop/DCP/Drought data/FWS DJFMP/plots/Winter-run lengths.svg")
dev.off()

############
# sample sizes
# sample sizes *for migration timing*
# lengths has less due to zeros
Chipps <- Chinook_all.expanded2[Chinook_all.expanded2$Location=="Chipps Island",]
table(Chipps$Species_Run) # not matching up

Sherwood <- Chinook_all.expanded2[Chinook_all.expanded2$Location=="Sherwood Harbor",]
table(Sherwood$Species_Run)

# for lengths
Chipps <- Chinook_all.expanded3[Chinook_all.expanded3$Location=="Chipps Island",]
table(Chipps$Species_Run)
Sherwood <- Chinook_all.expanded3[Chinook_all.expanded3$Location=="Sherwood Harbor",]
table(Sherwood$Species_Run)