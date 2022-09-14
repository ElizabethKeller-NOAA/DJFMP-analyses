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

# for this analysis, need only dt1 and dt2 from their script

#######
# dt1 #
#######
# file from "https://pasta.lternet.edu/package/data/eml/edi/244/9/71c16ead9b8ffa4da7a52da180f601f4" 
dt1 <-read.csv("1976-2001_DJFMP_trawl_fish_and_water_quality_data.csv",header=F 
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

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Location)!="factor") dt1$Location<- as.factor(dt1$Location)
if (class(dt1$RegionCode)!="factor") dt1$RegionCode<- as.factor(dt1$RegionCode)
if (class(dt1$StationCode)!="factor") dt1$StationCode<- as.factor(dt1$StationCode)                                   
# attempting to convert dt1$SampleDate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1SampleDate<-as.Date(dt1$SampleDate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1SampleDate) == length(tmp1SampleDate[!is.na(tmp1SampleDate)])){dt1$SampleDate <- tmp1SampleDate } else {print("Date conversion failed for dt1$SampleDate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1SampleDate) 
if (class(dt1$MethodCode)!="factor") dt1$MethodCode<- as.factor(dt1$MethodCode)
if (class(dt1$GearConditionCode)!="factor") dt1$GearConditionCode<- as.factor(dt1$GearConditionCode)
if (class(dt1$WeatherCode)!="factor") dt1$WeatherCode<- as.factor(dt1$WeatherCode)
if (class(dt1$DO)=="factor") dt1$DO <-as.numeric(levels(dt1$DO))[as.integer(dt1$DO) ]               
if (class(dt1$DO)=="character") dt1$DO <-as.numeric(dt1$DO)
if (class(dt1$WaterTemp)=="factor") dt1$WaterTemp <-as.numeric(levels(dt1$WaterTemp))[as.integer(dt1$WaterTemp) ]               
if (class(dt1$WaterTemp)=="character") dt1$WaterTemp <-as.numeric(dt1$WaterTemp)
if (class(dt1$Turbidity)=="factor") dt1$Turbidity <-as.numeric(levels(dt1$Turbidity))[as.integer(dt1$Turbidity) ]               
if (class(dt1$Turbidity)=="character") dt1$Turbidity <-as.numeric(dt1$Turbidity)
if (class(dt1$Secchi)=="factor") dt1$Secchi <-as.numeric(levels(dt1$Secchi))[as.integer(dt1$Secchi) ]               
if (class(dt1$Secchi)=="character") dt1$Secchi <-as.numeric(dt1$Secchi)
if (class(dt1$SpecificConductance)=="factor") dt1$SpecificConductance <-as.numeric(levels(dt1$SpecificConductance))[as.integer(dt1$SpecificConductance) ]               
if (class(dt1$SpecificConductance)=="character") dt1$SpecificConductance <-as.numeric(dt1$SpecificConductance)
if (class(dt1$TowNumber)=="factor") dt1$TowNumber <-as.numeric(levels(dt1$TowNumber))[as.integer(dt1$TowNumber) ]               
if (class(dt1$TowNumber)=="character") dt1$TowNumber <-as.numeric(dt1$TowNumber)
if (class(dt1$SamplingDirection)!="factor") dt1$SamplingDirection<- as.factor(dt1$SamplingDirection)
if (class(dt1$TowDuration)=="factor") dt1$TowDuration <-as.numeric(levels(dt1$TowDuration))[as.integer(dt1$TowDuration) ]               
if (class(dt1$TowDuration)=="character") dt1$TowDuration <-as.numeric(dt1$TowDuration)
if (class(dt1$FlowDebris)!="factor") dt1$FlowDebris<- as.factor(dt1$FlowDebris)
if (class(dt1$SiteDisturbance)!="factor") dt1$SiteDisturbance<- as.factor(dt1$SiteDisturbance)
if (class(dt1$AlternateSite)!="factor") dt1$AlternateSite<- as.factor(dt1$AlternateSite)
if (class(dt1$SeineLength)=="factor") dt1$SeineLength <-as.numeric(levels(dt1$SeineLength))[as.integer(dt1$SeineLength) ]               
if (class(dt1$SeineLength)=="character") dt1$SeineLength <-as.numeric(dt1$SeineLength)
if (class(dt1$SeineWidth)=="factor") dt1$SeineWidth <-as.numeric(levels(dt1$SeineWidth))[as.integer(dt1$SeineWidth) ]               
if (class(dt1$SeineWidth)=="character") dt1$SeineWidth <-as.numeric(dt1$SeineWidth)
if (class(dt1$SeineDepth)=="factor") dt1$SeineDepth <-as.numeric(levels(dt1$SeineDepth))[as.integer(dt1$SeineDepth) ]               
if (class(dt1$SeineDepth)=="character") dt1$SeineDepth <-as.numeric(dt1$SeineDepth)
if (class(dt1$FlowmeterStart)=="factor") dt1$FlowmeterStart <-as.numeric(levels(dt1$FlowmeterStart))[as.integer(dt1$FlowmeterStart) ]               
if (class(dt1$FlowmeterStart)=="character") dt1$FlowmeterStart <-as.numeric(dt1$FlowmeterStart)
if (class(dt1$FlowmeterEnd)=="factor") dt1$FlowmeterEnd <-as.numeric(levels(dt1$FlowmeterEnd))[as.integer(dt1$FlowmeterEnd) ]               
if (class(dt1$FlowmeterEnd)=="character") dt1$FlowmeterEnd <-as.numeric(dt1$FlowmeterEnd)
if (class(dt1$FlowmeterDifference)=="factor") dt1$FlowmeterDifference <-as.numeric(levels(dt1$FlowmeterDifference))[as.integer(dt1$FlowmeterDifference) ]               
if (class(dt1$FlowmeterDifference)=="character") dt1$FlowmeterDifference <-as.numeric(dt1$FlowmeterDifference)
if (class(dt1$Volume)=="factor") dt1$Volume <-as.numeric(levels(dt1$Volume))[as.integer(dt1$Volume) ]               
if (class(dt1$Volume)=="character") dt1$Volume <-as.numeric(dt1$Volume)
if (class(dt1$OrganismCode)!="factor") dt1$OrganismCode<- as.factor(dt1$OrganismCode)
if (class(dt1$IEPFishCode)!="factor") dt1$IEPFishCode<- as.factor(dt1$IEPFishCode)
if (class(dt1$CommonName)!="factor") dt1$CommonName<- as.factor(dt1$CommonName)
if (class(dt1$MarkCode)!="factor") dt1$MarkCode<- as.factor(dt1$MarkCode)
if (class(dt1$StageCode)!="factor") dt1$StageCode<- as.factor(dt1$StageCode)
if (class(dt1$Expression)!="factor") dt1$Expression<- as.factor(dt1$Expression)
if (class(dt1$ForkLength)=="factor") dt1$ForkLength <-as.numeric(levels(dt1$ForkLength))[as.integer(dt1$ForkLength) ]               
if (class(dt1$ForkLength)=="character") dt1$ForkLength <-as.numeric(dt1$ForkLength)
if (class(dt1$RaceByLength)!="factor") dt1$RaceByLength<- as.factor(dt1$RaceByLength)
if (class(dt1$TagCode)!="factor") dt1$TagCode<- as.factor(dt1$TagCode)
if (class(dt1$RaceByTag)!="factor") dt1$RaceByTag<- as.factor(dt1$RaceByTag)
if (class(dt1$ArchivalID)!="factor") dt1$ArchivalID<- as.factor(dt1$ArchivalID)
if (class(dt1$SpecialStudyID)!="factor") dt1$SpecialStudyID<- as.factor(dt1$SpecialStudyID)
if (class(dt1$GeneticID)!="factor") dt1$GeneticID<- as.factor(dt1$GeneticID)
if (class(dt1$Probability1)=="factor") dt1$Probability1 <-as.numeric(levels(dt1$Probability1))[as.integer(dt1$Probability1) ]               
if (class(dt1$Probability1)=="character") dt1$Probability1 <-as.numeric(dt1$Probability1)
if (class(dt1$GeneticID2)!="factor") dt1$GeneticID2<- as.factor(dt1$GeneticID2)
if (class(dt1$Probability2)=="factor") dt1$Probability2 <-as.numeric(levels(dt1$Probability2))[as.integer(dt1$Probability2) ]               
if (class(dt1$Probability2)=="character") dt1$Probability2 <-as.numeric(dt1$Probability2)
if (class(dt1$SexGeneID)!="factor") dt1$SexGeneID<- as.factor(dt1$SexGeneID)
if (class(dt1$Ots28)!="factor") dt1$Ots28<- as.factor(dt1$Ots28)
if (class(dt1$Lab)!="factor") dt1$Lab<- as.factor(dt1$Lab)
if (class(dt1$GeneticTest)!="factor") dt1$GeneticTest<- as.factor(dt1$GeneticTest)
if (class(dt1$GeneticModel)!="factor") dt1$GeneticModel<- as.factor(dt1$GeneticModel)
if (class(dt1$Count)=="factor") dt1$Count <-as.numeric(levels(dt1$Count))[as.integer(dt1$Count) ]               
if (class(dt1$Count)=="character") dt1$Count <-as.numeric(dt1$Count)

# Convert Missing Values to NA for non-dates

dt1$Location <- as.factor(ifelse((trimws(as.character(dt1$Location))==trimws("NA")),NA,as.character(dt1$Location)))
dt1$RegionCode <- as.factor(ifelse((trimws(as.character(dt1$RegionCode))==trimws("NA")),NA,as.character(dt1$RegionCode)))
dt1$StationCode <- as.factor(ifelse((trimws(as.character(dt1$StationCode))==trimws("NA")),NA,as.character(dt1$StationCode)))
dt1$MethodCode <- as.factor(ifelse((trimws(as.character(dt1$MethodCode))==trimws("NA")),NA,as.character(dt1$MethodCode)))
dt1$GearConditionCode <- as.factor(ifelse((trimws(as.character(dt1$GearConditionCode))==trimws("NA")),NA,as.character(dt1$GearConditionCode)))
dt1$WeatherCode <- as.factor(ifelse((trimws(as.character(dt1$WeatherCode))==trimws("NA")),NA,as.character(dt1$WeatherCode)))
dt1$DO <- ifelse((trimws(as.character(dt1$DO))==trimws("NA")),NA,dt1$DO)               
suppressWarnings(dt1$DO <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DO))==as.character(as.numeric("NA"))),NA,dt1$DO))
dt1$WaterTemp <- ifelse((trimws(as.character(dt1$WaterTemp))==trimws("NA")),NA,dt1$WaterTemp)               
suppressWarnings(dt1$WaterTemp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$WaterTemp))==as.character(as.numeric("NA"))),NA,dt1$WaterTemp))
dt1$Turbidity <- ifelse((trimws(as.character(dt1$Turbidity))==trimws("NA")),NA,dt1$Turbidity)               
suppressWarnings(dt1$Turbidity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Turbidity))==as.character(as.numeric("NA"))),NA,dt1$Turbidity))
dt1$Secchi <- ifelse((trimws(as.character(dt1$Secchi))==trimws("NA")),NA,dt1$Secchi)               
suppressWarnings(dt1$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Secchi))==as.character(as.numeric("NA"))),NA,dt1$Secchi))
dt1$SpecificConductance <- ifelse((trimws(as.character(dt1$SpecificConductance))==trimws("NA")),NA,dt1$SpecificConductance)               
suppressWarnings(dt1$SpecificConductance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SpecificConductance))==as.character(as.numeric("NA"))),NA,dt1$SpecificConductance))
dt1$TowNumber <- ifelse((trimws(as.character(dt1$TowNumber))==trimws("NA")),NA,dt1$TowNumber)               
suppressWarnings(dt1$TowNumber <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TowNumber))==as.character(as.numeric("NA"))),NA,dt1$TowNumber))
dt1$SamplingDirection <- as.factor(ifelse((trimws(as.character(dt1$SamplingDirection))==trimws("NA")),NA,as.character(dt1$SamplingDirection)))
dt1$TowDuration <- ifelse((trimws(as.character(dt1$TowDuration))==trimws("NA")),NA,dt1$TowDuration)               
suppressWarnings(dt1$TowDuration <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TowDuration))==as.character(as.numeric("NA"))),NA,dt1$TowDuration))
dt1$FlowDebris <- as.factor(ifelse((trimws(as.character(dt1$FlowDebris))==trimws("NA")),NA,as.character(dt1$FlowDebris)))
dt1$SiteDisturbance <- as.factor(ifelse((trimws(as.character(dt1$SiteDisturbance))==trimws("NA")),NA,as.character(dt1$SiteDisturbance)))
dt1$AlternateSite <- as.factor(ifelse((trimws(as.character(dt1$AlternateSite))==trimws("NA")),NA,as.character(dt1$AlternateSite)))
dt1$SeineLength <- ifelse((trimws(as.character(dt1$SeineLength))==trimws("NA")),NA,dt1$SeineLength)               
suppressWarnings(dt1$SeineLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SeineLength))==as.character(as.numeric("NA"))),NA,dt1$SeineLength))
dt1$SeineWidth <- ifelse((trimws(as.character(dt1$SeineWidth))==trimws("NA")),NA,dt1$SeineWidth)               
suppressWarnings(dt1$SeineWidth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SeineWidth))==as.character(as.numeric("NA"))),NA,dt1$SeineWidth))
dt1$SeineDepth <- ifelse((trimws(as.character(dt1$SeineDepth))==trimws("NA")),NA,dt1$SeineDepth)               
suppressWarnings(dt1$SeineDepth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SeineDepth))==as.character(as.numeric("NA"))),NA,dt1$SeineDepth))
dt1$FlowmeterStart <- ifelse((trimws(as.character(dt1$FlowmeterStart))==trimws("NA")),NA,dt1$FlowmeterStart)               
suppressWarnings(dt1$FlowmeterStart <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$FlowmeterStart))==as.character(as.numeric("NA"))),NA,dt1$FlowmeterStart))
dt1$FlowmeterEnd <- ifelse((trimws(as.character(dt1$FlowmeterEnd))==trimws("NA")),NA,dt1$FlowmeterEnd)               
suppressWarnings(dt1$FlowmeterEnd <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$FlowmeterEnd))==as.character(as.numeric("NA"))),NA,dt1$FlowmeterEnd))
dt1$FlowmeterDifference <- ifelse((trimws(as.character(dt1$FlowmeterDifference))==trimws("NA")),NA,dt1$FlowmeterDifference)               
suppressWarnings(dt1$FlowmeterDifference <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$FlowmeterDifference))==as.character(as.numeric("NA"))),NA,dt1$FlowmeterDifference))
dt1$Volume <- ifelse((trimws(as.character(dt1$Volume))==trimws("NA")),NA,dt1$Volume)               
suppressWarnings(dt1$Volume <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Volume))==as.character(as.numeric("NA"))),NA,dt1$Volume))
dt1$OrganismCode <- as.factor(ifelse((trimws(as.character(dt1$OrganismCode))==trimws("NA")),NA,as.character(dt1$OrganismCode)))
dt1$IEPFishCode <- as.factor(ifelse((trimws(as.character(dt1$IEPFishCode))==trimws("NA")),NA,as.character(dt1$IEPFishCode)))
dt1$CommonName <- as.factor(ifelse((trimws(as.character(dt1$CommonName))==trimws("NA")),NA,as.character(dt1$CommonName)))
dt1$MarkCode <- as.factor(ifelse((trimws(as.character(dt1$MarkCode))==trimws("NA")),NA,as.character(dt1$MarkCode)))
dt1$StageCode <- as.factor(ifelse((trimws(as.character(dt1$StageCode))==trimws("NA")),NA,as.character(dt1$StageCode)))
dt1$Expression <- as.factor(ifelse((trimws(as.character(dt1$Expression))==trimws("NA")),NA,as.character(dt1$Expression)))
dt1$ForkLength <- ifelse((trimws(as.character(dt1$ForkLength))==trimws("NA")),NA,dt1$ForkLength)               
suppressWarnings(dt1$ForkLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$ForkLength))==as.character(as.numeric("NA"))),NA,dt1$ForkLength))
dt1$RaceByLength <- as.factor(ifelse((trimws(as.character(dt1$RaceByLength))==trimws("NA")),NA,as.character(dt1$RaceByLength)))
dt1$TagCode <- as.factor(ifelse((trimws(as.character(dt1$TagCode))==trimws("NA")),NA,as.character(dt1$TagCode)))
dt1$RaceByTag <- as.factor(ifelse((trimws(as.character(dt1$RaceByTag))==trimws("NA")),NA,as.character(dt1$RaceByTag)))
dt1$ArchivalID <- as.factor(ifelse((trimws(as.character(dt1$ArchivalID))==trimws("NA")),NA,as.character(dt1$ArchivalID)))
dt1$SpecialStudyID <- as.factor(ifelse((trimws(as.character(dt1$SpecialStudyID))==trimws("NA")),NA,as.character(dt1$SpecialStudyID)))
dt1$GeneticID <- as.factor(ifelse((trimws(as.character(dt1$GeneticID))==trimws("NA")),NA,as.character(dt1$GeneticID)))
dt1$Probability1 <- ifelse((trimws(as.character(dt1$Probability1))==trimws("NA")),NA,dt1$Probability1)               
suppressWarnings(dt1$Probability1 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Probability1))==as.character(as.numeric("NA"))),NA,dt1$Probability1))
dt1$GeneticID2 <- as.factor(ifelse((trimws(as.character(dt1$GeneticID2))==trimws("NA")),NA,as.character(dt1$GeneticID2)))
dt1$Probability2 <- ifelse((trimws(as.character(dt1$Probability2))==trimws("NA")),NA,dt1$Probability2)               
suppressWarnings(dt1$Probability2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Probability2))==as.character(as.numeric("NA"))),NA,dt1$Probability2))
dt1$SexGeneID <- as.factor(ifelse((trimws(as.character(dt1$SexGeneID))==trimws("NA")),NA,as.character(dt1$SexGeneID)))
dt1$Ots28 <- as.factor(ifelse((trimws(as.character(dt1$Ots28))==trimws("NA")),NA,as.character(dt1$Ots28)))
dt1$Lab <- as.factor(ifelse((trimws(as.character(dt1$Lab))==trimws("NA")),NA,as.character(dt1$Lab)))
dt1$GeneticTest <- as.factor(ifelse((trimws(as.character(dt1$GeneticTest))==trimws("NA")),NA,as.character(dt1$GeneticTest)))
dt1$GeneticModel <- as.factor(ifelse((trimws(as.character(dt1$GeneticModel))==trimws("NA")),NA,as.character(dt1$GeneticModel)))
dt1$Count <- ifelse((trimws(as.character(dt1$Count))==trimws("NA")),NA,dt1$Count)               
suppressWarnings(dt1$Count <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Count))==as.character(as.numeric("NA"))),NA,dt1$Count))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
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

summary(as.factor(dt1$Location)) 
summary(as.factor(dt1$RegionCode)) 
summary(as.factor(dt1$StationCode)) 
summary(as.factor(dt1$MethodCode)) 
summary(as.factor(dt1$GearConditionCode)) 
summary(as.factor(dt1$WeatherCode)) 
summary(as.factor(dt1$SamplingDirection)) 
summary(as.factor(dt1$FlowDebris)) 
summary(as.factor(dt1$SiteDisturbance)) 
summary(as.factor(dt1$AlternateSite)) 
summary(as.factor(dt1$OrganismCode)) 
summary(as.factor(dt1$IEPFishCode)) 
summary(as.factor(dt1$CommonName)) 
summary(as.factor(dt1$MarkCode)) 
summary(as.factor(dt1$StageCode)) 
summary(as.factor(dt1$Expression)) 
summary(as.factor(dt1$RaceByLength)) 
summary(as.factor(dt1$TagCode)) 
summary(as.factor(dt1$RaceByTag)) 
summary(as.factor(dt1$ArchivalID)) 
summary(as.factor(dt1$SpecialStudyID)) 
summary(as.factor(dt1$GeneticID)) 
summary(as.factor(dt1$GeneticID2)) 
summary(as.factor(dt1$SexGeneID)) 
summary(as.factor(dt1$Ots28)) 
summary(as.factor(dt1$Lab)) 
summary(as.factor(dt1$GeneticTest)) 
summary(as.factor(dt1$GeneticModel))
detach(dt1)       

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

 

######################

#####################
# data manipulation #
#####################

# merge the two time periods together
#colnames(dt2)==colnames(dt3)
dataset<-rbind(dt1,dt2)
#remove(dt1,dt2)

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