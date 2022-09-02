# packages
library(here)
library(readxl)
library(lubridate)
#library(ggplot2)
#library(dplyr)
#library(purrr)
library(tidyverse)

#here::i_am("Spring pulse analysis.R")

########################
### CDEC Water Years ###
########################
WaterYr <- read_excel("CDEC Water Year Hydrologic Classification Indices.xlsx", 
                      sheet = "Sheet3")

###################################
# DJFMP data from EDI Data Portal #
###################################

# use R script from EDI Data Portal
# OR read in manually after download

# for this analysis, need only dt2 from their script
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

#####################
# data manipulation #
#####################

# Add Water Year
dt2$WY <- year(dt2$SampleDate) + 
  as.numeric(between(month(dt2$SampleDate),10,12))

# clip dt2 to station we want
# Sherwood Harbor
SH_Trawl <- dt2[dt2$StationCode=="SR055M",]
remove(dt2)

# Species = combine CommonName + RaceByLength to get species/run
SH_Trawl$Species <- as.character(SH_Trawl$CommonName)
SH_Trawl$Species[SH_Trawl$CommonName=="Chinook salmon"] <- paste(
  SH_Trawl$RaceByLength[SH_Trawl$CommonName=="Chinook salmon"],"-run ",
  "Chinook salmon",sep="")

################################################
# creating a dataframe of sample dates         #
# want a row for every species/run of interest #
# for every sample date                        #
################################################
SH_Trawl_sampledates <-SH_Trawl %>%
  group_by(SampleDate) %>%
  dplyr::summarize(
    WY = WY,
    Location = Location,
    StationCode = StationCode,
    SampleDate = SampleDate,
    MethodCode = MethodCode,
    ) 
#remove duplicate rows
SH_Trawl_sampledates <- SH_Trawl_sampledates[!duplicated(SH_Trawl_sampledates),]
#length(SH_Trawl_sampledates$SampleDate)
#length(unique(SH_Trawl_sampledates$SampleDate))

# need this whole df for every species
# add Species
SH_Trawl_sampledates$Species <- character(length(SH_Trawl_sampledates$SampleDate))

# want juvenile NMFS ESA/MSA species
species_list <- c("Fall-run Chinook salmon",
                  "NA-run Chinook salmon",
                  "Winter-run Chinook salmon",
                  "steelhead trout",
                  "LateFall-run Chinook salmon",
                  "Spring-run Chinook salmon")

# create dataframe with a row for each date for each species of interest
for (i in species_list){
  samples <- SH_Trawl_sampledates
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
SH_Trawl_sums <- SH_Trawl[(SH_Trawl$Species %in% species_list),c(colnames(SH_Trawl_sampledates),"Count")]

# combine the list of all sampledate rows of interest species
  # with the list of actual data
SH_Trawl_sums <- rbind(SH_Trawl_sums,interest_samples) # *******

# create a date and species variable
SH_Trawl_sums$DateSpecies <- paste(SH_Trawl_sums$SampleDate,
                                          SH_Trawl_sums$Species)


# sum over species+date
SH_Trawl_summary <- SH_Trawl_sums %>%
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
SH_Trawl_summary <-SH_Trawl_summary[!duplicated(SH_Trawl_summary),]
                  
#####################################
# analysis of a spring pulse period #
#             spring-run            #
#####################################

# cut to just spring-run for ease (no WR or steelhead)
SPP <- SH_Trawl_summary[SH_Trawl_summary$Species=="Spring-run Chinook salmon",]

# create data table for results
columns <- c("WY","Peak_Date","PP_sum","Annual_sum") 
SPP_results <- data.frame(matrix(nrow = (length(unique(SH_Trawl_summary$WY))-2), ncol = length(columns))) 
colnames(SPP_results) <- columns
SPP_results$WY <- unique(SPP$WY)[2:(length(unique(SPP$WY))-1)] # note years will be clipped 
# full WY not here for first year, only full calendar year; similar for last year in list 
SPP_results <- left_join(SPP_results,WaterYr[,c(1,3)])

yearlist <- SPP_results$WY 


for (i in 1:length(SPP_results$WY)){
  
  temp_dataset <- SPP[SPP$WY==SPP_results$WY[i],] # data for specific WY
  
  # get Date of the max daily catch
  SPP_results[SPP_results$WY==SPP_results$WY[i],]$Peak_Date <- 
    substr(temp_dataset[which.max(temp_dataset$Catch_sum),"SampleDate"][[1]], start=1, stop=10)
  # why won't the above work as date format in loop?
  
  # sum of all catch over the year
  SPP_results$Annual_sum[i] <- sum(temp_dataset$Catch_sum)
}
# put Peak_Date back in date format
SPP_results$Peak_Date <- ymd(SPP_results$Peak_Date)

# consider a multi-day pulse period around peak catch
PPdays <- 11 # number of days of the pulse period
PPdaybounds <- ((PPdays-1) #peakday
                /2 # days on each side
                +1) # using exclusive not inclusive <> below

for (i in 1:length(SPP_results$WY)){
  temp_dataset <- SPP[SPP$WY==SPP_results$WY[i] & as.Date(SPP$SampleDate) > SPP_results$Peak_Date[i]-PPdaybounds
                      & as.Date(SPP$SampleDate) < SPP_results$Peak_Date[i]+PPdaybounds,]
  # sum of daily catch for just the +-5 days period around the peak date  
  SPP_results$PP_sum[i] <- sum(temp_dataset$Catch_sum)
  
}
# Sherwood Harbor trawl is not daily, so we won't get 11 values - only ~4
# should still give a rough percentage of total catch, but coarser than daily data

SPP_results$proportion <- SPP_results$PP_sum / SPP_results$Annual_sum
colnames(SPP_results) <- c("Water Year","Date of Peak Catch",
                           "Pulse Period Catch","Annual Catch",
                           "Sac Water Year Type",
                           "Catch Proportion during Pulse Period")
write.csv(SPP_results, "SPP_results.csv", row.names=FALSE)
