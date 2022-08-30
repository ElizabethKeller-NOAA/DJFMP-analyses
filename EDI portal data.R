

###############################
# R code from EDI data portal #
# https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=244&revision=5
###############################

# Package ID: edi.244.5 Cataloging System:https://pasta.edirepository.org.
# Data set title: Interagency Ecological Program: Over four decades of juvenile fish monitoring data from the San Francisco Estuary, collected by the Delta Juvenile Fish Monitoring Program, 1976-2019.
# Data set creator:   Interagency Ecological Program (IEP) -  
# Data set creator:  Ryan McKenzie - United States Fish and Wildlife Service 
# Data set creator:  Jonathan Speegle - United States Fish and Wildlife Service 
# Data set creator:  Adam Nanninga - United States Fish and Wildlife Service 
# Data set creator:  John Cook - United States Fish and Wildlife Service 
# Data set creator:  Jackie Hagen - United States Fish and Wildlife Service 
# Data set creator:  Brian Mahardja - United States Fish and Wildlife Service 
# Data set creator:  Adriana Arrambide - United States Fish and Wildlife Service 
# Data set creator:  Lori Smith - United States Fish and Wildlife Service 
# Contact:  Ryan McKenzie Fish Biologist -  United States Fish and Wildlife Service  - ryan_mckenzie@fws.gov
# Contact:  Jonathan Speegle Data Manager -  United States Fish and Wildlife Service  - jonathan_speegle@fws.gov
# Contact:  Adam Nanninga Field Crew Supervisor -  United States Fish and Wildlife Service  - adam_nanninga@fws.gov
# Contact:  J. Ryan Cook Field Crew Supervisor -  United States Fish and Wildlife Service  - john_cook@fws.gov
# Contact:  Brian Mahardja Fish Biologist (Supervisor) -  United States Fish and Wildlife Service  - brian_mahardja@fws.gov
# Contact:  Jackie Hagen Small Craft Operator Supervisor -  United States Fish and Wildlife Service  - jackie_hagen@fws.gov
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

##################################################################
# 1. Name: 1976-2001_DJFMP_trawl_fish_and_water_quality_data.csv #
# File: 1976-2001_DJFMP_trawl_fish_and_water_quality_data.csv    #
##################################################################

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/244/5/71c16ead9b8ffa4da7a52da180f601f4" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

                   
 dt1 <-read.csv(infile1,header=F 
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
                    "WaterTemperature",     
                    "Turbidity",     
                    "Secchi",     
                    "Conductivity",     
                    "TowNumber",     
                    "TowDirectionCode",     
                    "TowDuration",     
                    "flowDebris",     
                    "SiteDisturbance",     
                    "AlternateSite",     
                    "SeineLength",     
                    "SeineWidth",     
                    "SeineDepth",     
                    "StartMeter",     
                    "EndMeter",     
                    "TotalMeter",     
                    "Volume",     
                    "OrganismCode",     
                    "CommonName",     
                    "MarkCode",     
                    "StageCode",     
                    "Maturation",     
                    "ForkLength",     
                    "RaceByLength",     
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
if (class(dt1$WaterTemperature)=="factor") dt1$WaterTemperature <-as.numeric(levels(dt1$WaterTemperature))[as.integer(dt1$WaterTemperature) ]               
if (class(dt1$WaterTemperature)=="character") dt1$WaterTemperature <-as.numeric(dt1$WaterTemperature)
if (class(dt1$Turbidity)=="factor") dt1$Turbidity <-as.numeric(levels(dt1$Turbidity))[as.integer(dt1$Turbidity) ]               
if (class(dt1$Turbidity)=="character") dt1$Turbidity <-as.numeric(dt1$Turbidity)
if (class(dt1$Secchi)=="factor") dt1$Secchi <-as.numeric(levels(dt1$Secchi))[as.integer(dt1$Secchi) ]               
if (class(dt1$Secchi)=="character") dt1$Secchi <-as.numeric(dt1$Secchi)
if (class(dt1$Conductivity)=="factor") dt1$Conductivity <-as.numeric(levels(dt1$Conductivity))[as.integer(dt1$Conductivity) ]               
if (class(dt1$Conductivity)=="character") dt1$Conductivity <-as.numeric(dt1$Conductivity)
if (class(dt1$TowNumber)=="factor") dt1$TowNumber <-as.numeric(levels(dt1$TowNumber))[as.integer(dt1$TowNumber) ]               
if (class(dt1$TowNumber)=="character") dt1$TowNumber <-as.numeric(dt1$TowNumber)
if (class(dt1$TowDirectionCode)!="factor") dt1$TowDirectionCode<- as.factor(dt1$TowDirectionCode)
if (class(dt1$TowDuration)=="factor") dt1$TowDuration <-as.numeric(levels(dt1$TowDuration))[as.integer(dt1$TowDuration) ]               
if (class(dt1$TowDuration)=="character") dt1$TowDuration <-as.numeric(dt1$TowDuration)
if (class(dt1$flowDebris)!="factor") dt1$flowDebris<- as.factor(dt1$flowDebris)
if (class(dt1$SiteDisturbance)!="factor") dt1$SiteDisturbance<- as.factor(dt1$SiteDisturbance)
if (class(dt1$AlternateSite)!="factor") dt1$AlternateSite<- as.factor(dt1$AlternateSite)
if (class(dt1$SeineLength)=="factor") dt1$SeineLength <-as.numeric(levels(dt1$SeineLength))[as.integer(dt1$SeineLength) ]               
if (class(dt1$SeineLength)=="character") dt1$SeineLength <-as.numeric(dt1$SeineLength)
if (class(dt1$SeineWidth)=="factor") dt1$SeineWidth <-as.numeric(levels(dt1$SeineWidth))[as.integer(dt1$SeineWidth) ]               
if (class(dt1$SeineWidth)=="character") dt1$SeineWidth <-as.numeric(dt1$SeineWidth)
if (class(dt1$SeineDepth)=="factor") dt1$SeineDepth <-as.numeric(levels(dt1$SeineDepth))[as.integer(dt1$SeineDepth) ]               
if (class(dt1$SeineDepth)=="character") dt1$SeineDepth <-as.numeric(dt1$SeineDepth)
if (class(dt1$StartMeter)=="factor") dt1$StartMeter <-as.numeric(levels(dt1$StartMeter))[as.integer(dt1$StartMeter) ]               
if (class(dt1$StartMeter)=="character") dt1$StartMeter <-as.numeric(dt1$StartMeter)
if (class(dt1$EndMeter)=="factor") dt1$EndMeter <-as.numeric(levels(dt1$EndMeter))[as.integer(dt1$EndMeter) ]               
if (class(dt1$EndMeter)=="character") dt1$EndMeter <-as.numeric(dt1$EndMeter)
if (class(dt1$TotalMeter)=="factor") dt1$TotalMeter <-as.numeric(levels(dt1$TotalMeter))[as.integer(dt1$TotalMeter) ]               
if (class(dt1$TotalMeter)=="character") dt1$TotalMeter <-as.numeric(dt1$TotalMeter)
if (class(dt1$Volume)=="factor") dt1$Volume <-as.numeric(levels(dt1$Volume))[as.integer(dt1$Volume) ]               
if (class(dt1$Volume)=="character") dt1$Volume <-as.numeric(dt1$Volume)
if (class(dt1$OrganismCode)!="factor") dt1$OrganismCode<- as.factor(dt1$OrganismCode)
if (class(dt1$CommonName)!="factor") dt1$CommonName<- as.factor(dt1$CommonName)
if (class(dt1$MarkCode)!="factor") dt1$MarkCode<- as.factor(dt1$MarkCode)
if (class(dt1$StageCode)!="factor") dt1$StageCode<- as.factor(dt1$StageCode)
if (class(dt1$Maturation)!="factor") dt1$Maturation<- as.factor(dt1$Maturation)
if (class(dt1$ForkLength)=="factor") dt1$ForkLength <-as.numeric(levels(dt1$ForkLength))[as.integer(dt1$ForkLength) ]               
if (class(dt1$ForkLength)=="character") dt1$ForkLength <-as.numeric(dt1$ForkLength)
if (class(dt1$RaceByLength)!="factor") dt1$RaceByLength<- as.factor(dt1$RaceByLength)
if (class(dt1$Count)=="factor") dt1$Count <-as.numeric(levels(dt1$Count))[as.integer(dt1$Count) ]               
if (class(dt1$Count)=="character") dt1$Count <-as.numeric(dt1$Count)
                
# Convert Missing Values to NA for non-dates
                
dt1$WeatherCode <- as.factor(ifelse((trimws(as.character(dt1$WeatherCode))==trimws("NA")),NA,as.character(dt1$WeatherCode)))
dt1$DO <- ifelse((trimws(as.character(dt1$DO))==trimws("NA")),NA,dt1$DO)               
suppressWarnings(dt1$DO <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DO))==as.character(as.numeric("NA"))),NA,dt1$DO))
dt1$WaterTemperature <- ifelse((trimws(as.character(dt1$WaterTemperature))==trimws("NA")),NA,dt1$WaterTemperature)               
suppressWarnings(dt1$WaterTemperature <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$WaterTemperature))==as.character(as.numeric("NA"))),NA,dt1$WaterTemperature))
dt1$Turbidity <- ifelse((trimws(as.character(dt1$Turbidity))==trimws("NA")),NA,dt1$Turbidity)               
suppressWarnings(dt1$Turbidity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Turbidity))==as.character(as.numeric("NA"))),NA,dt1$Turbidity))
dt1$Secchi <- ifelse((trimws(as.character(dt1$Secchi))==trimws("NA")),NA,dt1$Secchi)               
suppressWarnings(dt1$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Secchi))==as.character(as.numeric("NA"))),NA,dt1$Secchi))
dt1$Conductivity <- ifelse((trimws(as.character(dt1$Conductivity))==trimws("NA")),NA,dt1$Conductivity)               
suppressWarnings(dt1$Conductivity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Conductivity))==as.character(as.numeric("NA"))),NA,dt1$Conductivity))
dt1$TowNumber <- ifelse((trimws(as.character(dt1$TowNumber))==trimws("NA")),NA,dt1$TowNumber)               
suppressWarnings(dt1$TowNumber <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TowNumber))==as.character(as.numeric("NA"))),NA,dt1$TowNumber))
dt1$TowDirectionCode <- as.factor(ifelse((trimws(as.character(dt1$TowDirectionCode))==trimws("NA")),NA,as.character(dt1$TowDirectionCode)))
dt1$TowDuration <- ifelse((trimws(as.character(dt1$TowDuration))==trimws("NA")),NA,dt1$TowDuration)               
suppressWarnings(dt1$TowDuration <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TowDuration))==as.character(as.numeric("NA"))),NA,dt1$TowDuration))
dt1$flowDebris <- as.factor(ifelse((trimws(as.character(dt1$flowDebris))==trimws("NA")),NA,as.character(dt1$flowDebris)))
dt1$SiteDisturbance <- as.factor(ifelse((trimws(as.character(dt1$SiteDisturbance))==trimws("NA")),NA,as.character(dt1$SiteDisturbance)))
dt1$AlternateSite <- as.factor(ifelse((trimws(as.character(dt1$AlternateSite))==trimws("NA")),NA,as.character(dt1$AlternateSite)))
dt1$SeineLength <- ifelse((trimws(as.character(dt1$SeineLength))==trimws("NA")),NA,dt1$SeineLength)               
suppressWarnings(dt1$SeineLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SeineLength))==as.character(as.numeric("NA"))),NA,dt1$SeineLength))
dt1$SeineWidth <- ifelse((trimws(as.character(dt1$SeineWidth))==trimws("NA")),NA,dt1$SeineWidth)               
suppressWarnings(dt1$SeineWidth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SeineWidth))==as.character(as.numeric("NA"))),NA,dt1$SeineWidth))
dt1$SeineDepth <- ifelse((trimws(as.character(dt1$SeineDepth))==trimws("NA")),NA,dt1$SeineDepth)               
suppressWarnings(dt1$SeineDepth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SeineDepth))==as.character(as.numeric("NA"))),NA,dt1$SeineDepth))
dt1$StartMeter <- ifelse((trimws(as.character(dt1$StartMeter))==trimws("NA")),NA,dt1$StartMeter)               
suppressWarnings(dt1$StartMeter <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$StartMeter))==as.character(as.numeric("NA"))),NA,dt1$StartMeter))
dt1$EndMeter <- ifelse((trimws(as.character(dt1$EndMeter))==trimws("NA")),NA,dt1$EndMeter)               
suppressWarnings(dt1$EndMeter <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$EndMeter))==as.character(as.numeric("NA"))),NA,dt1$EndMeter))
dt1$TotalMeter <- ifelse((trimws(as.character(dt1$TotalMeter))==trimws("NA")),NA,dt1$TotalMeter)               
suppressWarnings(dt1$TotalMeter <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TotalMeter))==as.character(as.numeric("NA"))),NA,dt1$TotalMeter))
dt1$Volume <- ifelse((trimws(as.character(dt1$Volume))==trimws("NA")),NA,dt1$Volume)               
suppressWarnings(dt1$Volume <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Volume))==as.character(as.numeric("NA"))),NA,dt1$Volume))
dt1$StageCode <- as.factor(ifelse((trimws(as.character(dt1$StageCode))==trimws("n/p")),NA,as.character(dt1$StageCode)))
dt1$Maturation <- as.factor(ifelse((trimws(as.character(dt1$Maturation))==trimws("n/p")),NA,as.character(dt1$Maturation)))
dt1$ForkLength <- ifelse((trimws(as.character(dt1$ForkLength))==trimws("NA")),NA,dt1$ForkLength)               
suppressWarnings(dt1$ForkLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$ForkLength))==as.character(as.numeric("NA"))),NA,dt1$ForkLength))
dt1$RaceByLength <- as.factor(ifelse((trimws(as.character(dt1$RaceByLength))==trimws("n/p")),NA,as.character(dt1$RaceByLength)))
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
summary(WaterTemperature)
summary(Turbidity)
summary(Secchi)
summary(Conductivity)
summary(TowNumber)
summary(TowDirectionCode)
summary(TowDuration)
summary(flowDebris)
summary(SiteDisturbance)
summary(AlternateSite)
summary(SeineLength)
summary(SeineWidth)
summary(SeineDepth)
summary(StartMeter)
summary(EndMeter)
summary(TotalMeter)
summary(Volume)
summary(OrganismCode)
summary(CommonName)
summary(MarkCode)
summary(StageCode)
summary(Maturation)
summary(ForkLength)
summary(RaceByLength)
summary(Count) 
                # Get more details on character variables
                 
summary(as.factor(dt1$Location)) 
summary(as.factor(dt1$RegionCode)) 
summary(as.factor(dt1$StationCode)) 
summary(as.factor(dt1$MethodCode)) 
summary(as.factor(dt1$GearConditionCode)) 
summary(as.factor(dt1$WeatherCode)) 
summary(as.factor(dt1$TowDirectionCode)) 
summary(as.factor(dt1$flowDebris)) 
summary(as.factor(dt1$SiteDisturbance)) 
summary(as.factor(dt1$AlternateSite)) 
summary(as.factor(dt1$OrganismCode)) 
summary(as.factor(dt1$CommonName)) 
summary(as.factor(dt1$MarkCode)) 
summary(as.factor(dt1$StageCode)) 
summary(as.factor(dt1$Maturation)) 
summary(as.factor(dt1$RaceByLength))
detach(dt1)               

##################################################################         
# 2. Name: 2002-2020_DJFMP_trawl_fish_and_water_quality_data.csv #
# File: 2002-2020_DJFMP_trawl_fish_and_water_quality_data.csv    # 
##################################################################         

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/244/5/0edf413c39ac8b111a576d894306a60f" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")

                   
 dt2 <-read.csv(infile2,header=F 
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
                    "WaterTemperature",     
                    "Turbidity",     
                    "Secchi",     
                    "Conductivity",     
                    "TowNumber",     
                    "TowDirectionCode",     
                    "TowDuration",     
                    "flowDebris",     
                    "SiteDisturbance",     
                    "AlternateSite",     
                    "SeineLength",     
                    "SeineWidth",     
                    "SeineDepth",     
                    "StartMeter",     
                    "EndMeter",     
                    "TotalMeter",     
                    "Volume",     
                    "OrganismCode",     
                    "CommonName",     
                    "MarkCode",     
                    "StageCode",     
                    "Maturation",     
                    "ForkLength",     
                    "RaceByLength",     
                    "Count"    ), check.names=TRUE)
               
unlink(infile2)
		    
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
if (class(dt2$WaterTemperature)=="factor") dt2$WaterTemperature <-as.numeric(levels(dt2$WaterTemperature))[as.integer(dt2$WaterTemperature) ]               
if (class(dt2$WaterTemperature)=="character") dt2$WaterTemperature <-as.numeric(dt2$WaterTemperature)
if (class(dt2$Turbidity)=="factor") dt2$Turbidity <-as.numeric(levels(dt2$Turbidity))[as.integer(dt2$Turbidity) ]               
if (class(dt2$Turbidity)=="character") dt2$Turbidity <-as.numeric(dt2$Turbidity)
if (class(dt2$Secchi)=="factor") dt2$Secchi <-as.numeric(levels(dt2$Secchi))[as.integer(dt2$Secchi) ]               
if (class(dt2$Secchi)=="character") dt2$Secchi <-as.numeric(dt2$Secchi)
if (class(dt2$Conductivity)=="factor") dt2$Conductivity <-as.numeric(levels(dt2$Conductivity))[as.integer(dt2$Conductivity) ]               
if (class(dt2$Conductivity)=="character") dt2$Conductivity <-as.numeric(dt2$Conductivity)
if (class(dt2$TowNumber)=="factor") dt2$TowNumber <-as.numeric(levels(dt2$TowNumber))[as.integer(dt2$TowNumber) ]               
if (class(dt2$TowNumber)=="character") dt2$TowNumber <-as.numeric(dt2$TowNumber)
if (class(dt2$TowDirectionCode)!="factor") dt2$TowDirectionCode<- as.factor(dt2$TowDirectionCode)
if (class(dt2$TowDuration)=="factor") dt2$TowDuration <-as.numeric(levels(dt2$TowDuration))[as.integer(dt2$TowDuration) ]               
if (class(dt2$TowDuration)=="character") dt2$TowDuration <-as.numeric(dt2$TowDuration)
if (class(dt2$flowDebris)!="factor") dt2$flowDebris<- as.factor(dt2$flowDebris)
if (class(dt2$SiteDisturbance)!="factor") dt2$SiteDisturbance<- as.factor(dt2$SiteDisturbance)
if (class(dt2$AlternateSite)!="factor") dt2$AlternateSite<- as.factor(dt2$AlternateSite)
if (class(dt2$SeineLength)=="factor") dt2$SeineLength <-as.numeric(levels(dt2$SeineLength))[as.integer(dt2$SeineLength) ]               
if (class(dt2$SeineLength)=="character") dt2$SeineLength <-as.numeric(dt2$SeineLength)
if (class(dt2$SeineWidth)=="factor") dt2$SeineWidth <-as.numeric(levels(dt2$SeineWidth))[as.integer(dt2$SeineWidth) ]               
if (class(dt2$SeineWidth)=="character") dt2$SeineWidth <-as.numeric(dt2$SeineWidth)
if (class(dt2$SeineDepth)=="factor") dt2$SeineDepth <-as.numeric(levels(dt2$SeineDepth))[as.integer(dt2$SeineDepth) ]               
if (class(dt2$SeineDepth)=="character") dt2$SeineDepth <-as.numeric(dt2$SeineDepth)
if (class(dt2$StartMeter)=="factor") dt2$StartMeter <-as.numeric(levels(dt2$StartMeter))[as.integer(dt2$StartMeter) ]               
if (class(dt2$StartMeter)=="character") dt2$StartMeter <-as.numeric(dt2$StartMeter)
if (class(dt2$EndMeter)=="factor") dt2$EndMeter <-as.numeric(levels(dt2$EndMeter))[as.integer(dt2$EndMeter) ]               
if (class(dt2$EndMeter)=="character") dt2$EndMeter <-as.numeric(dt2$EndMeter)
if (class(dt2$TotalMeter)=="factor") dt2$TotalMeter <-as.numeric(levels(dt2$TotalMeter))[as.integer(dt2$TotalMeter) ]               
if (class(dt2$TotalMeter)=="character") dt2$TotalMeter <-as.numeric(dt2$TotalMeter)
if (class(dt2$Volume)=="factor") dt2$Volume <-as.numeric(levels(dt2$Volume))[as.integer(dt2$Volume) ]               
if (class(dt2$Volume)=="character") dt2$Volume <-as.numeric(dt2$Volume)
if (class(dt2$OrganismCode)!="factor") dt2$OrganismCode<- as.factor(dt2$OrganismCode)
if (class(dt2$CommonName)!="factor") dt2$CommonName<- as.factor(dt2$CommonName)
if (class(dt2$MarkCode)!="factor") dt2$MarkCode<- as.factor(dt2$MarkCode)
if (class(dt2$StageCode)!="factor") dt2$StageCode<- as.factor(dt2$StageCode)
if (class(dt2$Maturation)!="factor") dt2$Maturation<- as.factor(dt2$Maturation)
if (class(dt2$ForkLength)=="factor") dt2$ForkLength <-as.numeric(levels(dt2$ForkLength))[as.integer(dt2$ForkLength) ]               
if (class(dt2$ForkLength)=="character") dt2$ForkLength <-as.numeric(dt2$ForkLength)
if (class(dt2$RaceByLength)!="factor") dt2$RaceByLength<- as.factor(dt2$RaceByLength)
if (class(dt2$Count)=="factor") dt2$Count <-as.numeric(levels(dt2$Count))[as.integer(dt2$Count) ]               
if (class(dt2$Count)=="character") dt2$Count <-as.numeric(dt2$Count)
                
# Convert Missing Values to NA for non-dates
                
dt2$WeatherCode <- as.factor(ifelse((trimws(as.character(dt2$WeatherCode))==trimws("NA")),NA,as.character(dt2$WeatherCode)))
dt2$DO <- ifelse((trimws(as.character(dt2$DO))==trimws("NA")),NA,dt2$DO)               
suppressWarnings(dt2$DO <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$DO))==as.character(as.numeric("NA"))),NA,dt2$DO))
dt2$WaterTemperature <- ifelse((trimws(as.character(dt2$WaterTemperature))==trimws("NA")),NA,dt2$WaterTemperature)               
suppressWarnings(dt2$WaterTemperature <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$WaterTemperature))==as.character(as.numeric("NA"))),NA,dt2$WaterTemperature))
dt2$Turbidity <- ifelse((trimws(as.character(dt2$Turbidity))==trimws("NA")),NA,dt2$Turbidity)               
suppressWarnings(dt2$Turbidity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Turbidity))==as.character(as.numeric("NA"))),NA,dt2$Turbidity))
dt2$Secchi <- ifelse((trimws(as.character(dt2$Secchi))==trimws("NA")),NA,dt2$Secchi)               
suppressWarnings(dt2$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Secchi))==as.character(as.numeric("NA"))),NA,dt2$Secchi))
dt2$Conductivity <- ifelse((trimws(as.character(dt2$Conductivity))==trimws("NA")),NA,dt2$Conductivity)               
suppressWarnings(dt2$Conductivity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Conductivity))==as.character(as.numeric("NA"))),NA,dt2$Conductivity))
dt2$TowNumber <- ifelse((trimws(as.character(dt2$TowNumber))==trimws("NA")),NA,dt2$TowNumber)               
suppressWarnings(dt2$TowNumber <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TowNumber))==as.character(as.numeric("NA"))),NA,dt2$TowNumber))
dt2$TowDirectionCode <- as.factor(ifelse((trimws(as.character(dt2$TowDirectionCode))==trimws("NA")),NA,as.character(dt2$TowDirectionCode)))
dt2$TowDuration <- ifelse((trimws(as.character(dt2$TowDuration))==trimws("NA")),NA,dt2$TowDuration)               
suppressWarnings(dt2$TowDuration <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TowDuration))==as.character(as.numeric("NA"))),NA,dt2$TowDuration))
dt2$flowDebris <- as.factor(ifelse((trimws(as.character(dt2$flowDebris))==trimws("NA")),NA,as.character(dt2$flowDebris)))
dt2$SiteDisturbance <- as.factor(ifelse((trimws(as.character(dt2$SiteDisturbance))==trimws("NA")),NA,as.character(dt2$SiteDisturbance)))
dt2$AlternateSite <- as.factor(ifelse((trimws(as.character(dt2$AlternateSite))==trimws("NA")),NA,as.character(dt2$AlternateSite)))
dt2$SeineLength <- ifelse((trimws(as.character(dt2$SeineLength))==trimws("NA")),NA,dt2$SeineLength)               
suppressWarnings(dt2$SeineLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SeineLength))==as.character(as.numeric("NA"))),NA,dt2$SeineLength))
dt2$SeineWidth <- ifelse((trimws(as.character(dt2$SeineWidth))==trimws("NA")),NA,dt2$SeineWidth)               
suppressWarnings(dt2$SeineWidth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SeineWidth))==as.character(as.numeric("NA"))),NA,dt2$SeineWidth))
dt2$SeineDepth <- ifelse((trimws(as.character(dt2$SeineDepth))==trimws("NA")),NA,dt2$SeineDepth)               
suppressWarnings(dt2$SeineDepth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SeineDepth))==as.character(as.numeric("NA"))),NA,dt2$SeineDepth))
dt2$StartMeter <- ifelse((trimws(as.character(dt2$StartMeter))==trimws("NA")),NA,dt2$StartMeter)               
suppressWarnings(dt2$StartMeter <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$StartMeter))==as.character(as.numeric("NA"))),NA,dt2$StartMeter))
dt2$EndMeter <- ifelse((trimws(as.character(dt2$EndMeter))==trimws("NA")),NA,dt2$EndMeter)               
suppressWarnings(dt2$EndMeter <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$EndMeter))==as.character(as.numeric("NA"))),NA,dt2$EndMeter))
dt2$TotalMeter <- ifelse((trimws(as.character(dt2$TotalMeter))==trimws("NA")),NA,dt2$TotalMeter)               
suppressWarnings(dt2$TotalMeter <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TotalMeter))==as.character(as.numeric("NA"))),NA,dt2$TotalMeter))
dt2$Volume <- ifelse((trimws(as.character(dt2$Volume))==trimws("NA")),NA,dt2$Volume)               
suppressWarnings(dt2$Volume <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Volume))==as.character(as.numeric("NA"))),NA,dt2$Volume))
dt2$StageCode <- as.factor(ifelse((trimws(as.character(dt2$StageCode))==trimws("n/p")),NA,as.character(dt2$StageCode)))
dt2$Maturation <- as.factor(ifelse((trimws(as.character(dt2$Maturation))==trimws("n/p")),NA,as.character(dt2$Maturation)))
dt2$ForkLength <- ifelse((trimws(as.character(dt2$ForkLength))==trimws("NA")),NA,dt2$ForkLength)               
suppressWarnings(dt2$ForkLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ForkLength))==as.character(as.numeric("NA"))),NA,dt2$ForkLength))
dt2$RaceByLength <- as.factor(ifelse((trimws(as.character(dt2$RaceByLength))==trimws("n/p")),NA,as.character(dt2$RaceByLength)))
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
summary(WaterTemperature)
summary(Turbidity)
summary(Secchi)
summary(Conductivity)
summary(TowNumber)
summary(TowDirectionCode)
summary(TowDuration)
summary(flowDebris)
summary(SiteDisturbance)
summary(AlternateSite)
summary(SeineLength)
summary(SeineWidth)
summary(SeineDepth)
summary(StartMeter)
summary(EndMeter)
summary(TotalMeter)
summary(Volume)
summary(OrganismCode)
summary(CommonName)
summary(MarkCode)
summary(StageCode)
summary(Maturation)
summary(ForkLength)
summary(RaceByLength)
summary(Count) 
                # Get more details on character variables
                 
summary(as.factor(dt2$Location)) 
summary(as.factor(dt2$RegionCode)) 
summary(as.factor(dt2$StationCode)) 
summary(as.factor(dt2$MethodCode)) 
summary(as.factor(dt2$GearConditionCode)) 
summary(as.factor(dt2$WeatherCode)) 
summary(as.factor(dt2$TowDirectionCode)) 
summary(as.factor(dt2$flowDebris)) 
summary(as.factor(dt2$SiteDisturbance)) 
summary(as.factor(dt2$AlternateSite)) 
summary(as.factor(dt2$OrganismCode)) 
summary(as.factor(dt2$CommonName)) 
summary(as.factor(dt2$MarkCode)) 
summary(as.factor(dt2$StageCode)) 
summary(as.factor(dt2$Maturation)) 
summary(as.factor(dt2$RaceByLength))
detach(dt2)               

########################################################################        
# 3. Name: 1976-2020_DJFMP_beach_seine_fish_and_water_quality_data.csv #
# File: 1976-2020_DJFMP_beach_seine_fish_and_water_quality_data.csv    # 
######################################################################## 

inUrl3  <- "https://pasta.lternet.edu/package/data/eml/edi/244/5/a3e94e8f0cf6f675d716151c1ca17b4f" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl"))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")

                   
 dt3 <-read.csv(infile3,header=F 
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
                    "WaterTemperature",     
                    "Turbidity",     
                    "Secchi",     
                    "Conductivity",     
                    "TowNumber",     
                    "TowDirectionCode",     
                    "TowDuration",     
                    "flowDebris",     
                    "SiteDisturbance",     
                    "AlternateSite",     
                    "SeineLength",     
                    "SeineWidth",     
                    "SeineDepth",     
                    "StartMeter",     
                    "EndMeter",     
                    "TotalMeter",     
                    "Volume",     
                    "OrganismCode",     
                    "CommonName",     
                    "MarkCode",     
                    "StageCode",     
                    "Maturation",     
                    "ForkLength",     
                    "RaceByLength",     
                    "Count"    ), check.names=TRUE)
               
unlink(infile3)
		    
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
if (class(dt3$WaterTemperature)=="factor") dt3$WaterTemperature <-as.numeric(levels(dt3$WaterTemperature))[as.integer(dt3$WaterTemperature) ]               
if (class(dt3$WaterTemperature)=="character") dt3$WaterTemperature <-as.numeric(dt3$WaterTemperature)
if (class(dt3$Turbidity)=="factor") dt3$Turbidity <-as.numeric(levels(dt3$Turbidity))[as.integer(dt3$Turbidity) ]               
if (class(dt3$Turbidity)=="character") dt3$Turbidity <-as.numeric(dt3$Turbidity)
if (class(dt3$Secchi)=="factor") dt3$Secchi <-as.numeric(levels(dt3$Secchi))[as.integer(dt3$Secchi) ]               
if (class(dt3$Secchi)=="character") dt3$Secchi <-as.numeric(dt3$Secchi)
if (class(dt3$Conductivity)=="factor") dt3$Conductivity <-as.numeric(levels(dt3$Conductivity))[as.integer(dt3$Conductivity) ]               
if (class(dt3$Conductivity)=="character") dt3$Conductivity <-as.numeric(dt3$Conductivity)
if (class(dt3$TowNumber)=="factor") dt3$TowNumber <-as.numeric(levels(dt3$TowNumber))[as.integer(dt3$TowNumber) ]               
if (class(dt3$TowNumber)=="character") dt3$TowNumber <-as.numeric(dt3$TowNumber)
if (class(dt3$TowDirectionCode)!="factor") dt3$TowDirectionCode<- as.factor(dt3$TowDirectionCode)
if (class(dt3$TowDuration)=="factor") dt3$TowDuration <-as.numeric(levels(dt3$TowDuration))[as.integer(dt3$TowDuration) ]               
if (class(dt3$TowDuration)=="character") dt3$TowDuration <-as.numeric(dt3$TowDuration)
if (class(dt3$flowDebris)!="factor") dt3$flowDebris<- as.factor(dt3$flowDebris)
if (class(dt3$SiteDisturbance)!="factor") dt3$SiteDisturbance<- as.factor(dt3$SiteDisturbance)
if (class(dt3$AlternateSite)!="factor") dt3$AlternateSite<- as.factor(dt3$AlternateSite)
if (class(dt3$SeineLength)=="factor") dt3$SeineLength <-as.numeric(levels(dt3$SeineLength))[as.integer(dt3$SeineLength) ]               
if (class(dt3$SeineLength)=="character") dt3$SeineLength <-as.numeric(dt3$SeineLength)
if (class(dt3$SeineWidth)=="factor") dt3$SeineWidth <-as.numeric(levels(dt3$SeineWidth))[as.integer(dt3$SeineWidth) ]               
if (class(dt3$SeineWidth)=="character") dt3$SeineWidth <-as.numeric(dt3$SeineWidth)
if (class(dt3$SeineDepth)=="factor") dt3$SeineDepth <-as.numeric(levels(dt3$SeineDepth))[as.integer(dt3$SeineDepth) ]               
if (class(dt3$SeineDepth)=="character") dt3$SeineDepth <-as.numeric(dt3$SeineDepth)
if (class(dt3$StartMeter)=="factor") dt3$StartMeter <-as.numeric(levels(dt3$StartMeter))[as.integer(dt3$StartMeter) ]               
if (class(dt3$StartMeter)=="character") dt3$StartMeter <-as.numeric(dt3$StartMeter)
if (class(dt3$EndMeter)=="factor") dt3$EndMeter <-as.numeric(levels(dt3$EndMeter))[as.integer(dt3$EndMeter) ]               
if (class(dt3$EndMeter)=="character") dt3$EndMeter <-as.numeric(dt3$EndMeter)
if (class(dt3$TotalMeter)=="factor") dt3$TotalMeter <-as.numeric(levels(dt3$TotalMeter))[as.integer(dt3$TotalMeter) ]               
if (class(dt3$TotalMeter)=="character") dt3$TotalMeter <-as.numeric(dt3$TotalMeter)
if (class(dt3$Volume)=="factor") dt3$Volume <-as.numeric(levels(dt3$Volume))[as.integer(dt3$Volume) ]               
if (class(dt3$Volume)=="character") dt3$Volume <-as.numeric(dt3$Volume)
if (class(dt3$OrganismCode)!="factor") dt3$OrganismCode<- as.factor(dt3$OrganismCode)
if (class(dt3$CommonName)!="factor") dt3$CommonName<- as.factor(dt3$CommonName)
if (class(dt3$MarkCode)!="factor") dt3$MarkCode<- as.factor(dt3$MarkCode)
if (class(dt3$StageCode)!="factor") dt3$StageCode<- as.factor(dt3$StageCode)
if (class(dt3$Maturation)!="factor") dt3$Maturation<- as.factor(dt3$Maturation)
if (class(dt3$ForkLength)=="factor") dt3$ForkLength <-as.numeric(levels(dt3$ForkLength))[as.integer(dt3$ForkLength) ]               
if (class(dt3$ForkLength)=="character") dt3$ForkLength <-as.numeric(dt3$ForkLength)
if (class(dt3$RaceByLength)!="factor") dt3$RaceByLength<- as.factor(dt3$RaceByLength)
if (class(dt3$Count)=="factor") dt3$Count <-as.numeric(levels(dt3$Count))[as.integer(dt3$Count) ]               
if (class(dt3$Count)=="character") dt3$Count <-as.numeric(dt3$Count)
                
# Convert Missing Values to NA for non-dates
                
dt3$WeatherCode <- as.factor(ifelse((trimws(as.character(dt3$WeatherCode))==trimws("NA")),NA,as.character(dt3$WeatherCode)))
dt3$DO <- ifelse((trimws(as.character(dt3$DO))==trimws("NA")),NA,dt3$DO)               
suppressWarnings(dt3$DO <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$DO))==as.character(as.numeric("NA"))),NA,dt3$DO))
dt3$WaterTemperature <- ifelse((trimws(as.character(dt3$WaterTemperature))==trimws("NA")),NA,dt3$WaterTemperature)               
suppressWarnings(dt3$WaterTemperature <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$WaterTemperature))==as.character(as.numeric("NA"))),NA,dt3$WaterTemperature))
dt3$Turbidity <- ifelse((trimws(as.character(dt3$Turbidity))==trimws("NA")),NA,dt3$Turbidity)               
suppressWarnings(dt3$Turbidity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Turbidity))==as.character(as.numeric("NA"))),NA,dt3$Turbidity))
dt3$Secchi <- ifelse((trimws(as.character(dt3$Secchi))==trimws("NA")),NA,dt3$Secchi)               
suppressWarnings(dt3$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Secchi))==as.character(as.numeric("NA"))),NA,dt3$Secchi))
dt3$Conductivity <- ifelse((trimws(as.character(dt3$Conductivity))==trimws("NA")),NA,dt3$Conductivity)               
suppressWarnings(dt3$Conductivity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Conductivity))==as.character(as.numeric("NA"))),NA,dt3$Conductivity))
dt3$TowNumber <- ifelse((trimws(as.character(dt3$TowNumber))==trimws("NA")),NA,dt3$TowNumber)               
suppressWarnings(dt3$TowNumber <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$TowNumber))==as.character(as.numeric("NA"))),NA,dt3$TowNumber))
dt3$TowDirectionCode <- as.factor(ifelse((trimws(as.character(dt3$TowDirectionCode))==trimws("NA")),NA,as.character(dt3$TowDirectionCode)))
dt3$TowDuration <- ifelse((trimws(as.character(dt3$TowDuration))==trimws("NA")),NA,dt3$TowDuration)               
suppressWarnings(dt3$TowDuration <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$TowDuration))==as.character(as.numeric("NA"))),NA,dt3$TowDuration))
dt3$flowDebris <- as.factor(ifelse((trimws(as.character(dt3$flowDebris))==trimws("NA")),NA,as.character(dt3$flowDebris)))
dt3$SiteDisturbance <- as.factor(ifelse((trimws(as.character(dt3$SiteDisturbance))==trimws("NA")),NA,as.character(dt3$SiteDisturbance)))
dt3$AlternateSite <- as.factor(ifelse((trimws(as.character(dt3$AlternateSite))==trimws("NA")),NA,as.character(dt3$AlternateSite)))
dt3$SeineLength <- ifelse((trimws(as.character(dt3$SeineLength))==trimws("NA")),NA,dt3$SeineLength)               
suppressWarnings(dt3$SeineLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$SeineLength))==as.character(as.numeric("NA"))),NA,dt3$SeineLength))
dt3$SeineWidth <- ifelse((trimws(as.character(dt3$SeineWidth))==trimws("NA")),NA,dt3$SeineWidth)               
suppressWarnings(dt3$SeineWidth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$SeineWidth))==as.character(as.numeric("NA"))),NA,dt3$SeineWidth))
dt3$SeineDepth <- ifelse((trimws(as.character(dt3$SeineDepth))==trimws("NA")),NA,dt3$SeineDepth)               
suppressWarnings(dt3$SeineDepth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$SeineDepth))==as.character(as.numeric("NA"))),NA,dt3$SeineDepth))
dt3$StartMeter <- ifelse((trimws(as.character(dt3$StartMeter))==trimws("NA")),NA,dt3$StartMeter)               
suppressWarnings(dt3$StartMeter <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$StartMeter))==as.character(as.numeric("NA"))),NA,dt3$StartMeter))
dt3$EndMeter <- ifelse((trimws(as.character(dt3$EndMeter))==trimws("NA")),NA,dt3$EndMeter)               
suppressWarnings(dt3$EndMeter <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$EndMeter))==as.character(as.numeric("NA"))),NA,dt3$EndMeter))
dt3$TotalMeter <- ifelse((trimws(as.character(dt3$TotalMeter))==trimws("NA")),NA,dt3$TotalMeter)               
suppressWarnings(dt3$TotalMeter <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$TotalMeter))==as.character(as.numeric("NA"))),NA,dt3$TotalMeter))
dt3$Volume <- ifelse((trimws(as.character(dt3$Volume))==trimws("NA")),NA,dt3$Volume)               
suppressWarnings(dt3$Volume <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Volume))==as.character(as.numeric("NA"))),NA,dt3$Volume))
dt3$StageCode <- as.factor(ifelse((trimws(as.character(dt3$StageCode))==trimws("n/p")),NA,as.character(dt3$StageCode)))
dt3$Maturation <- as.factor(ifelse((trimws(as.character(dt3$Maturation))==trimws("n/p")),NA,as.character(dt3$Maturation)))
dt3$ForkLength <- ifelse((trimws(as.character(dt3$ForkLength))==trimws("NA")),NA,dt3$ForkLength)               
suppressWarnings(dt3$ForkLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$ForkLength))==as.character(as.numeric("NA"))),NA,dt3$ForkLength))
dt3$RaceByLength <- as.factor(ifelse((trimws(as.character(dt3$RaceByLength))==trimws("n/p")),NA,as.character(dt3$RaceByLength)))
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
summary(WaterTemperature)
summary(Turbidity)
summary(Secchi)
summary(Conductivity)
summary(TowNumber)
summary(TowDirectionCode)
summary(TowDuration)
summary(flowDebris)
summary(SiteDisturbance)
summary(AlternateSite)
summary(SeineLength)
summary(SeineWidth)
summary(SeineDepth)
summary(StartMeter)
summary(EndMeter)
summary(TotalMeter)
summary(Volume)
summary(OrganismCode)
summary(CommonName)
summary(MarkCode)
summary(StageCode)
summary(Maturation)
summary(ForkLength)
summary(RaceByLength)
summary(Count) 
                # Get more details on character variables
                 
summary(as.factor(dt3$Location)) 
summary(as.factor(dt3$RegionCode)) 
summary(as.factor(dt3$StationCode)) 
summary(as.factor(dt3$MethodCode)) 
summary(as.factor(dt3$GearConditionCode)) 
summary(as.factor(dt3$WeatherCode)) 
summary(as.factor(dt3$TowDirectionCode)) 
summary(as.factor(dt3$flowDebris)) 
summary(as.factor(dt3$SiteDisturbance)) 
summary(as.factor(dt3$AlternateSite)) 
summary(as.factor(dt3$OrganismCode)) 
summary(as.factor(dt3$CommonName)) 
summary(as.factor(dt3$MarkCode)) 
summary(as.factor(dt3$StageCode)) 
summary(as.factor(dt3$Maturation)) 
summary(as.factor(dt3$RaceByLength))
detach(dt3)               

####################################         
# 4. Name: DJFMP_Fish_Taxonomy.csv #
# File: DJFMP_Fish_Taxonomy.csv    # 
####################################

inUrl4  <- "https://pasta.lternet.edu/package/data/eml/edi/244/5/17c9974d9b7b0125c146a887f3c64bd8" 
infile4 <- tempfile()
try(download.file(inUrl4,infile4,method="curl"))
if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")

                   
 dt4 <-read.csv(infile4,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "OrganismCode",     
                    "CommonName",     
                    "NonNative",     
                    "Phylum",     
                    "Class",     
                    "Order",     
                    "Family",     
                    "Genus",     
                    "Species",     
                    "Active"    ), check.names=TRUE)
               
unlink(infile4)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt4$OrganismCode)!="factor") dt4$OrganismCode<- as.factor(dt4$OrganismCode)
if (class(dt4$CommonName)!="factor") dt4$CommonName<- as.factor(dt4$CommonName)
if (class(dt4$NonNative)!="factor") dt4$NonNative<- as.factor(dt4$NonNative)
if (class(dt4$Phylum)!="factor") dt4$Phylum<- as.factor(dt4$Phylum)
if (class(dt4$Class)!="factor") dt4$Class<- as.factor(dt4$Class)
if (class(dt4$Order)!="factor") dt4$Order<- as.factor(dt4$Order)
if (class(dt4$Family)!="factor") dt4$Family<- as.factor(dt4$Family)
if (class(dt4$Genus)!="factor") dt4$Genus<- as.factor(dt4$Genus)
if (class(dt4$Species)!="factor") dt4$Species<- as.factor(dt4$Species)
if (class(dt4$Active)!="factor") dt4$Active<- as.factor(dt4$Active)
                
# Convert Missing Values to NA for non-dates
                
dt4$Phylum <- as.factor(ifelse((trimws(as.character(dt4$Phylum))==trimws("NA")),NA,as.character(dt4$Phylum)))
dt4$Class <- as.factor(ifelse((trimws(as.character(dt4$Class))==trimws("NA")),NA,as.character(dt4$Class)))
dt4$Order <- as.factor(ifelse((trimws(as.character(dt4$Order))==trimws("NA")),NA,as.character(dt4$Order)))
dt4$Family <- as.factor(ifelse((trimws(as.character(dt4$Family))==trimws("NA")),NA,as.character(dt4$Family)))
dt4$Genus <- as.factor(ifelse((trimws(as.character(dt4$Genus))==trimws("NA")),NA,as.character(dt4$Genus)))
dt4$Species <- as.factor(ifelse((trimws(as.character(dt4$Species))==trimws("NA")),NA,as.character(dt4$Species)))


# Here is the structure of the input data frame:
str(dt4)                            
attach(dt4)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(OrganismCode)
summary(CommonName)
summary(NonNative)
summary(Phylum)
summary(Class)
summary(Order)
summary(Family)
summary(Genus)
summary(Species)
summary(Active) 
                # Get more details on character variables
                 
summary(as.factor(dt4$OrganismCode)) 
summary(as.factor(dt4$CommonName)) 
summary(as.factor(dt4$NonNative)) 
summary(as.factor(dt4$Phylum)) 
summary(as.factor(dt4$Class)) 
summary(as.factor(dt4$Order)) 
summary(as.factor(dt4$Family)) 
summary(as.factor(dt4$Genus)) 
summary(as.factor(dt4$Species)) 
summary(as.factor(dt4$Active))
detach(dt4)               
         
#####################################
# 5. Name: DJFMP_Site_Locations.csv #
# File: DJFMP_Site_Locations.csv    # 
#####################################

inUrl5  <- "https://pasta.lternet.edu/package/data/eml/edi/244/5/99a038d691f27cd306ff93fdcbc03b77" 
infile5 <- tempfile()
try(download.file(inUrl5,infile5,method="curl"))
if (is.na(file.size(infile5))) download.file(inUrl5,infile5,method="auto")

                   
 dt5 <-read.csv(infile5,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "MethodCode",     
                    "Location",     
                    "StationCode",     
                    "Latitude_location",     
                    "Longitude_location"    ), check.names=TRUE)
               
unlink(infile5)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt5$MethodCode)!="factor") dt5$MethodCode<- as.factor(dt5$MethodCode)
if (class(dt5$Location)!="factor") dt5$Location<- as.factor(dt5$Location)
if (class(dt5$StationCode)!="factor") dt5$StationCode<- as.factor(dt5$StationCode)
if (class(dt5$Latitude_location)=="factor") dt5$Latitude_location <-as.numeric(levels(dt5$Latitude_location))[as.integer(dt5$Latitude_location) ]               
if (class(dt5$Latitude_location)=="character") dt5$Latitude_location <-as.numeric(dt5$Latitude_location)
if (class(dt5$Longitude_location)=="factor") dt5$Longitude_location <-as.numeric(levels(dt5$Longitude_location))[as.integer(dt5$Longitude_location) ]               
if (class(dt5$Longitude_location)=="character") dt5$Longitude_location <-as.numeric(dt5$Longitude_location)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt5)                            
attach(dt5)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(MethodCode)
summary(Location)
summary(StationCode)
summary(Latitude_location)
summary(Longitude_location) 
                # Get more details on character variables
                 
summary(as.factor(dt5$MethodCode)) 
summary(as.factor(dt5$Location)) 
summary(as.factor(dt5$StationCode))
detach(dt5)               

##############################################################
# remove all the infile and inURL that we don't need anymore #
remove(infile1, infile2, infile3, infile4, infile5,
       inUrl1,inUrl2,inUrl3,inUrl4,inUrl5)
