# packages
library(here)
library(readxl)
library(lubridate)
library(ggplot2)
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

# read in via 'DJFMP data.R"            

#####################
# data manipulation #
#####################

# Add Water Year
dt2$WY <- year(dt2$SampleDate) + 
  as.numeric(between(month(dt2$SampleDate),10,12))

# clip dt2 to station we want
# Sherwood Harbor
SH_Trawl <- dt2[dt2$StationCode=="SR055M",]
#remove(dt2)

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
for(j in c(1,3,5,7,9,11,13)){PPdays <- j # number of days of the pulse period
PPdaybounds <- ((PPdays-1) #peakday
                /2 # days on each side
                +1) # using exclusive not inclusive <> below
SPP_results_fill <- SPP_results
for (i in 1:length(SPP_results$WY)){
  temp_dataset <- SPP[SPP$WY==SPP_results$WY[i] & as.Date(SPP$SampleDate) > SPP_results$Peak_Date[i]-PPdaybounds
                      & as.Date(SPP$SampleDate) < SPP_results$Peak_Date[i]+PPdaybounds,]
  # sum of daily catch for just the +-5 days period around the peak date  
  SPP_results_fill$PP_sum[i] <- sum(temp_dataset$Catch_sum)
  # add variable for number of days in pulse period
  SPP_results_fill$PP_days <- PPdays
} 
# want to add each SPP_results_fill for each period length to one dataframe
ifelse(j==1,
       (SPP_results <- SPP_results_fill),
       (SPP_results <- rbind(SPP_results,SPP_results_fill)))
}
# Sherwood Harbor trawl is not daily, so we won't get 11 values - only ~4
# should still give a rough percentage of total catch, but coarser than daily data

SPP_results$proportion <- SPP_results$PP_sum / SPP_results$Annual_sum
# optionally give pretty names to columns for presenting as table
#colnames(SPP_results) <- c("Water Year","Date of Peak Catch",
#                           "Pulse Period Catch","Annual Catch",
#                           "Sac Water Year Type",
#                           "Catch Proportion during Pulse Period","Pulse Period Length")
#write.csv(SPP_results, "SPP_results.csv", row.names=FALSE)

# PLOT 
# line graph with line for each PP_days
ggplot(SPP_results) +             # Create ggplot2 plot
  geom_line(data=SPP_results, aes(x=WY, y=proportion*100, group = PP_days, col = PP_days))+
  #geom_tile(data=SPP_results,aes(x=WY, y=max(proportion)*10/2,fill=as.factor(`Sac_Yr-type`)), # WY type as background color
  #          height=max(SPP_results$proportion)*10,alpha=0.2)+
  # alpha isn't working to lower transparency???
  labs(title="Percentage of spring-run catch during peak migration", x="Water Year", 
       y = "percentage of annual catch during pulse period") + labs(colour = "Days")


