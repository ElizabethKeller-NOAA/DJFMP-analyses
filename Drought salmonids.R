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
# consider: addWaterYear() in the dataRetrieval package


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