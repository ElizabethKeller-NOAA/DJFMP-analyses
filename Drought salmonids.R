# packages
library(here)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(ggridges)

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
# read in DJFMP data
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

# add day of water year
# use difftime to get wtr_day to be the number of days from 9-30 
dataset <- dataset %>%
  group_by(WY) %>% 
  mutate(wtr_day = (as.integer(difftime(
    SampleDate,ymd(paste0(WY - 1 ,'-09-30')), units = "days"))))


# Add water year type & drought period year type
dataset <- left_join(dataset,DroughtYr[,c(1:5)], by="WY")

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

# remove WY 2022, since it is incomplete
Trawl <-Trawl[!Trawl$WY==2022,]

################################################
# creating a dataframe of sample dates         #
# want a row for every species/run of interest #
# for every sample date                        #
################################################

# this will ensure we have a 0 count for samples when there was sampling
  # but wasn't any catch of each species of interest

# dataframe of every sampling date*location
  # removed station code b/c it was actually by station (not "location") before
Trawl_sampledates <-Trawl %>%
  group_by(SampleDate) %>%
  dplyr::summarize(
    WY = WY,
    Location = Location,
    StationCode = StationCode,
    SampleDate = SampleDate,
    MethodCode = MethodCode,
    TowNumber = TowNumber,
    #ForkLength = ForkLength,
    wtr_day = wtr_day,
    Drought = Drought,
    Yr_type = Yr_type,
    DroughtYear = DroughtYear
  ) 
#remove duplicate rows
Trawl_sampledates <- Trawl_sampledates[!duplicated(Trawl_sampledates),]
#length(Trawl_sampledates$SampleDate)
#length(unique(Trawl_sampledates$SampleDate))

# need this whole df for every species
# add Species (empty for now)
Trawl_sampledates$Species <- character(length(Trawl_sampledates$SampleDate))

# want juvenile NMFS ESA/MSA species
species_list <- c("Fall-run Chinook salmon",
                  #"NA-run Chinook salmon",
                  "Winter-run Chinook salmon",
                  "steelhead trout",
                  "LateFall-run Chinook salmon",
                  "Spring-run Chinook salmon")

# create dataframe with a row for each date*location*each species of interest
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
#length(Trawl_sampledates$SampleDate)
#length(interest_samples$SampleDate)/length(species_list)

#add Count to samples dataframe (zeros here; will be combined with count data)
interest_samples$Count <- 0
# add back Fork_Length
interest_samples$ForkLength <- numeric(length(interest_samples$SampleDate))

#############################################
# combine data w/ dates/species of interest #
#############################################

# will have at least one row for every date*location*species of interest

# clip trawl data to that of interest
Trawl2 <- Trawl[(Trawl$Species %in% species_list),# clip to species of interest
                c(colnames(interest_samples))] # clip to the same columns as my sample/date/species list

# combine data with rows in interest samples
Trawl2 <- rbind(Trawl2,interest_samples) #


###################################
# expanded dataframe for plotting #
###################################
# for the plots, need to expand this to have a row per sample/count
# need the number of rows = count for ridge/density plotting
# expand rows to account for Catch
Trawl2.expanded <- Trawl2[rep(row.names(Trawl2), Trawl2$Count), ]

# rerunning the code and the WYT is not ordered factor...
# let's fix that
Trawl2.expanded$Yr_type <- factor(Trawl2.expanded$Yr_type, levels=c("Wet","Above Normal","Below Normal","Dry","Critical"), ordered=TRUE)

# other dataset for winter-run lengths
Trawl2.expanded_L <- Trawl2.expanded[Trawl2.expanded$ForkLength>0 & !is.na(Trawl2.expanded$ForkLength),]


######################
#        PLOTS       #
# Timing and Lengths #
######################

# for fall-, spring-, and winter-run Chinook salmon only for manuscript


####################################
## by multi-year drought year type #
####################################

# migration timing histograms - WR, Sherwood
Trawl2.expanded[(Trawl2.expanded$Species %in% c("Winter-run Chinook salmon") & Trawl2.expanded$Location %in% c("Sherwood Harbor"))#,"Spring-run Chinook salmon","Fall-run Chinook salmon"
                                               ,] %>%
  mutate(across(Location, factor, levels=c("Sherwood Harbor"#,"Chipps Island"
                                           ))) %>%
  
ggplot(aes(x = wtr_day, fill = DroughtYear)) +
  scale_fill_brewer(name="Drought Year", palette = "Dark2", labels=c("Wet years", "First/only dry year","Second dry year","Three or more dry years")) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 100)

################
# polygons?
#################
# migration timing at Sherwood, WR

Trawl2.expanded[(Trawl2.expanded$Species %in% c("Winter-run Chinook salmon") & Trawl2.expanded$Location %in% c("Sherwood Harbor"))#,"Spring-run Chinook salmon","Fall-run Chinook salmon"
                ,] %>%
  mutate(across(Location, factor, levels=c("Sherwood Harbor"#,"Chipps Island"
  ))) %>%
  
  ggplot(aes(x = wtr_day, color = DroughtYear, 
                       fill = DroughtYear)) +
  geom_freqpoly(binwidth = 10) +
  scale_fill_brewer(name="Drought Year", palette = "Dark2", labels=c("Wet years", "First/only dry year","Second dry year","Three or more dry years")) +
  scale_color_brewer(name="Drought Year", palette = "Dark2", labels=c("Wet years", "First/only dry year","Second dry year","Three or more dry years")) +
  labs(title = "WR Chinook - Sherwood", 
       x = "Day of the water year", 
       y = "Frequency") + theme_bw() #+
  #scale_y_discrete(labels = c("Fall-run","Spring-run","Winter-run")) + # labels bottom to top??
  #facet_wrap(~Location)
#ggsave("Trawls DroughtYear timing.tiff", device = "tiff", width = 9, height = 6, units = "in")


################
# migration timing at Sherwood and Chipps
# density ridges
Trawl2.expanded[Trawl2.expanded$Species %in% c("Winter-run Chinook salmon","Spring-run Chinook salmon","Fall-run Chinook salmon"),] %>%
  mutate(across(Location, factor, levels=c("Sherwood Harbor","Chipps Island"))) %>%
  
  ggplot(aes(x = wtr_day, y = Species, 
             color = DroughtYear, 
             fill = DroughtYear)) + scale_fill_brewer(name="Drought Year", palette = "Dark2", labels=c("Wet years", "First/only dry year","Second dry year","Three or more dry years")) +
  scale_color_brewer(name="Drought Year", palette = "Dark2", labels=c("Wet years", "First/only dry year","Second dry year","Three or more dry years")) +
  geom_density_ridges(alpha = .3, scale=0.95) +
  labs(title = "Chinook Salmon Trawl Catch Timing (1988-2021)", 
       x = "Day of the water year", 
       y = "Run Type") + theme_bw() +
  scale_y_discrete(labels = c("Fall-run","Spring-run","Winter-run")) + # labels bottom to top??
  facet_wrap(~Location)
ggsave("Trawls DroughtYear timing.tiff", device = "tiff", width = 9, height = 6, units = "in")

# winter-run lengths at Sherwood and Chipps
# winter-run lengths only - both trawls
#tiff("Winter-run WYT lengths.tiff", width = 8, height = 6, units = 'in', res = 300)
ggplot(Trawl2.expanded_L[Trawl2.expanded_L$Species=="Winter-run Chinook salmon"&
                           (Trawl2.expanded_L$Location=="Chipps Island" |
                              Trawl2.expanded_L$Location=="Sherwood Harbor"),],
       aes(x = ForkLength, y = Location, 
           color = DroughtYear, 
           fill = DroughtYear)) + scale_fill_brewer(name="Drought Year", palette = "Dark2", labels=c("Wet years", "First/only dry year","Second dry year","Three or more dry years")) +
  scale_color_brewer(name="Drought Year", palette = "Dark2", labels=c("Wet years", "First/only dry year","Second dry year","Three or more dry years")) +
  geom_density_ridges(alpha = .3, scale=0.7) +
  labs(title = "Winter-run Chinook Lengths (1988-2021)", 
       x = "Fork length (mm)", 
       y = "Trawl Location") + theme_bw()
ggsave("Winter-run DroughtYear lengths.tiff", device = "tiff", width = 7, height = 6, units = "in")

# winter-run lengths vs day at Sherwood
ggplot(Trawl2.expanded_L[Trawl2.expanded_L$Species=="Winter-run Chinook salmon"&
                           (Trawl2.expanded_L$Location=="Sherwood Harbor"),],
       aes(x = wtr_day, y = ForkLength, 
           color = DroughtYear, 
           #fill = Yr_type, 
           #alpha=0.2
       )) +
  geom_point()+
  #scale_fill_manual(name = "Water Year Type",values=pal_yrtype) +
  scale_color_brewer(name="Drought Year", palette = "Dark2", labels=c("Wet years", "First/only dry year","Second dry year","Three or more dry years")) +
  labs(title = "Sherwood Harbor Winter-run Chinook Lengths (1988-2021)", 
       x = "Day of the water year", 
       y = "Fork length (mm)") + theme_bw()
ggsave("Sherwood Winter-run lengths timing DroughtYear.tiff", device = "tiff", width = 7, height = 5, units = "in")

##############################################################################

# Migration timing
# Sherwood Harbor
#tiff("Sherwood WYT timing.tiff", width = 8, height = 8, units = 'in', res = 300)
ggplot(Trawl2.expanded[Trawl2.expanded$Location=="Sherwood Harbor" & Trawl2.expanded$WY > 1987 & Trawl2.expanded$Species %in% c("Fall-run Chinook salmon","Spring-run Chinook salmon","Winter-run Chinook salmon"),],
       aes(x = wtr_day, y = Species, 
           color = Yr_type, 
           fill = Yr_type)) +scale_fill_manual(name = "Water Year Type",values=pal_yrtype) +
  scale_color_manual(name = "Water Year Type",values=pal_yrtype) +
  geom_density_ridges(alpha = .3, scale=0.95) +
  labs(title = "Sherwood Harbor Trawl Migration Timing (1988-2021)", 
       x = "Day of the water year", 
       y = "Run Type") + theme_bw() +
  scale_y_discrete(labels = c("Winter-run","Spring-run","Fall-run")) # don't need full species, just run
ggsave("Sherwood WYT timing.tiff", device = "tiff", width = 7, height = 6, units = "in")
#dev.copy(svg,"Sherwood WYT timing.svg")
#dev.off()

# Chipps Island
#tiff("Chipps WYT timing.tiff", width = 8, height = 8, units = 'in', res = 300)
ggplot(Trawl2.expanded[Trawl2.expanded$Location=="Chipps Island" & Trawl2.expanded$Species %in% c("Fall-run Chinook salmon","Spring-run Chinook salmon","Winter-run Chinook salmon"),],
       aes(x = wtr_day, y = Species, 
           color = Yr_type, 
           fill = Yr_type)) +scale_fill_manual(name = "Water Year Type",values=pal_yrtype) +
  scale_color_manual(name = "Water Year Type",values=pal_yrtype) +
  geom_density_ridges(alpha = .3, scale=0.95) +
  labs(title = "Chipps Island Trawl Migration Timing (1988-2021)", 
       x = "Day of the water year", 
       y = "Run Type") + theme_bw() +
  scale_y_discrete(labels = c("Winter-run","Spring-run","Fall-run")) # don't need full species, just run
ggsave("Chipps WYT timing.tiff", device = "tiff", width = 7, height = 6, units = "in")
#dev.copy(svg,"Chipps WYT timing.svg")
#dev.off()

# both trawl locations in one figure - final manuscript figure (inital submission)

# add code to switch order of sites (Sherwood on left)
Trawl2.expanded[Trawl2.expanded$Species %in% c("Winter-run Chinook salmon","Spring-run Chinook salmon","Fall-run Chinook salmon"),] %>%
  mutate(across(Location, factor, levels=c("Sherwood Harbor","Chipps Island"))) %>%

ggplot(aes(x = wtr_day, y = Species, 
           color = Yr_type, 
           fill = Yr_type)) +scale_fill_manual(name = "Water Year Type",values=pal_yrtype) +
  scale_color_manual(name = "Water Year Type",values=pal_yrtype) +
  geom_density_ridges(alpha = .3, scale=0.95) +
  labs(title = "Chinook Salmon Trawl Catch Timing (1988-2021)", 
       x = "Day of the water year", 
       y = "Run Type") + theme_bw() +
  scale_y_discrete(labels = c("Fall-run","Spring-run","Winter-run")) + # labels bottom to top??
  facet_wrap(~Location)
ggsave("Trawls WYT timing.tiff", device = "tiff", width = 9, height = 6, units = "in")

# Lengths
# Sherwood Harbor
tiff("Sherwood WYT lengths.tiff", width = 8, height = 8, units = 'in', res = 300)
ggplot(Trawl2.expanded_L[Trawl2.expanded_L$Location=="Sherwood Harbor",],
       aes(x = ForkLength, y = Species, 
           color = Yr_type, 
           fill = Yr_type)) +scale_fill_manual(name = "Water Year Type",values=pal_yrtype) +
  scale_color_manual(name = "Water Year Type",values=pal_yrtype) +
  geom_density_ridges(alpha = .3, scale=0.95) +
  labs(title = "Sherwood Harbor Trawl Fish Lengths (1988-2021)", 
       x = "Fork length", 
       y = "Species") + theme_bw()
#dev.copy(svg,"Sherwood WYT lengths.svg")
dev.off()

# Chipps Island
tiff("Chipps WYT lengths.tiff", width = 8, height = 8, units = 'in', res = 300)
ggplot(Trawl2.expanded_L[Trawl2.expanded_L$Location=="Chipps Island",],
       aes(x = ForkLength, y = Species, 
           color = Yr_type, 
           fill = Yr_type)) +scale_fill_manual(name = "Water Year Type",values=pal_yrtype) +
  scale_color_manual(name = "Water Year Type",values=pal_yrtype) +
  geom_density_ridges(alpha = .3, scale=0.95) +
  labs(title = "Chipps Island Trawl Fish Lengths (1988-2021)", 
       x = "Fork length", 
       y = "Species") + theme_bw()
#dev.copy(svg,"Chipps WYT lengths.svg")
dev.off()

# winter-run lengths only - both trawls
#tiff("Winter-run WYT lengths.tiff", width = 8, height = 6, units = 'in', res = 300)
ggplot(Trawl2.expanded_L[Trawl2.expanded_L$Species=="Winter-run Chinook salmon"&
                               (Trawl2.expanded_L$Location=="Chipps Island" |
                                  Trawl2.expanded_L$Location=="Sherwood Harbor"),],
       aes(x = ForkLength, y = Location, 
           color = Yr_type, 
           fill = Yr_type)) +scale_fill_manual(name = "Water Year Type",values=pal_yrtype) +
  scale_color_manual(name = "Water Year Type",values=pal_yrtype) +
  geom_density_ridges(alpha = .3, scale=0.7) +
  labs(title = "Winter-run Chinook Lengths (1988-2021)", 
       x = "Fork length (mm)", 
       y = "Trawl Location") + theme_bw()
ggsave("Winter-run WYT lengths.tiff", device = "tiff", width = 7, height = 6, units = "in")
#dev.copy(svg,"Winter-run WYT lengths.svg")
#dev.off()


#####################
# lengths vs timing #
#####################

test <- Trawl2.expanded_L[Trawl2.expanded_L$Species=="Winter-run Chinook salmon"&
                            (Trawl2.expanded_L$Location=="Sherwood Harbor"),]

# are those smaller fish at Sherwood during dry and BN years the same ones that appear early in the water year? 
ggplot(Trawl2.expanded_L[Trawl2.expanded_L$Species=="Winter-run Chinook salmon"&
                           (Trawl2.expanded_L$Location=="Sherwood Harbor"),],
       aes(x = wtr_day, y = ForkLength, 
           color = Yr_type, 
           #fill = Yr_type, 
           #alpha=0.2
           )) +
  geom_point()+
  #scale_fill_manual(name = "Water Year Type",values=pal_yrtype) +
  scale_color_manual(name = "Water Year Type",values=pal_yrtype) +
  labs(title = "Sherwood Harbor Winter-run Chinook Lengths (1988-2021)", 
       x = "Day of the water year", 
       y = "Fork length (mm)") + theme_bw()
ggsave("Sherwood Winter-run lengths timing.tiff", device = "tiff", width = 7, height = 5, units = "in")

############
# sample sizes
# sample sizes *for migration timing*
# lengths has less due to zeros
Chipps <- Trawl2.expanded[Trawl2.expanded$Location=="Chipps Island",]
table(Chipps$Species) # not matching up

Sherwood <- Trawl2.expanded[Trawl2.expanded$Location=="Sherwood Harbor",]
table(Sherwood$Species)

## alternate timing sample size calculation
Trawl2$SpeciesLocation <- paste(Trawl2$Species,Trawl2$Location)
TSS <- Trawl2 %>%
  group_by(SpeciesLocation) %>%
  dplyr::summarize(
    Species = Species,
    Location = Location,
    Catch_sum = sum(Count)
  ) 
TSS <-TSS[!duplicated(TSS),]

# for lengths
Chipps <- Trawl2.expanded_L[Trawl2.expanded_L$Location=="Chipps Island",]
table(Chipps$Species)
Sherwood <- Trawl2.expanded_L[Trawl2.expanded_L$Location=="Sherwood Harbor",]
table(Sherwood$Species)

#####################
# Boxplots for Evan #
#   escapement/CRR  #
#####################

# CRR data
# winter-run
WR_CRR <- 
  read_excel("Grandtab WR FR SR CRR v2.xlsx",
             sheet = "WR 2022")
# spring-run
SR_CRR <- 
  read_excel("Grandtab WR FR SR CRR v2.xlsx",
             sheet = "SR 2022")
# Fall-run
FR_CRR <- 
  read_excel("Grandtab WR FR SR CRR v2.xlsx",
             sheet = "CV FR 2022")

# need to make WYT an ordered factor
WR_CRR$WYT <- factor(WR_CRR$WYT, levels=c("C","D","BN","AN","W"), ordered=TRUE)
SR_CRR$WYT <- factor(SR_CRR$WYT, levels=c("C","D","BN","AN","W"), ordered=TRUE)
FR_CRR$WYT <- factor(FR_CRR$WYT, levels=c("C","D","BN","AN","W"), ordered=TRUE)

########################
# by DROUGHT year type #
########################

tiff("Winter-run CRR D.tiff", width = 8, height = 6, units = 'in', res = 300)
ggplot(WR_CRR, aes(x=Drought, y=CRR, fill=Drought)) + 
  geom_boxplot() +theme_bw() + scale_fill_manual(values=pal_drought, labels=c('Dry', 'Neutral', 'Wet'))+
  labs(title = "Age Structure Winter-run CRR 1974-2021", 
       x = "Juvenile migration drought year type", 
       y = "Cohort Replacement Rate (CRR)")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, color="black")
#dev.copy(svg,"C:/Users/Elizabeth.keller/Desktop/DCP/Drought data/Winter-run CRR.svg")
dev.off()

tiff("Sring-run CRR D.tiff", width = 8, height = 6, units = 'in', res = 300)
ggplot(SR_CRR, aes(x=Drought, y=CRR, fill=Drought)) + 
  geom_boxplot() +theme_bw() + scale_fill_manual(values=pal_drought, labels=c('Dry', 'Neutral', 'Wet'))+
  labs(title = "Age Structure Spring-run CRR 1974-2021", 
       x = "Juvenile migration drought year type", 
       y = "Cohort Replacement Rate (CRR)")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, color="black")
#dev.copy(svg,"C:/Users/Elizabeth.keller/Desktop/DCP/Drought data/Spring-run CRR.svg")
dev.off()

tiff("Fall-run CRR D.tiff", width = 8, height = 6, units = 'in', res = 300)
ggplot(FR_CRR, aes(x=Drought, y=CRR, fill=Drought)) + 
  geom_boxplot() +theme_bw() + scale_fill_manual(values=pal_drought, labels=c('Dry', 'Neutral', 'Wet'))+
  labs(title = "Age Structure Fall-run CRR 1974-2021", 
       x = "Juvenile migration drought year type", 
       y = "Cohort Replacement Rate (CRR)")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, color="black")
#dev.copy(svg,"C:/Users/Elizabeth.keller/Desktop/DCP/Drought data/Fall-run CRR.svg")
dev.off()

############################
# by WATER YEAR TYPE (WYT) #
############################

tiff("Winter-run CRR WYT.tiff", width = 8, height = 4, units = 'in', res = 300)
ggplot(WR_CRR, aes(x=WYT, y=CRR, fill=WYT)) + 
  geom_boxplot() +theme_bw() + scale_fill_manual(values=pal_yrtype2, labels=c(
    'Critical','Dry','Below Normal','Above Normal','Wet'))+
  labs(title = "Age Structure Winter-run CRR 1974-2021", 
       x = "Juvenile migration water year type", 
       y = "Cohort Replacement Rate (CRR)")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, color="black")
dev.off()

tiff("Sring-run CRR WYT.tiff", width = 8, height = 4, units = 'in', res = 300)
ggplot(SR_CRR, aes(x=WYT, y=CRR, fill=WYT)) + 
  geom_boxplot() +theme_bw() + scale_fill_manual(values=pal_yrtype2, labels=c(
    'Critical','Dry','Below Normal','Above Normal','Wet'))+
  labs(title = "Age Structure Spring-run CRR 1974-2021", 
       x = "Juvenile migration water year type", 
       y = "Cohort Replacement Rate (CRR)")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, color="black")
dev.off()

tiff("Fall-run CRR WYT.tiff", width = 8, height = 4, units = 'in', res = 300)
ggplot(FR_CRR, aes(x=WYT, y=CRR, fill=WYT)) + 
  geom_boxplot() +theme_bw() + scale_fill_manual(values=pal_yrtype2, labels=c(
    'Critical','Dry','Below Normal','Above Normal','Wet'))+
  labs(title = "Age Structure Fall-run CRR 1974-2021", 
       x = "Juvenile migration water year type", 
       y = "Cohort Replacement Rate (CRR)")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, color="black")
dev.off()

##############################
#      CRR WYT boxplots      #
#    pairwise comparisons    #
# incomplete **************************************
##############################
# boxplot w pairwise comparisons

# all comparisons
# list(c("C","W"),c("D","W"),c("BN","W"),c("AN","W"),c("C", "AN"), c("D", "AN"),c("BN", "AN"), c("C", "BN"),c("D","BN"),c("C","D"))

library(ggsignif)

# winter-run
ggplot(WR_CRR, aes(x=WYT, y=CRR, fill=WYT)) +
  ylim(-0.5, (max(WR_CRR$CRR)+2)) + # was (0, 15)
  geom_boxplot() +
  geom_signif( # add the significance markers
    comparisons = list(c("C","W")),
    y_position = c(7),
    map_signif_level = TRUE,
    textsize = 8
  ) +
  scale_fill_manual(values = pal_yrtype2, labels=c(
    'Critical','Dry','Below Normal','Above Normal','Wet')) +
  labs(title = "Winter-run Cohort Replacement Rate 1974-2021", 
       x = "Juvenile migration water year type", 
       y = "Cohort Replacement Rate (CRR)",
       fill = "Water Year Type") +
  theme_bw()
ggsave("Winter-run CRR WYT.tiff", device = "tiff", width = 7, height = 6, units = "in")

# spring-run
ggplot(SR_CRR, aes(x=WYT, y=CRR, fill=WYT)) +
  ylim(-0.5, (max(SR_CRR$CRR)+6)) + # was (0, 15)
  geom_boxplot() +
  geom_signif( # add the significance markers
    comparisons = list(c("C","W"),c("D","W"),c("C", "AN"), c("D", "AN"), c("C", "BN")),
    y_position = c(11,10, 9, 8, 7),
    map_signif_level = TRUE,
    textsize = 8
  ) +
  scale_fill_manual(values = pal_yrtype2, labels=c(
    'Critical','Dry','Below Normal','Above Normal','Wet')) +
  labs(title = "Spring-run Cohort Replacement Rate 1974-2021", 
       x = "Juvenile migration water year type", 
       y = "Cohort Replacement Rate (CRR)",
       fill = "Water Year Type") +
  theme_bw()
ggsave("Spring-run CRR WYT.tiff", device = "tiff", width = 7, height = 6, units = "in")

# fall-run
ggplot(FR_CRR, aes(x=WYT, y=CRR, fill=WYT)) +
  ylim(-0.5, (max(FR_CRR$CRR)+2)) + # was (0, 15)
  geom_boxplot() +
  geom_signif( # add the significance markers
    comparisons = list(c("C","W")),
    y_position = c(max(FR_CRR$CRR)+1),
    annotation = "pairwise comparisons NS", 
    tip_length = 0,
    map_signif_level = TRUE,
    textsize = 8
  ) +
  scale_fill_manual(values = pal_yrtype2, labels=c(
    'Critical','Dry','Below Normal','Above Normal','Wet')) +
  labs(title = "Fall-run Cohort Replacement Rate 1974-2021", 
       x = "Juvenile migration water year type", 
       y = "Cohort Replacement Rate (CRR)",
       fill = "Water Year Type") +
  theme_bw()
ggsave("Fall-run CRR WYT.tiff", device = "tiff", width = 7, height = 5, units = "in")

########################
# CRR boxplots
# pairwise comparisons #
# panel plot
########################
library(gridExtra)

# fix the individual plots how I want them
# winter-run
winterCRR <- ggplot(WR_CRR, aes(x=WYT, y=CRR, fill=WYT)) +
  ylim(-0.5, (max(WR_CRR$CRR)+2)) + # was (0, 15)
  geom_boxplot(show.legend = FALSE) +
  geom_signif( # add the significance markers
    comparisons = list(c("C","W")),
    y_position = c(6.5),
    map_signif_level = TRUE,
    textsize = 5
  ) +
  scale_fill_manual(values = pal_yrtype2, labels=c(
    'Critical','Dry','Below Normal','Above Normal','Wet')) +
  labs(title = "Winter-run", 
       x = "Juvenile migration water year type", 
       y = "CRR",
       fill = "Water Year Type") +
  theme_bw()

# spring-run
springCRR <- ggplot(SR_CRR, aes(x=WYT, y=CRR, fill=WYT)) +
  ylim(-0.5, (max(SR_CRR$CRR)+6)) + # was (0, 15)
  geom_boxplot(show.legend = FALSE) +
  geom_signif( # add the significance markers
    comparisons = list(c("C","W"),c("D","W"),c("C", "AN"), c("D", "AN"), c("C", "BN")),
    y_position = c(10.7, 9.4, 8.1, 6.8,5.5),
    map_signif_level = TRUE,
    textsize = 5
  ) +
  scale_fill_manual(values = pal_yrtype2, labels=c(
    'Critical','Dry','Below Normal','Above Normal','Wet')) +
  labs(title = "Spring-run", 
       x = "Juvenile migration water year type", 
       y = "CRR",
       fill = "Water Year Type") +
  theme_bw()

# fall-run
fallCRR <- ggplot(FR_CRR, aes(x=WYT, y=CRR, fill=WYT)) +
  ylim(-0.5, (max(FR_CRR$CRR)+2)) + # was (0, 15)
  geom_boxplot(show.legend = FALSE) +
  geom_signif( # add the significance markers
    comparisons = list(c("C","W")),
    y_position = c(max(FR_CRR$CRR)+1),
    annotation = "pairwise comparisons NS", 
    tip_length = 0,
    map_signif_level = TRUE,
    textsize = 4
  ) +
  scale_fill_manual(values = pal_yrtype2, labels=c(
    'Critical','Dry','Below Normal','Above Normal','Wet')) +
  labs(title = "Fall-run", 
       x = "Juvenile migration water year type", 
       y = "CRR",
       fill = "Water Year Type") +
  theme_bw()

# Create user-defined function, which extracts legends from ggplots
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}
# winter-run WITH LEGEND
winterCRR_legend <- ggplot(WR_CRR, aes(x=WYT, y=CRR, fill=WYT)) +
  ylim(-0.5, (max(WR_CRR$CRR)+2)) + # was (0, 15)
  geom_boxplot() +
  geom_signif( # add the significance markers
    comparisons = list(c("C","W")),
    y_position = c(7),
    map_signif_level = TRUE,
    textsize = 4
  ) +
  scale_fill_manual(values = pal_yrtype2, labels=c(
    'Critical','Dry','Below Normal','Above Normal','Wet')) +
  labs(title = "Winter-run", 
       x = "Juvenile migration water year type", 
       y = "CRR",
       fill = "Water Year Type") +
  theme_bw()
# Apply user-defined function to extract legend
shared_legend <- extract_legend(winterCRR_legend)

# Draw plots with shared legend
library(grid)
plot <- grid.arrange(arrangeGrob(winterCRR, springCRR, fallCRR,shared_legend, ncol = 2), top=textGrob("Chinook Salmon Cohort Replacement Rate (CRR) 1974-2021",gp=gpar(fontsize=20,font=1)))
plot
ggsave("CRR.tiff", plot, device = "tiff", width = 8, height = 6.5, units = "in")



##############################
# SacPAS migration duration #
##############################

# read data - multiple files
# create list of files in folder
# outdated #file_list <- list.files(path = "C:/Users/elizabeth.keller/Documents/GitHub/DJFMP-analyses/SacPAS")
file_list <- list.files(path = "C:/Users/elizabeth.keller/Documents/GitHub/DJFMP-analyses/SacPAS 20221117")

# read in all files in the folder and combine
for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (file==file_list[1]){
    dataset <- read.csv(here(paste("./SacPAS 20221117/",file,sep="")), header=TRUE, stringsAsFactors = F)
    dataset <- dataset[-(1:3),]
    dataset$filename <- file # add filename as a column
  }
  
  # if the merged dataset does exist, append to it
  if (file!=file_list[1]){
    temp_dataset <-read.csv(here(paste("./SacPAS 20221117/",file,sep="")), header=TRUE, stringsAsFactors = F)
    temp_dataset <- temp_dataset[-(1:3),]
    temp_dataset$filename <- file
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
}

# add water year stats
# check brood year to water year math for each run
dataset$WY <- as.numeric(dataset$Brood.Year)+1
dataset <- left_join(dataset, WaterYr)

## fix some variable types
# should be number
dataset$Run.Size <- as.numeric(dataset$Run.Size)
# should be factors
dataset$`Sac_Yr-type` <- factor(dataset$`Sac_Yr-type`, 
                                      levels=c("C","D","BN","AN","W"), 
                                      ordered=TRUE)
dataset$`SJ_Yr-type` <- factor(dataset$`SJ_Yr-type`, 
                                     levels=c("C","D","BN","AN","W"), 
                                     ordered=TRUE)
#rename
colnames(dataset)[19] <- "Sac_Yr_type"
colnames(dataset)[21] <- "SJ_Yr_type"

# dates should be dates
dataset[,c("FirstPassageDate",      "X5.PassageDate",        "X10.PassageDate",      
      "X25.PassageDate",       "X50.PassageDate",       "X75.PassageDate",       "X90.PassageDate",      
      "X95.PassageDate",       "LastPassageDate")] <- 
  lapply(dataset[,c("FirstPassageDate",      "X5.PassageDate",        "X10.PassageDate",      
               "X25.PassageDate",       "X50.PassageDate",       "X75.PassageDate",       "X90.PassageDate",      
               "X95.PassageDate",       "LastPassageDate")],
                                function(x) as.Date(x, format="%m/%d/%Y"))

# split filename into location and species/run
df <- data.frame(dataset$filename)
test <- df %>% separate(dataset.filename, c("Location", "Species","other"), sep=" ")
dataset <- cbind(dataset,test)

#remove 2022 because no WYT assigned
dataset <- dataset[dataset$WY < 2022,]

# box plots of migration duration
# separate by species/run and location; also will be different by duration days/percentage
# steelhead has an annoying outlier, removing for now
ggplot(dataset[!dataset$Species=="steelhead"&dataset$Location=="Sherwood",], aes(x=Sac_Yr_type, y=DurationMiddle50.Days, fill=Sac_Yr_type)) + 
  geom_boxplot() +theme_bw() + scale_fill_manual(values=pal_yrtype2, labels=c(
    'Critical','Dry','Below Normal','Above Normal','Wet'))+
  labs(title = "Duration Middle 50% Days - Sherwood Harbor  (WY 1998 - 2021)", 
       x = "Sacramento water year type", 
       y = "Duration of middle 50% days")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, color="black")+
  facet_wrap(~Species)

ggplot(dataset[!dataset$Species=="steelhead"&dataset$Location=="Chipps",], aes(x=Sac_Yr_type, y=DurationMiddle50.Days, fill=Sac_Yr_type)) + 
  geom_boxplot() +theme_bw() + scale_fill_manual(values=pal_yrtype2, labels=c(
    'Critical','Dry','Below Normal','Above Normal','Wet'))+
  labs(title = "Duration Middle 50% Days - Chipps Island (WY 1998 - 2021)", 
       x = "Sacramento water year type", 
       y = "Duration of middle 50% days")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, color="black")+
  facet_wrap(~Species)


# amount of time in Delta/ time between Delta entry and exit

# reform dataset, so each row is a species/year to substract dates
# add species/year variable
dataset$SpeciesWY <- paste(dataset$Species,dataset$WY)
Sherwood <- dataset[dataset$Location=="Sherwood",c(1:10,14,17:21,23,25)]
Chipps <- dataset[dataset$Location=="Chipps",c(1:10,14,17:21,23,25)]

Delta <- inner_join(Sherwood, Chipps, by="SpeciesWY")
# create Delta duration variables
Delta$Duration50.Delta <- Delta$X50.PassageDate.y - Delta$X50.PassageDate.x


ggplot(Delta[Delta$Species.x=="winter-run",], aes(x=Sac_Yr_type.x, y=Duration50.Delta, fill=Sac_Yr_type.x)) + 
  geom_boxplot() +theme_bw() + scale_fill_manual(values=pal_yrtype2, labels=c(
    'Critical','Dry','Below Normal','Above Normal','Wet'))+
  labs(title = "Duration Middle 50 Days - Delta, winter-run", 
       x = "Sacramento water year type", 
       y = "Duration of middle 50 days")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, color="black")


# try grouped by species
# removing steelhead due to numerous years with negative value (50% catch at Chipps happened before 50% at Sherwood)
ggplot(Delta[!Delta$Species.x=="steelhead",], aes(x=Sac_Yr_type.x, y=Duration50.Delta, fill=Sac_Yr_type.x)) + 
  geom_boxplot() +theme_bw() + scale_fill_manual(values=pal_yrtype2, labels=c(
    'Critical','Dry','Below Normal','Above Normal','Wet'))+
  labs(title = "Migration Duration through the Delta (WY 1998-2021)", 
       x = "Sacramento water year type", 
       y = "Days between 50% passage at Sherwood and Chipps")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, color="black")+
  facet_wrap(~Species.x)

##############################################################################################
# not used


##############
# summarized #
#   by date  #
##############

# this data frame will have one row per date*location*species
# with a sum of count/catch across that date*location*species # confirm since editing

# create a date*location* species variable
Trawl2$DateSpeciesLocation <- paste(Trawl2$SampleDate,Trawl2$Location,
                                    Trawl2$Species)

# sum over species+date+location
Trawl_summary <- Trawl2 %>%
  group_by(DateSpeciesLocation) %>%
  dplyr::summarize(
    SampleDate = SampleDate,
    WY = WY,
    Location = Location,
    #StationCode = StationCode,
    MethodCode = MethodCode,
    Species = Species,
    wtr_day = wtr_day, 
    Drought = Drought,
    Yr_type = Yr_type,
    Catch_sum = sum(Count)
  ) 
#remove duplicate rows
Trawl_summary <-Trawl_summary[!duplicated(Trawl_summary),]

###############
# some new work 11/2/2023
##########

# Sherwood winter-run density ridges by year
# density ridges
Trawl2.expanded[Trawl2.expanded$Species %in% c("Winter-run Chinook salmon"#,"Spring-run Chinook salmon","Fall-run Chinook salmon"
),] %>%
  mutate(across(Location, factor, levels=c("Sherwood Harbor"#,"Chipps Island"
  ))) %>%
  
  ggplot(aes(x = wtr_day, y = as.factor(WY),color = DroughtYear, fill = DroughtYear)) + scale_fill_brewer(name="Drought Year", palette = "Dark2", labels=c("Wet years", "First/only dry year","Second dry year","Three or more dry years")) +
  scale_color_brewer(name="Drought Year", palette = "Dark2", labels=c("Wet years", "First/only dry year","Second dry year","Three or more dry years")) +
  geom_density_ridges(alpha=0,scale=0.95) +
  labs(title = "Sherwood Harbor WR (1988-2021)", 
       x = "Day of the water year", 
       y = "Water Year") + theme_bw() + xlim(15,345)
ggsave("Sherwood WR by WY.tiff", device = "tiff", width = 6, height = 25, units = "in")


# Sherwood winter-run histograms by year
# migration timing histograms - WR, Sherwood
Trawl2.expanded[(Trawl2.expanded$Species %in% c("Winter-run Chinook salmon") & Trawl2.expanded$Location %in% c("Sherwood Harbor"))#,"Spring-run Chinook salmon","Fall-run Chinook salmon"
                ,] %>%
  mutate(across(Location, factor, levels=c("Sherwood Harbor"#,"Chipps Island"
  ))) %>%
  
  ggplot(aes(x = wtr_day, fill = DroughtYear)) +
  scale_fill_brewer(name="Drought Year", palette = "Dark2", labels=c("Wet years", "First/only dry year","Second dry year","Three or more dry years")) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 100)  +
  facet_grid(WY~.)

ggsave("Sherwood WR hist by WY.tiff", device = "tiff", width = 6, height = 25, units = "in")


# grouped boxplot

# Sherwood Harbor, multiple runs
Trawl2.expanded[(Trawl2.expanded$Species %in% c("Winter-run Chinook salmon","Spring-run Chinook salmon","Fall-run Chinook salmon") & Trawl2.expanded$Location %in% c("Sherwood Harbor")),] %>%
  mutate(across(Location, factor, levels=c("Sherwood Harbor"#,"Chipps Island"
  ))) %>%
  
  ggplot(aes(x=Species, y=wtr_day, fill=DroughtYear)) + 
  geom_boxplot()
# BUT this is lumping all years together (for years in the same drought group)... i.e., in the current plot a year with a lot of outmigrants could swamp out patterns from lower years
# really want to average across years in same group...

# let's do boxplots by year to see general variation...

# Sherwood Harbor, winter-run
Trawl2.expanded[(Trawl2.expanded$Species %in% c("Winter-run Chinook salmon"#,"Spring-run Chinook salmon","Fall-run Chinook salmon"
) & Trawl2.expanded$Location %in% c("Sherwood Harbor")),] %>%
  mutate(across(Location, factor, levels=c("Sherwood Harbor"#,"Chipps Island"
  ))) %>%
  
  ggplot(aes(x=as.factor(WY), y=wtr_day, fill=DroughtYear)) + 
  geom_boxplot()
