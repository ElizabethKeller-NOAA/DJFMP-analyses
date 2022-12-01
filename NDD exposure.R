# how much of each run occurs in each NDD level

# run DJFMP data and DCP example year data files first

#####################
# data manipulation #
#####################

# Add Water Year
dt2$WY <- year(dt2$SampleDate) + 
  as.numeric(between(month(dt2$SampleDate),10,12))

# clip dt2 to station we want
# Sherwood Harbor
SH_Trawl <- dt2[dt2$StationCode=="SR055M",]

# Species = combine CommonName + RaceByLength to get species/run
SH_Trawl$Species <- as.character(SH_Trawl$CommonName)
SH_Trawl$Species[SH_Trawl$CommonName=="Chinook salmon"] <- paste(
  SH_Trawl$RaceByLength[SH_Trawl$CommonName=="Chinook salmon"],"-run ",
  "Chinook salmon",sep="")

# Date column for join
SH_Trawl$Date <- SH_Trawl$SampleDate
# then join with the DCP data to get the NDD & NDD level at each date
NDD <- left_join(SH_Trawl,DCP)

# only need Chinook salmon
# can remove lots of the unnecessary variables from the trawl data
NDD <- NDD[NDD$OrganismCode=="CHN",c("Location","StationCode","SampleDate", 
                                     "SampleTime","MethodCode","GearConditionCode",
                                     "WeatherCode",  
  "DO",                  "WaterTemp",           "Turbidity",           "Secchi",             
  "SpecificConductance","OrganismCode",        "IEPFishCode",        
  "CommonName",          "MarkCode",            "StageCode",           "Expression",         
  "ForkLength",          "RaceByLength",        "TagCode",             "RaceByTag",          
  "ArchivalID",          "SpecialStudyID",      "GeneticID",           "Probability1",       
  "GeneticID2",          "Probability2",        "SexGeneID",           "Ots28",              
  "Lab",                 "GeneticTest",         "GeneticModel",        "Count",              
  "WY",                  "Species",             "Date",                "Year",               
  "Sac Flow",            "Wilkins Slough",      "NDD",                 "Bypass Flow",        
  "NDD Level")]

# remove rows where there were no NDD values (only have select example years for DCP)
# remove adult and NA-run Chinook
NDD <- NDD[!is.na(NDD$NDD) & NDD$Species %in% c("Winter-run Chinook salmon","LateFall-run Chinook salmon",
                                                "Fall-run Chinook salmon","Spring-run Chinook salmon"),]

# want to have sums by year, run, NDD level
# table per run --> loop?
# each table should be year by NDD level?

NDD_sums <- NDD %>%
  group_by(Species,WY,`NDD Level`) %>%
  dplyr::summarize(
    WY = WY,
    Sum = sum(Count)
  ) 
#remove duplicate rows
NDD_sums <- NDD_sums[!duplicated(NDD_sums),]

# get annual sums too
NDD_sums2 <- NDD_sums %>%
  group_by(Species,WY) %>%
  dplyr::summarize(
    WY = WY,
    NDD_Level = `NDD Level`,
    Sum = Sum,
    Annual_Sum = sum(Sum)
  ) 

NDD_sums2$NDD_percent <- round((NDD_sums2$Sum / NDD_sums2$Annual_Sum *100),digit=2)

write.csv(NDD_sums2, "NDD_sums.csv")
## consider later how to best visualize or organize table

# OR histograms??
# one histogram per year, each run as a color
# catch on y, NDD Level on x
# need to weight by catch?

#ggplot(NDD, aes(x = `NDD Level`, y = ..density.., weight = Count, fill=Species,alpha=0.3)) + geom_histogram(binwidth=1)

#ggplot(NDD, aes(x = `NDD Level`, fill=Species,alpha=0.3)) + geom_histogram(binwidth=1)

#ggplot(NDD[NDD$Species=="Winter-run Chinook salmon",], aes(x = `NDD Level`, y = ..density.., weight = Count, fill=Species,alpha=0.3)) + geom_histogram(binwidth=1)


# want percentages instead?
# need to split these up by year


