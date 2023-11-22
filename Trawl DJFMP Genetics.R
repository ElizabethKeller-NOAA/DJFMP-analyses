# read in data
############################
# CHIPPS ISLAND
# data webpage: https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1055.1
# code from: https://portal.edirepository.org/nis/codeGeneration?packageId=edi.1055.1&statisticalFileType=r

# Package ID: edi.1055.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Chipps Island trawl, Delta Juvenile Fish Monitoring Program, Genetic Determination of Population of Origin 2017-2021.
# Data set creator:  Elissa Buttermore - U.S. Bureau of Reclamation 
# Data set creator:  Joshua Israel - U.S. Bureau of Reclamation 
# Data set creator:  Kevin Reece - California Department of Water Resources 
# Data set creator:  Scott Blankenship - Cramer Fish Sciences - Genidaqs 
# Metadata Provider:  Scott Blankenship - Cramer Fish Sciences - Genidaqs 
# Contact:  Joshua Israel -  U.S. Bureau of Reclamation  - jaisrael@usbr.gov
# Contact:  Scott Blankenship -  Cramer Fish Sciences - Genidaqs  - scott.blankenship@fishsciences.net
# Contact:  Elissa Buttermore -  U.S. Bureau of Reclamation  - ebuttermore@usbr.gov
# Contact:  Kevin Reece -  California Department of Water Resources  - kevin.reece@water.ca.gov
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/1055/1/4a3b853edcf849ea4cbeb2b826885f0a" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "ID",     
                 "SampleDate",     
                 "ForkLength",     
                 "Julian",     
                 "GeneticID",     
                 "PosProb",     
                 "Ots28",     
                 "LengthByDate",     
                 "FieldID"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$ID)!="factor") dt1$ID<- as.factor(dt1$ID)                                   
# attempting to convert dt1$SampleDate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1SampleDate<-as.Date(dt1$SampleDate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1SampleDate) == length(tmp1SampleDate[!is.na(tmp1SampleDate)])){dt1$SampleDate <- tmp1SampleDate } else {print("Date conversion failed for dt1$SampleDate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1SampleDate) 
if (class(dt1$ForkLength)=="factor") dt1$ForkLength <-as.numeric(levels(dt1$ForkLength))[as.integer(dt1$ForkLength) ]               
if (class(dt1$ForkLength)=="character") dt1$ForkLength <-as.numeric(dt1$ForkLength)
if (class(dt1$Julian)=="factor") dt1$Julian <-as.numeric(levels(dt1$Julian))[as.integer(dt1$Julian) ]               
if (class(dt1$Julian)=="character") dt1$Julian <-as.numeric(dt1$Julian)
if (class(dt1$GeneticID)!="factor") dt1$GeneticID<- as.factor(dt1$GeneticID)
if (class(dt1$PosProb)=="factor") dt1$PosProb <-as.numeric(levels(dt1$PosProb))[as.integer(dt1$PosProb) ]               
if (class(dt1$PosProb)=="character") dt1$PosProb <-as.numeric(dt1$PosProb)
if (class(dt1$Ots28)!="factor") dt1$Ots28<- as.factor(dt1$Ots28)
if (class(dt1$LengthByDate)!="factor") dt1$LengthByDate<- as.factor(dt1$LengthByDate)
if (class(dt1$FieldID)!="factor") dt1$FieldID<- as.factor(dt1$FieldID)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(ID)
summary(SampleDate)
summary(ForkLength)
summary(Julian)
summary(GeneticID)
summary(PosProb)
summary(Ots28)
summary(LengthByDate)
summary(FieldID) 
# Get more details on character variables

summary(as.factor(dt1$ID)) 
summary(as.factor(dt1$GeneticID)) 
summary(as.factor(dt1$Ots28)) 
summary(as.factor(dt1$LengthByDate)) 
summary(as.factor(dt1$FieldID))
detach(dt1)               

Chipps_genetics <- dt1

# SACRAMENTO TRAWL (SHERWOOD HARBOR)

# data webpage: https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1054.1
# code from: https://portal.edirepository.org/nis/codeGeneration?packageId=edi.1054.1&statisticalFileType=r

# Package ID: edi.1054.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Sacramento trawl, Delta Juvenile Fish Monitoring Program, Genetic Determination of Population of Origin 2017-2021.
# Data set creator:  Elissa Buttermore - U.S. Bureau of Reclamation 
# Data set creator:  Joshua Israel - U.S. Bureau of Reclamation 
# Data set creator:  Kevin Reece - California Department of Water Resources 
# Data set creator:  Scott Blankenship - Cramer Fish Sciences - Genidaqs 
# Metadata Provider:  Scott Blankenship - Cramer Fish Sciences - Genidaqs 
# Contact:  Joshua Israel -  U.S. Bureau of Reclamation  - jaisrael@usbr.gov
# Contact:  Elissa Buttermore -  U.S. Bureau of Reclamation  - ebuttermore@usbr.gov
# Contact:  Scott Blankenship -  Cramer Fish Sciences - Genidaqs  - scott.blankenship@fishsciences.net
# Contact:  Kevin Reece -  California Department of Water Resources  - kevin.reece@water.ca.gov
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/1054/1/9c08ec12afde764744f360ca7c89a672" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "ID",     
                 "SampleDate",     
                 "ForkLength",     
                 "Julian",     
                 "GeneticID",     
                 "PosProb",     
                 "Ots28",     
                 "LengthByDate",     
                 "FieldID"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$ID)!="factor") dt1$ID<- as.factor(dt1$ID)                                   
# attempting to convert dt1$SampleDate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1SampleDate<-as.Date(dt1$SampleDate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1SampleDate) == length(tmp1SampleDate[!is.na(tmp1SampleDate)])){dt1$SampleDate <- tmp1SampleDate } else {print("Date conversion failed for dt1$SampleDate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1SampleDate) 
if (class(dt1$ForkLength)=="factor") dt1$ForkLength <-as.numeric(levels(dt1$ForkLength))[as.integer(dt1$ForkLength) ]               
if (class(dt1$ForkLength)=="character") dt1$ForkLength <-as.numeric(dt1$ForkLength)
if (class(dt1$Julian)=="factor") dt1$Julian <-as.numeric(levels(dt1$Julian))[as.integer(dt1$Julian) ]               
if (class(dt1$Julian)=="character") dt1$Julian <-as.numeric(dt1$Julian)
if (class(dt1$GeneticID)!="factor") dt1$GeneticID<- as.factor(dt1$GeneticID)
if (class(dt1$PosProb)=="factor") dt1$PosProb <-as.numeric(levels(dt1$PosProb))[as.integer(dt1$PosProb) ]               
if (class(dt1$PosProb)=="character") dt1$PosProb <-as.numeric(dt1$PosProb)
if (class(dt1$Ots28)!="factor") dt1$Ots28<- as.factor(dt1$Ots28)
if (class(dt1$LengthByDate)!="factor") dt1$LengthByDate<- as.factor(dt1$LengthByDate)
if (class(dt1$FieldID)!="factor") dt1$FieldID<- as.factor(dt1$FieldID)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(ID)
summary(SampleDate)
summary(ForkLength)
summary(Julian)
summary(GeneticID)
summary(PosProb)
summary(Ots28)
summary(LengthByDate)
summary(FieldID) 
# Get more details on character variables

summary(as.factor(dt1$ID)) 
summary(as.factor(dt1$GeneticID)) 
summary(as.factor(dt1$Ots28)) 
summary(as.factor(dt1$LengthByDate)) 
summary(as.factor(dt1$FieldID))
detach(dt1)               

Sacramento_genetics <- dt1
###########################

###########################
# combine and manipulate data
########################
# add Location
Sacramento_genetics$Location <- "Sherwood Harbor"
Chipps_genetics$Location <- "Chipps Island"

# merge datasets
Trawl_genetics <- rbind(Sacramento_genetics,Chipps_genetics)

# Add Water Year
Trawl_genetics$WY <- year(Trawl_genetics$SampleDate) + 
  as.numeric(between(month(Trawl_genetics$SampleDate),10,12))
# add day of water year
# use difftime to get wtr_day to be the number of days from 9-30 
Trawl_genetics <- Trawl_genetics %>%
  group_by(WY) %>% 
  mutate(wtr_day = (as.integer(difftime(
    SampleDate,ymd(paste0(WY - 1 ,'-09-30')), units = "days"))))
# Add water year type & drought period year type
Trawl_genetics <- left_join(Trawl_genetics,DroughtYr[,c(1:5)], by="WY")


# PLOTS
# plot genetic trawl data
# by location, by run
# years 2017 - 2021
# migration timing at Sherwood and Chipps
# density ridges
Trawl_genetics %>%
  
  ggplot(aes(x = wtr_day, y = GeneticID, 
             color = DroughtYear, 
             fill = DroughtYear)) + scale_fill_brewer(name="Drought Year", palette = "Dark2", labels=c("Wet years", "First/only dry year","Second dry year","Three or more dry years")) +
  scale_color_brewer(name="Drought Year", palette = "Dark2", labels=c("Wet years", "First/only dry year","Second dry year","Three or more dry years")) +
  geom_density_ridges(alpha = .3, scale=0.95) +
  labs(title = "[Genetic] Chinook Salmon Trawl Catch Timing (2017-2021)", 
       x = "Day of the water year", 
       y = "Run Type") + theme_bw() +
  #scale_y_discrete(labels = c("Fall-run","Spring-run","Winter-run")) + # labels bottom to top??
  facet_wrap(~Location)
#ggsave("Trawls DroughtYear timing.tiff", device = "tiff", width = 9, height = 6, units = "in")

# LAD from genetics dataset
Trawl_genetics %>%
  
  ggplot(aes(x = wtr_day, y = LengthByDate, 
             color = DroughtYear, 
             fill = DroughtYear)) + scale_fill_brewer(name="Drought Year", palette = "Dark2", labels=c("Wet years", "First/only dry year","Second dry year","Three or more dry years")) +
  scale_color_brewer(name="Drought Year", palette = "Dark2", labels=c("Wet years", "First/only dry year","Second dry year","Three or more dry years")) +
  geom_density_ridges(alpha = .3, scale=0.95) +
  labs(title = "[LAD] Chinook Salmon Trawl Catch Timing (2017-2021)", 
       x = "Day of the water year", 
       y = "Run Type") + theme_bw() +
  #scale_y_discrete(labels = c("Fall-run","Spring-run","Winter-run")) + # labels bottom to top??
  facet_wrap(~Location)
#ggsave("Trawls DroughtYear timing.tiff", device = "tiff", width = 9, height = 6, units = "in")


# plot same years from Drought salmonids.R
# (LAD trawl data)
# by location, by run
# years: 2017 - 2021
# migration timing at Sherwood and Chipps
# density ridges
Trawl2.expanded[Trawl2.expanded$Species %in% c("Winter-run Chinook salmon","Spring-run Chinook salmon","Fall-run Chinook salmon"),] %>%
  mutate(across(Location, factor, levels=c("Sherwood Harbor","Chipps Island"))) %>%
  
  ggplot(aes(x = wtr_day, y = Species, 
             color = DroughtYear, 
             fill = DroughtYear)) + scale_fill_brewer(name="Drought Year", palette = "Dark2", labels=c("Wet years", "First/only dry year","Second dry year","Three or more dry years")) +
  scale_color_brewer(name="Drought Year", palette = "Dark2", labels=c("Wet years", "First/only dry year","Second dry year","Three or more dry years")) +
  geom_density_ridges(alpha = .3, scale=0.95) +
  labs(title = "Chinook Salmon Trawl Catch Timing (2017-2021)", 
       x = "Day of the water year", 
       y = "Run Type") + theme_bw() +
  scale_y_discrete(labels = c("Fall-run","Spring-run","Winter-run")) + # labels bottom to top??
  facet_wrap(~Location)
#ggsave("Trawls DroughtYear timing.tiff", device = "tiff", width = 9, height = 6, units = "in")

# bubble plot comparing genetic vs LAD assignments
library(tidyr)
table <- table(Trawl_genetics$LengthByDate,Trawl_genetics$GeneticID)
table <- as.data.frame(table)
colnames(table)<- c("LAD","GeneticID","Freq")
ggplot(as.data.frame(table), aes(x=LAD, y=GeneticID,size=Freq)) +
  geom_point(alpha=0.7) + scale_size(range = c(1, 20))

# need split by location
Chipps_table <- table(Trawl_genetics[Trawl_genetics$Location=="Chipps Island",]$LengthByDate,Trawl_genetics[Trawl_genetics$Location=="Chipps Island",]$GeneticID)
Chipps_table <- as.data.frame(Chipps_table)
colnames(Chipps_table)<- c("LAD","GeneticID","Freq")
ggplot(Chipps_table, aes(x=LAD, y=GeneticID,size=Freq)) +
  geom_point(alpha=0.7) + scale_size(range = c(1, 20))

Sac_table <- table(Trawl_genetics[Trawl_genetics$Location=="Sherwood Harbor",]$LengthByDate,Trawl_genetics[Trawl_genetics$Location=="Sherwood Harbor",]$GeneticID)
Sac_table <- as.data.frame(Sac_table)
colnames(Sac_table)<- c("LAD","GeneticID","Freq")
ggplot(Sac_table, aes(x=LAD, y=GeneticID,size=Freq)) +
  geom_point(alpha=0.7) + scale_size(range = c(1, 20))
