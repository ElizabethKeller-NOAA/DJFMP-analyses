library(here)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
## ######################
# regression or ANOVAs? #
#########################

## ANOVA of migration timing...?
# by drought year type, by run
# compare 50% migration - median

# create dataset with stats for each year
# for each run (and location): day of 50% migration, duration of migration, duration of 50% of migration?
# all in SacPAS?
# SacPAS folder!

# read data - multiple files
# create list of files in folder
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

## fix some variable types
# should be number
dataset$Run.Size <- as.numeric(dataset$Run.Size)

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

# add water year stats - pull WY from passage dates
# Add Water Year
dataset$WY <- year(dataset$X50.PassageDate) + as.numeric(between(month(dataset$X50.PassageDate),10,12))
# consider: addWaterYear() in the dataRetrieval package

# add day of water year
# use difftime to get wtr_day to be the number of days from 9-30 
dataset <- dataset %>%
  group_by(WY) %>% 
  mutate(X50_wtr_day = (as.integer(difftime(
    X50.PassageDate,ymd(paste0(WY - 1 ,'-09-30')), units = "days"))))
# error because one line has no dates (Sherwood steelhead in brood year 2016)

# Add water year type & drought period year type
#DroughtYr <- read.csv("yearassignments.csv") # in Drought salmonids.R
#colnames(DroughtYr)[1] <- "WY"
dataset <- left_join(dataset,DroughtYr[,c(1:5)], by="WY")

#remove 2022
dataset <- dataset[dataset$WY < 2022,]

# boxplots to visualize

# 50% passage
# by run and location
dataset[(dataset$Species %in% c("winter-run","spring-run","fall-run"
) & dataset$Location %in% c("Sherwood","Chipps")),] %>%
  mutate(across(Location, factor, levels=c("Sherwood","Chipps"
  ))) %>%
  
  ggplot(aes(x=Species, y=X50_wtr_day, fill=DroughtYear)) + 
  geom_boxplot() + facet_wrap(~Location)



# migration duration - 50%
dataset[(dataset$Species %in% c("winter-run","spring-run","fall-run"
) & dataset$Location %in% c("Sherwood","Chipps")),] %>%
  mutate(across(Location, factor, levels=c("Sherwood","Chipps"
  ))) %>%
  
  ggplot(aes(x=Species, y=DurationMiddle50.Days, fill=DroughtYear)) + 
  geom_boxplot() + facet_wrap(~Location)

# remove steelhead
dataset = dataset[dataset$Species!="steelhead",]

# two-factor ANOVAs for each timing and duration? (by run and drought) or separate ANOVAs by drought year for each run?

# variables of interest: X50_wtr_day and DurationMiddle50.Days
# by ; Species and Location and DroughtYear **would be 3-way then...

library(tidyverse)
install.packages("ggpubr")
library(ggpubr)
library(rstatix)

# summary stats - day of 50% passage
dataset %>%
  group_by(Species, Location, DroughtYear) %>%
  get_summary_stats(X50_wtr_day, type = "mean_sd")

# summary stats - duration middle 50% passage
dataset %>%
  group_by(Species, Location, DroughtYear) %>%
  get_summary_stats(DurationMiddle50.Days, type = "mean_sd")


# assumptions

# 1) Independence of the observations. Each subject should belong to only one group. There is no relationship between the observations in each group. Having repeated measures for the same participants is not allowed.

# 2) No significant outliers in any cell of the design
outlier1 <- dataset %>%
  group_by(Species, Location, DroughtYear) %>%
  identify_outliers(X50_wtr_day)
# Chipps winter-run 3+ 2015 April 4
# Sherwood winter-run 3+ 2014 December 8

outlier2 <- dataset %>%
  group_by(Species, Location, DroughtYear) %>%
  identify_outliers(DurationMiddle50.Days)
# Sherwood spring-run 0 2005 80
# Sherwood spring-run 3+ 2014 116

# 2 extreme outliers in each set
# You can include the outlier in the analysis anyway if you do not believe the result will be substantially affected. This can be evaluated by comparing the result of the ANOVA test with and without the outlier.
# It’s also possible to keep the outliers in the data and perform robust ANOVA test using the WRS2 package.



# 3) Normality. the data for each design cell should be approximately normally distributed.

model1  <- lm(X50_wtr_day ~ Species*Location*DroughtYear, data = dataset)
# Create a QQ plot of residuals
ggqqplot(residuals(model1)) # **issue with this package loading... :(
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model1))

model2 <- lm(DurationMiddle50.Days ~ Species*Location*DroughtYear, data = dataset)
# Create a QQ plot of residuals
ggqqplot(residuals(model2))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model2))

# ** NEITHER LOOK NORMAL

# Check normality assumption by groups. Computing Shapiro-Wilk test for each combinations of factor levels.

dataset %>%
  group_by(Species, Location, DroughtYear) %>%
  shapiro_test(X50_wtr_day)

# Create QQ plot for each cell of design:

ggqqplot(dataset, "X50_wtr_day", ggtheme = theme_bw()) +
  facet_grid(Species + Location ~ DroughtYear, labeller = "label_both")

# errors after this point **

dataset %>%
  group_by(Species, Location, DroughtYear) %>%
  shapiro_test(DurationMiddle50.Days)
ggqqplot(dataset, "DurationMiddle50.Days", ggtheme = theme_bw()) +
  facet_grid(Species + Location ~ DroughtYear, labeller = "label_both")

# 4) Homogeneity of variances. The variance of the outcome variable should be equal in every cell of the design.

# Levene’s test:

dataset %>% levene_test(X50_wtr_day ~ Species*Location*DroughtYear)

dataset %>% levene_test(DurationMiddle50.Days ~ Species*Location*DroughtYear)

# ** Note that, if the above assumptions are not met there are a non-parametric alternative (Kruskal-Wallis test) to the one-way ANOVA.
# Unfortunately, there are no non-parametric alternatives to the two-way and the three-way ANOVA. 
# It’s also possible to perform robust ANOVA test using the WRS2 R package.



# consider phenomix package later if have time
