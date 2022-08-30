# packages
library(readxl)
library(lubridate)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyverse)

########################
### CDEC Water Years ###
########################
WaterYr <- read_excel("C:/Users/Elizabeth.keller/Desktop/DCP/Drought data/CDEC Water Year Hydrologic Classification Indices.xlsx", 
                      sheet = "Sheet3")

###################################################
# download fish data using EDI portal data.R file #
###################################################

