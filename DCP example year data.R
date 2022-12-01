library(dplyr)
library(purrr)
library(readxl)
# DCP data
# read in of the DCP data all to one dataframe
path <- "C:/Users/Elizabeth.keller/Desktop/DCP/Migration timing data/DCP Conceptual Data_data only.xlsx"

DCP <- path %>%
  excel_sheets() %>%
  set_names() %>%
  map_df(read_excel,
         path = path,
         .id = "Year")
#str(DCP)