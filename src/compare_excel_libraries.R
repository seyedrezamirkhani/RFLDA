# This script compares use of
# openxlsx and readxl libraries
# The results show that
# 1. openxlsx is the faster library
# 2. readxl trims strings in cells when it reads them
# This results in the difference in data on row 57 of 02-diseases-412.xlsx
# The true value is ' bone remodeling disease' with a leading space char 
# readxl trims the value showing 'bone remodeling disease'
# where as openxlsx does not
#
# openxlsx is both faster and more reliable based on these tests
#



library(openxlsx)
library(readxl)
library(dplyr)
library(diffdf)

test_file_names <- c(
  ### L:lncRNA(240*1)
  "../input_data/01-lncRNAs-240.xlsx",
  ### D:diseases(412*1)
  "../input_data/02-diseases-412.xlsx",
  ### M:miRNA(495*1)
  "../input_data/03-miRNAs-495.xlsx",
  ### LL:lncRNA-lncRNA functional similarities (240*240)
  "../input_data/04-lncRNA-lncRNA.xlsx",
  ### LD:lncRNA-disease associations (240*412)
  "../input_data/05-lncRNA-disease.xlsx",
  ### MD:miRNA-disease associations (495*412)
  "../input_data/06-miRNA-disease.xlsx",
  ### DD:disease-disease semantic similarities (412*412)
  "../input_data/07-disease-disease.xlsx",
  ### LM:lncRNA-miRNA interactions (240*495)
  "../input_data/08-lncRNA-miRNA.xlsx"
)

readxl_times <- c()
readxl_dfs <- c()

for (f in test_file_names) {
  start_time <- Sys.time()
  df <- read_xlsx(f, sheet = 1, col_names = FALSE)
  end_time <- Sys.time()

  readxl_dfs <- append(readxl_dfs, df)

  duration <- as.numeric(difftime(end_time, start_time), units = "secs")
  readxl_times <- append(readxl_times, duration)
}


openxlsx_times <- c()
openxlsx_dfs <- c()

for (f in test_file_names) {
  start_time <- Sys.time()
  df <- read.xlsx(f, sheet = 1, colNames = FALSE)
  end_time <- Sys.time()
  
  openxlsx_dfs <- append(openxlsx_dfs, df)
  
  duration <- as.numeric(difftime(end_time, start_time), units = "secs")
  openxlsx_times <- append(openxlsx_times, duration)
}

read_speed_df <- data.frame(test_file_names, readxl_times, openxlsx_times)

for (i in 1:length(test_file_names)) {
  readxl_df <- data.frame(readxl_dfs[i])
  openxlsx_df <- data.frame(openxlsx_dfs[i])
  
  # make the column names the same
  colnames(readxl_df) <- colnames(openxlsx_df)
  
  #print(all_equal(readxl_df, openxlsx_df))
  
  if (isTRUE(all.equal(readxl_df, openxlsx_df)) == FALSE) {
    print("NOT EQUAL")  
    print(c("index", i, "filename", test_file_names[i]))
    print(diffdf(readxl_df, openxlsx_df))
  }
}




