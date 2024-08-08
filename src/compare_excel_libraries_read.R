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

options(readxl.show_progress = FALSE)

test_result_file_name <- "../optimisation_data/excel_read_result.csv"

input_folder <- "../input_data/"

optimisation_folder <- "../optimisation_data/"

test_file_names <- c(
  ### L:lncRNA(240*1)
  "01-lncRNAs-240.xlsx",
  ### D:diseases(412*1)
  "02-diseases-412.xlsx",
  ### M:miRNA(495*1)
  "03-miRNAs-495.xlsx",
  ### LL:lncRNA-lncRNA functional similarities (240*240)
  "04-lncRNA-lncRNA.xlsx",
  ### LD:lncRNA-disease associations (240*412)
  "05-lncRNA-disease.xlsx",
  ### MD:miRNA-disease associations (495*412)
  "06-miRNA-disease.xlsx",
  ### DD:disease-disease semantic similarities (412*412)
  "07-disease-disease.xlsx",
  ### LM:lncRNA-miRNA interactions (240*495)
  "08-lncRNA-miRNA.xlsx"
)

LoadAndTimeReadXL <- function(file_name) {
  full_file_name <- paste(input_folder, file_name, sep = "")

  start_time <- Sys.time()
  df <- read_xlsx(full_file_name, sheet = 1, col_names = FALSE)
  end_time <- Sys.time()

  duration <- as.numeric(difftime(end_time, start_time), units = "secs")

  df <- data.frame(df)

  return(list(time_taken = duration, data = df))
}


LoadAndTimeOpenXLSX <- function(file_name) {
  full_file_name <- paste(input_folder, file_name, sep = "")

  start_time <- Sys.time()
  df <- read.xlsx(full_file_name, sheet = 1, colNames = FALSE)
  end_time <- Sys.time()

  duration <- as.numeric(difftime(end_time, start_time), units = "secs")

  df <- data.frame(df)

  return(list(time_taken = duration, data = df))
}

result <- lapply(test_file_names, LoadAndTimeReadXL)
readxl_times <- unlist(lapply(result, function(x) x$time_taken))
readxl_dfs <- lapply(result, function(x) x$data)
readxl_nrows <- unlist(sapply(readxl_dfs, nrow, USE.NAMES = FALSE))
readxl_ncols <- unlist(sapply(readxl_dfs, ncol, USE.NAMES = FALSE))
readxl_dfsize <- unlist(sapply(readxl_dfs, object.size, USE.NAMES = FALSE))

result <- lapply(test_file_names, LoadAndTimeOpenXLSX)
openxlsx_times <- unlist(lapply(result, function(x) x$time_taken))
openxlsx_dfs <- lapply(result, function(x) x$data)
openxlsx_nrows <- unlist(sapply(openxlsx_dfs, nrow, USE.NAMES = FALSE))
openxlsx_ncols <- unlist(sapply(openxlsx_dfs, ncol, USE.NAMES = FALSE))
openxlsx_dfsize <- unlist(sapply(openxlsx_dfs, object.size, USE.NAMES = FALSE))

is_data_equal <- c()

for (i in 1:length(test_file_names)) {
  readxl_df <- data.frame(readxl_dfs[i])
  openxlsx_df <- data.frame(openxlsx_dfs[i])

  # make the column names the same
  colnames(readxl_df) <- colnames(openxlsx_df)

  if (isTRUE(all.equal(readxl_df, openxlsx_df)) == FALSE) {
    diff_file_name <- paste(optimisation_folder, "diff_result_", test_file_names[i], ".txt", sep = "")

    diffdf(base = readxl_df, compare = openxlsx_df, file = diff_file_name, suppress_warnings = TRUE)

    is_data_equal <- append(is_data_equal, FALSE)
  } else {
    is_data_equal <- append(is_data_equal, TRUE)
  }
}

test_result_df <- data.frame(
  test_file_names,
  is_data_equal,
  readxl_times,
  openxlsx_times,
  readxl_nrows,
  openxlsx_nrows,
  readxl_ncols,
  openxlsx_ncols,
  readxl_dfsize,
  openxlsx_dfsize
)

colnames(test_result_df) <- c(
  "file_name",
  "is_data_equal",
  "readxl_duration_s",
  "openxlsx_duration_s",
  "readxl_nrows",
  "openxlsx_nrows",
  "readxl_ncols",
  "openxlsx_ncols",
  "readxl_dfsize_b",
  "openxlsx_dfsize_b"
)

write.csv(test_result_df, test_result_file_name, row.names = FALSE)
