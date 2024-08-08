# This script compares use of
# openxlsx and writexl libraries for saving data in the xlsx format
# The results show that writexl is the faster library
#
#


library(openxlsx)
library(writexl)
library(dplyr)
library(diffdf)

source("common_functions.R")

test_result_file_name <- "../optimisation_data/excel_write_result.csv"

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

full_test_file_names <- unlist(lapply(test_file_names, function(x) paste(input_folder, x, sep = "")))

dfs <- sapply(full_test_file_names, ReadExcelData)
nrows <- unlist(sapply(dfs, nrow, USE.NAMES = FALSE))
ncols <- unlist(sapply(dfs, ncol, USE.NAMES = FALSE))
dfsize <- unlist(sapply(dfs, object.size, USE.NAMES = FALSE))

openxlsx_times <- sapply(dfs, WriteAndTimeOpenXLSX, USE.NAMES = FALSE)
writexl_times <- sapply(dfs, WriteAndTimeWriteXL, USE.NAMES = FALSE)

test_result_df <- data.frame(
  test_file_names,
  writexl_times,
  openxlsx_times,
  nrows,
  ncols,
  dfsize
)

colnames(test_result_df) <- c(
  "file_name",
  "writexl_duration_s",
  "openxlsx_duration_s",
  "nrows",
  "ncols",
  "dfsize_b"
)

# sort data frame by size
test_result_df <- test_result_df[order(test_result_df$dfsize_b), ]

write.csv(test_result_df, test_result_file_name, row.names = FALSE)
