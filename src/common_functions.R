# This file contains common functions used for testing performance of
# openxlsx, writexl and arrow libraries


library(readxl)
library(openxlsx)
library(writexl)
library(arrow, warn.conflicts = FALSE)

ReadExcelData <- function(full_file_name) {
  df <- read.xlsx(full_file_name, sheet = 1, colNames = FALSE)

  return(df)
}

LoadAndTimeReadXL <- function(full_file_name) {
  start_time <- Sys.time()
  df <- read_xlsx(full_file_name, sheet = 1, col_names = FALSE)
  end_time <- Sys.time()

  duration <- as.numeric(difftime(end_time, start_time), units = "secs")

  df <- data.frame(df)

  return(list(time_taken = duration, data = df))
}


LoadAndTimeOpenXLSX <- function(full_file_name) {
  start_time <- Sys.time()
  df <- read.xlsx(full_file_name, sheet = 1, colNames = FALSE)
  end_time <- Sys.time()

  duration <- as.numeric(difftime(end_time, start_time), units = "secs")

  df <- data.frame(df)

  return(list(time_taken = duration, data = df))
}

ReadParquetData <- function(full_file_name) {
  df <- read_parquet(full_file_name)

  return(df)
}

WriteAndTimeOpenXLSX <- function(df) {
  full_file_name <- tempfile(pattern = "file", fileext = ".xslt")

  start_time <- Sys.time()
  df <- write.xlsx(df, file = full_file_name, colNames = FALSE)
  end_time <- Sys.time()

  duration <- as.numeric(difftime(end_time, start_time), units = "secs")

  return(duration)
}

WriteAndTimeWriteXL <- function(df) {
  full_file_name <- tempfile(pattern = "file", fileext = ".xslt")

  start_time <- Sys.time()
  df <- write_xlsx(df, full_file_name, format_headers = FALSE, use_zip64 = TRUE)
  end_time <- Sys.time()

  duration <- as.numeric(difftime(end_time, start_time), units = "secs")

  return(duration)
}

WriteAndTimeArrow <- function(df) {
  full_file_name <- tempfile(pattern = "file", fileext = ".parquet")

  start_time <- Sys.time()
  write_parquet(df, full_file_name)
  end_time <- Sys.time()

  duration <- as.numeric(difftime(end_time, start_time), units = "secs")

  return(duration)
}
