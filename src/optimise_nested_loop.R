# This script shows the original code which uses nested loops for join two
# data sets with using a join from the sqldf package
#


library(openxlsx)
library(sqldf)
library(diffdf)

source("common_functions.R")

test_result_file_name <- "../optimisation_data/optimise_nested_loop.csv"

input_folder <- "../output_data/"

optimisation_folder <- "../optimisation_data/"

LoadAndTimeNestedLoop <- function() {
  LDA <- ReadParquetData("../output_data/lncRNA-disease-associations-2697.parquet")
  LDExcl0 <- ReadParquetData("../output_data/lncRNA-disease-Excl0.parquet")

  ### set label for each sample, 1 for known LDA, 0 for unknown LDA (98880*1955)
  label.fr <- data.frame("label" = c(0))
  LDExcl0 <- cbind(label.fr, LDExcl0)

  start_time <- Sys.time()
  for (i in 1:nrow(LDExcl0))
  {
    for (j in 1:nrow(LDA))
    {
      if ((LDExcl0[i, 2] == LDA[j, 1]) && (LDExcl0[i, 3] == LDA[j, 2])) {
        LDExcl0[i, 1] <- 1
      }
    }
  }
  end_time <- Sys.time()

  duration <- as.numeric(difftime(end_time, start_time), units = "secs")

  return(list(time_taken = duration, data = LDExcl0))
}

LoadAndTimeSQLDF <- function() {
  LDA <- ReadParquetData("../output_data/lncRNA-disease-associations-2697.parquet")
  LDExcl0 <- ReadParquetData("../output_data/lncRNA-disease-Excl0.parquet")

  ### set label for each sample, 1 for known LDA, 0 for unknown LDA (98880*1955)
  label.fr <- data.frame("label" = c(0))
  LDExcl0 <- cbind(label.fr, LDExcl0)


  start_time <- Sys.time()
  LDExcl0 <- sqldf(
    c(
      '
        UPDATE LDExcl0 SET label = 1
        WHERE EXISTS (
          SELECT "x" FROM LDA
            WHERE LDExcl0.X1 = LDA.X1 AND LDExcl0.X2 = LDA.X2
        )',
      "SELECT * FROM main.LDExcl0"
    )
  )
  end_time <- Sys.time()

  duration <- as.numeric(difftime(end_time, start_time), units = "secs")
  return(list(time_taken = duration, data = LDExcl0))
}

result <- LoadAndTimeSQLDF()
sqldf_duration <- result$time_taken
sqldf_df <- result$data

result <- LoadAndTimeNestedLoop()
nested_loop_duration <- result$time_taken
nested_loop_df <- result$data


if (isTRUE(all.equal(sqldf_df, nested_loop_df)) == FALSE) {
  diff_file_name <- paste(optimisation_folder, "diff_result_nested_loop", ".txt", sep = "")

  diffdf(base = sqldf_df, compare = nested_loop_df, file = diff_file_name, suppress_warnings = TRUE)
} else {
  print("sqldf_df and nested_loop_df are the same")
}
