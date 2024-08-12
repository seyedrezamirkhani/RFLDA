# This script shows the original code which uses nested loops for join two
# data sets with using a join from the sqldf package
#


library(openxlsx)
library(sqldf)
library(diffdf)

source("common_functions.R")

test_result_file_name <- "../optimisation_data/optimise_nested_loop.csv"

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


  # Add a new numeric row_id column to aid setting the labels correctly
  row_id.fr <- data.frame("row_id" = seq_len(nrow(LDExcl0)))
  LDExcl0 <- cbind(row_id.fr, LDExcl0)

  LDExcl0_1 <- sqldf(
    "SELECT LDExcl0.row_id FROM LDExcl0, LDA WHERE LDExcl0.X1 = LDA.X1 AND LDExcl0.X2 = LDA.X2"
  )

  # update label to 1 for rows which match LDExcl0 and LDA on X1 and X2 columns
  LDExcl0[LDExcl0_1$row_id, "label"] <- rep(1, nrow(LDExcl0_1))

  # Now we can drop row_id; it was only needed to populate the label column correctly
  LDExcl0 <- subset(LDExcl0, select = -c(row_id))

  end_time <- Sys.time()

  duration <- as.numeric(difftime(end_time, start_time), units = "secs")
  return(list(time_taken = duration, data = LDExcl0))
}

result <- LoadAndTimeSQLDF()
sqldf_duration <- result$time_taken
sqldf_df <- result$data
sqldf_nrows <- nrow(sqldf_df)
sqldf_ncols <- ncol(sqldf_df)
sqldf_dfsize <- object.size(sqldf_df)


result <- LoadAndTimeNestedLoop()
nested_loop_duration <- result$time_taken
nested_loop_df <- result$data
nested_loop_nrows <- nrow(nested_loop_df)
nested_loop_ncols <- ncol(nested_loop_df)
nested_loop_dfsize <- object.size(nested_loop_df)

is_data_equal <- TRUE

if (isTRUE(all.equal(sqldf_df, nested_loop_df)) == FALSE) {
  is_data_equal <- FALSE

  diff_file_name <- paste(optimisation_folder, "diff_result_nested_loop", ".txt", sep = "")

  diffdf(base = sqldf_df, compare = nested_loop_df, file = diff_file_name, suppress_warnings = TRUE)
} else {
  print("sqldf_df and nested_loop_df are the same")
}

test_result_df <- data.frame(
  is_data_equal,
  nested_loop_duration,
  sqldf_duration,
  nested_loop_nrows,
  sqldf_nrows,
  nested_loop_ncols,
  sqldf_ncols,
  nested_loop_dfsize,
  sqldf_dfsize
)

colnames(test_result_df) <- c(
  "is_data_equal",
  "nested_loop_duration_s",
  "sqldf_duration_s",
  "nested_loop_nrows",
  "sqldf_nrows",
  "nested_loop_ncols",
  "sqldf_ncols",
  "nested_loop_dfsize",
  "sqldf_dfsize"
)

write.csv(test_result_df, test_result_file_name, row.names = FALSE)
