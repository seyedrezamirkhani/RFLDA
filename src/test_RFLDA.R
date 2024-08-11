# Executes functions of RFLDA in sequence and records associated execution time

source("RFLDA.R")

test_result_file_name <- "../optimisation_data/RFLDA_result.csv"

optimisation_folder <- "../optimisation_data/"

ExecuteAndTimeRFLDAFunction <- function(func) {
  file_format <- "parquet"

  start_time <- Sys.time()
  func(file_format)
  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time), units = "secs")
  return(duration)
}

test_functions <- list(
  list(name = "PrepareData", func = PrepareData),
  list(name = "ComputeFeatureImportance", func = ComputeFeatureImportance),
  list(name = "ComputeClassficationAccuracy", func = ComputeClassficationAccuracy),
  list(name = "FiveFoldCrossingValidation", func = FiveFoldCrossingValidation),
  list(name = "Predict", func = Predict)
)

durations <- c()

for (f in test_functions) {
  print(paste("processing", f$name))
  duration <- ExecuteAndTimeRFLDAFunction(f$func)
  print(paste("duration for", f$name, "is", duration))

  durations <- append(durations, duration)
}

function_names <- unlist(lapply(test_functions, function(x) x$name))

test_result_df <- data.frame(
  function_names,
  durations
)

colnames(test_result_df) <- c(
  "function_name",
  "duration_s"
)

write.csv(test_result_df, test_result_file_name, row.names = FALSE)
