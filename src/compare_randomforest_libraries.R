# This script compares use of
# RandomForest and Ranger libraries
# TODO: Write Summary and Result
#
#

library(doParallel)
library(ranger)
library(randomForest)

source("common_functions.R")

test_result_file_name <- "../optimisation_data/randomforest_comparison_result.csv"

input_folder <- "../input_data/"

optimisation_folder <- "../optimisation_data/"


############# training RandomForest model with top 50, 100, ..., 1950 features ###############################
### read training sample set consisting of 5394 lncRNA-disease pairs (5394*(3+1952))
training_sample_df <- ReadParquetData(
  full_file_name = "../output_data/TrainingSample.parquet"
)

### read variable importance score of each feature
feature_score_df <- ReadParquetData(
  full_file_name = "../output_data/FeatureScore.parquet"
)

GetTestData <- function(no_features) {
  ### construct training sample subset consisted X1(label) + top no_features
  top_features <- feature_score_df[1:no_features, 1]
  B1 <- subset(training_sample_df[, ], select = top_features)
  B3 <- subset(training_sample_df[, ], select = X1)
  B4 <- cbind(B3, B1)

  # SRM: Convert X1, the label to a factor which makes the RandomForest classification rather than regression model
  # https://stackoverflow.com/questions/56943991/random-forest-error-message-response-has-five-or-fewer-unique-values
  B4$X1 <- as.factor(B4$X1)

  ### random resampling in 10-fold crossing validation
  ind <- sample(
    10,
    nrow(B4),
    replace = TRUE,
    prob = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
  )
  return(list(data = B4, index = ind))
}

TrainAndTimeRandomForest <- function(no_features) {
  result <- GetTestData(no_features)

  data <- result$data
  index <- result$index

  # We use this to exclude 10% of the data during training
  i <- 1

  ### train Random Forest model
  start_time <- Sys.time()

  rf <- randomForest(
    X1 ~ .,
    data = data[index != i, ],
    mtry = floor(no_features / 3),
    importance = TRUE,
    ntree = 500,
    na.action = na.omit
  )

  end_time <- Sys.time()

  duration <- as.numeric(difftime(end_time, start_time), units = "secs")

  return(duration)
}

TrainAndTimeRanger <- function(no_features) {
  result <- GetTestData(no_features)

  data <- result$data
  index <- result$index

  # We use this to exclude 10% of the data during training
  i <- 1

  # Use one less core than available
  ncores_to_use <- detectCores() - 1

  ### train Random Forest model
  start_time <- Sys.time()

  rf <- ranger(
    X1 ~ .,
    data = data[index != i, ],
    mtry = floor(no_features / 3),
    importance = "impurity",
    # Use 'impurity' or 'permutation' for ranger importance
    num.trees = 500,
    num.threads = ncores_to_use # Use one less core than available
  )

  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time), units = "secs")

  return(duration)
}

# feature_range <- seq(from = 50, to = 1950, by = 50)
# reduce the number of models that are created/tested by reducing the groups of features we test with
feature_range <- seq(from = 50, to = 500, by = 50)

randomforest_durations <- sapply(feature_range, TrainAndTimeRandomForest, USE.NAMES = FALSE)
ranger_durations <- sapply(feature_range, TrainAndTimeRanger, USE.NAMES = FALSE)


test_result_df <- data.frame(
  feature_range,
  randomforest_durations,
  ranger_durations
)

colnames(test_result_df) <- c(
  "no_features",
  "randomforest_duration_s",
  "ranger_duration_s"
)

write.csv(test_result_df, test_result_file_name, row.names = FALSE)
