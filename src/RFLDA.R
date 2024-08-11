# NOTE by Seyed Reza Mirkhani
# 1 - Original-00-Rcode-RFLDA.R Fails to write and read back the LDA object.
# This is resolved by converting LDA to a data.frame before saving it to disk.
# Perhaps the code may have worked with the openxlsx before and subsequent
#   changes to this package stopped supporting of writing the matrix to excel?
# 2 - The openxlsx is very slow at writing xlsx files. Additionally, the file
#   lncRNA-disease-ALL.xlsx cannot be opened with LibreOffice Calc. To resolve
#   this issue, writexl is used instead.
# 3 - The original code converted LDA into a matrix which is a bug as this
#   dataframe contains two columns of text. The code shown below is commented now
#   LDA <- data.matrix(LDA)
# 4 - Changed generation of labels for LDExcl0 to use sqldf instead of nested loops

########################################### 20191226 IRFMDA ###################################################
########################################### 20191226 load R packages ##########################################
library(openxlsx)
library(ROCR)
library(plyr)
library(writexl)
library(ranger)
library(doParallel)
library(sqldf)
library(arrow, warn.conflicts = FALSE)
##############################################################################################################

### SaveData
### writes dataframe to disk as either parquet or excel
### in the original code col_names are not saved when using excel
### to keep things consistent the parquet column_names are replaced
### with default columns assigned by openxlsx X1, ... Xn
SaveData <- function(df, file_name, file_format) {
  if (tolower(file_format) == "excel") {
    write_xlsx(
      df,
      paste(file_name, ".xlsx", sep = ""),
      col_names = FALSE,
      use_zip64 = TRUE
    )
  } else if (tolower(file_format) == "parquet") {
    # replace column_names with default names generated when openxlsx opens a
    # spreadsheet without headers
    generic_cols <- sprintf("X%d", seq(1:length(colnames(df))))
    colnames(df) <- generic_cols
    write_parquet(df, paste(file_name, ".parquet", sep = ""))
  } else {
    stop("Unknown file_format supplied")
  }
}

LoadData <- function(file_name, file_format) {
  if (tolower(file_format) == "excel") {
    df <- read.xlsx(paste(file_name, ".xlsx", sep = ""),
      sheet = 1,
      colNames = FALSE
    )
    return(df)
  } else if (tolower(file_format) == "parquet") {
    df <- read_parquet(paste(file_name, ".parquet", sep = ""))
    return(df)
  } else {
    stop("Unknown file_format supplied")
  }
}

########################################### 20191226 data preparation  #########################################
PrepareData <- function(file_format) {
  ### L:lncRNA(240*1)
  L <- read.xlsx("../input_data/01-lncRNAs-240.xlsx",
    sheet = 1,
    colNames = FALSE
  )
  ### D:diseases(412*1)
  D <- read.xlsx("../input_data/02-diseases-412.xlsx",
    sheet = 1,
    colNames = FALSE
  )
  ### M:miRNA(495*1)
  M <- read.xlsx("../input_data/03-miRNAs-495.xlsx",
    sheet = 1,
    colNames = FALSE
  )
  ### LL:lncRNA-lncRNA functional similarities (240*240)
  LL <- read.xlsx("../input_data/04-lncRNA-lncRNA.xlsx",
    sheet = 1,
    colNames = FALSE
  )
  ### LD:lncRNA-disease associations (240*412)
  LD <- read.xlsx("../input_data/05-lncRNA-disease.xlsx",
    sheet = 1,
    colNames = FALSE
  )
  ### MD:miRNA-disease associations (495*412)
  MD <- read.xlsx("../input_data/06-miRNA-disease.xlsx",
    sheet = 1,
    colNames = FALSE
  )
  ### DD:disease-disease semantic similarities (412*412)
  DD <- read.xlsx("../input_data/07-disease-disease.xlsx",
    sheet = 1,
    colNames = FALSE
  )
  ### LM:lncRNA-miRNA interactions (240*495)
  LM <- read.xlsx("../input_data/08-lncRNA-miRNA.xlsx",
    sheet = 1,
    colNames = FALSE
  )

  ##### consturcting sample dataset
  ### Represent lncRNA by 1147 features, L1147: (240*1147)
  L1147 <- cbind(LL, LM, LD)
  ### adding lncRNA name column for L1147
  L1148 <- cbind(L[, 1], L1147)
  ### Represent disease by 1147 features, D1147: (412*1147)
  ### DL: the transfer matrix of LD
  DL <- t(LD)
  ### DM: the transfer matrix of MD
  DM <- t(MD)
  ### D1147: (412*1147)
  D1147 <- cbind(DL, DM, DD)
  ### adding disease name column for D1147
  D1148 <- cbind(D[, 1], D1147)
  ### merge L1148 and D1148 to LDALL (98880*(2+1147+1147=2296))
  LDALL <- merge(x = L1148, y = D1148, by = NULL)

  ### Adjust column position of LDALL (98880*(2+1147+1147=2296))
  d1 <- subset(LDALL, select = 1)
  d2 <- subset(LDALL, select = 1149)
  d3 <- subset(LDALL, select = c(-1, -1149))
  LDALL <- data.frame(d1, d2, d3)
  SaveData(
    df = LDALL,
    file_name = "../output_data/lncRNA-disease-ALL",
    file_format = file_format
  )
  LDALL <- LoadData(
    file_name = "../output_data/lncRNA-disease-ALL", file_format =
      file_format
  )

  ### exclude columns with full zero values,(98880*(3+1952=1955))
  d1 <- subset(LDALL, select = c(1, 2))
  d2 <- subset(LDALL, select = c(-1, -2))
  d2 <- d2[, which(colSums(d2) > 0)]
  LDExcl0 <- cbind(d1, d2)
  SaveData(
    df = LDExcl0,
    file_name = "../output_data/lncRNA-disease-Excl0",
    file_format = file_format
  )
  LDExcl0 <- LoadData(
    file_name = "../output_data/lncRNA-disease-Excl0", file_format =
      file_format
  )

  ### construct known lncRNA-disease association pairs
  xy <- which(LD[, ] == 1, arr.ind = TRUE)
  LDA <- cbind(L[xy[, 1], 1], D[xy[, 2], 1])
  LDA <- data.frame(LDA)
  SaveData(
    df = LDA,
    file_name = "../output_data/lncRNA-disease-associations-2697",
    file_format = file_format
  )
  LDA <- LoadData(
    file_name = "../output_data/lncRNA-disease-associations-2697", file_format =
      file_format
  )

  # NOTE by SRM! The line below converts the LDA data which has two text columns
  #   into numeric values! It is a bug, hence it is now commented
  #   For reference, the first few lines of the LDA data frame are:
  #
  #   PCA3	disease of metabolism
  #   LINC00271	disease of metabolism
  #   HYMAI	disease of metabolism
  #   SNHG11	disease of metabolism
  #
  # LDA <- data.matrix(LDA)

  ### set label for each sample, 1 for known LDA, 0 for unknown LDA (98880*1955)
  label.fr <- data.frame("label" = c(0))
  LDExcl0 <- cbind(label.fr, LDExcl0)

  # Add a new numeric row_id column to aid setting the labels correctly
  row_id.fr <- data.frame("row_id" = seq_len(nrow(LDExcl0)))
  LDExcl0 <- cbind(row_id.fr, LDExcl0)

  # print('building LDExcl0_1')
  # print(Sys.time())
  LDExcl0_1 <- sqldf(
    "SELECT LDExcl0.row_id FROM LDExcl0, LDA WHERE LDExcl0.X1 = LDA.X1 AND LDExcl0.X2 = LDA.X2"
  )
  # print('finished LDExcl0_1')
  # print(Sys.time())

  # print('updating LDExcl0')
  # print(Sys.time())
  LDExcl0[LDExcl0_1$row_id, "label"] <- rep(1, nrow(LDExcl0_1))
  # print('finished LDExcl0')
  # print(Sys.time())

  # Now we can drop row_id; it was only needed to populate the label column correctly
  LDExcl0 <- subset(LDExcl0, select = -c(row_id))

  # print('building LDExcl0_0')
  # print(Sys.time())
  # LDExcl0_0 <- sqldf(
  #   'SELECT 0 as label, LDExcl0.* FROM LDExcl0, LDA WHERE LDExcl0.X1 <> LDA.X1 OR LDExcl0.X2 <> LDA.X2'
  # )
  # print('finished LDExcl0_0')
  # print(Sys.time())

  # print('joining LDExcl0_1, LDExcl0_0')
  # print(Sys.time())
  #
  # LDExcl0 <- rbind(LDExcl0_1, LDExcl0_0)
  #
  # print('finished LDExcl0_1, LDExcl0_0')
  # print(Sys.time())



  # system.time(LDExcl0 <- sqldf(
  #   c(
  #     '
  #         UPDATE LDExcl0 SET label = 1
  #         WHERE EXISTS (
  #           SELECT "x" FROM LDExcl0, LDA
  #             WHERE LDExcl0.X1 = LDA.X1 AND LDExcl0.X2 = LDA.X2
  #         )',
  #     "SELECT * FROM main.LDExcl0"
  #   )
  # ))

  ### regularize samples using maximum-minimum regularization method (98880*1955)
  B1 <- subset(LDExcl0[, ], select = -c(label, X1, X2))
  center <- sweep(B1, 2, apply(B1, 2, min), "-")
  R <- apply(B1, 2, max) - apply(B1, 2, min)
  B2 <- sweep(center, 2, R, "/")
  B3 <- subset(LDExcl0[, ], select = c(label, X1, X2))
  LDExcl0 <- cbind(B3, B2)
  SaveData(
    df = LDExcl0,
    file_name = "../output_data/lncRNA-disease-Excl0-regulation",
    file_format = file_format
  )
  LDExcl0 <- LoadData(
    file_name = "../output_data/lncRNA-disease-Excl0-regulation", file_format =
      file_format
  )

  ### consturct positive samples (2697*1955)
  PositiveSample <- LDExcl0[LDExcl0[, 1] == 1, ]
  SaveData(
    df = PositiveSample,
    file_name = "../output_data/PositiveSample",
    file_format = file_format
  )
  PositiveSample <- LoadData(
    file_name = "../output_data/PositiveSample", file_format =
      file_format
  )

  ### consturct unlabeled samples ((98880-2697=96183)*1955)
  UnlabeledSample <- LDExcl0[LDExcl0[, 1] == 0, ]
  SaveData(
    df = UnlabeledSample,
    file_name = "../output_data/UnlabeledSample",
    file_format = file_format
  )
  UnlabeledSample <- LoadData(
    file_name = "../output_data/UnlabeledSample", file_format =
      file_format
  )

  ### consturct negative samples (2697*1955)
  set.seed(1234)
  sp <- sample(
    nrow(UnlabeledSample),
    nrow(PositiveSample),
    replace = FALSE,
    prob = NULL
  )
  sps <- sort(sp)
  NegativeSample <- UnlabeledSample[sps, ]
  SaveData(
    df = NegativeSample,
    file_name = "../output_data/NegativeSample",
    file_format = file_format
  )
  NegativeSample <- LoadData(
    file_name = "../output_data/NegativeSample", file_format =
      file_format
  )

  ### construct training sample set by combining positive and negative samples (5394*1955)
  TrainingSample <- rbind(PositiveSample, NegativeSample)
  SaveData(
    df = TrainingSample,
    file_name = "../output_data/TrainingSample",
    file_format = file_format
  )
  TrainingSample <- LoadData(
    file_name = "../output_data/TrainingSample", file_format =
      file_format
  )
}

# system.time(PrepareData(file_format = "parquet"))

# user  system elapsed
# 179.677  15.908 183.646
##############################################################################################################



########################### compute variable importance score using Random Forest #############################

### compute variable importance score
ComputeFeatureImportance <- function(file_format) {
  ncores_to_use <- detectCores() - 1 # Use one less core than available for parallel processing

  ### read training sample set
  TrainingSample <- LoadData(file_name = "../output_data/TrainingSample", file_format = file_format)

  # Remove lncRNA and Disease columns
  B1 <- subset(TrainingSample[, ], select = -(X2:X3))

  # Used to store feature importance by model
  im <- NULL

  set.seed(1234)

  ### repeat 10 times
  for (i in 1:10)
  {
    ### train RandomForest model with parameter mtry=1952/3
    # Train the random forest using ranger
    rf <- ranger(
      X1 ~ .,
      data = B1,
      mtry = 651,
      importance = "impurity",
      # Use 'impurity' for Gini importance or 'permutation' for permutation importance
      num.trees = 500,
      num.threads = ncores_to_use
    )
    ### accumulate variable importance score
    if (is.null(im)) {
      im <- t(round(importance(rf), 3))[1, ]
    } else {
      im <- im + t(round(importance(rf), 3))[1, ]
    }
  }

  ### compute average variable importance score of 10 runnings
  im <- im / 10

  ### sort features by their variable importance score
  fsort <- sort(im, decreasing = TRUE)

  ### store feature names and correspongding variable importance score
  fs <- data.frame(attr(fsort, "names"), fsort)
  SaveData(
    df = fs,
    file_name = "../output_data/FeatureScore",
    file_format = file_format
  )
}

# system.time(ComputeFeatureImportance(file_format = "parquet"))
# user   system  elapsed
# 3345.435    2.353  188.555
##############################################################################################################



############# training RandomForest model with top 50, 100, ..., 1950 features ###############################

ComputeClassficationAccuracy <- function(file_format = file_format) {
  ### read training sample set consisting of 5394 lncRNA-disease pairs (5394*(3+1952))
  B <- LoadData(
    file_name = "../output_data/TrainingSample", file_format =
      file_format
  )
  ### read variable importance score of each feature
  fs <- LoadData(
    file_name = "../output_data/FeatureScore", file_format =
      file_format
  )

  ncores_to_use <- detectCores() - 1

  # We declare the number of tree's to use for the forest here to use it
  # for calculating number of tree's per core; this was declared as 500
  # in the initial code within the call to randomForest as ntree parameter
  ntrees_for_forest <- 500

  # Determine the number of trees per core
  ntree_per_core <- floor((ntrees_for_forest / ncores_to_use)) # Adjust based on the total number of cores and desired chunk size


  ### gaccuracy is used to record classification accuracy on differnt training sample subset
  gaccuracy <- matrix(0, nrow = 39, ncol = 2)

  tt <- 50

  ### nrow(df)=1952
  while (tt < nrow(fs)) {
    ### classification accuracy in each fold in 10-fold crossing validataion
    laccuracy <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    ### average classification accuracy in 10-fold crossing validataion
    lmeanaccuracy <- 0

    ### construct training sample subset consisted X1(label) + top tt feature
    ttt <- fs[1:tt, 1]
    B1 <- subset(B[, ], select = ttt)
    B3 <- subset(B[, ], select = X1)
    B4 <- cbind(B3, B1)

    ### random resampling in 10-fold crossing validation
    ind <- sample(
      10,
      nrow(B4),
      replace = TRUE,
      prob = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
    )

    for (i in 1:10)
    {
      ### train RandomForest model
      # rf=randomForest(X1~.,data = B4[ind != i,],mtry=floor(tt/3), importance = TRUE, ntree=500, na.action=na.omit)
      # NOTE by SRM!
      # Training this model is slow and using a single processor. See below
      # user  system elapsed
      # 28.947   0.042  28.973
      # This means for the entire work to complete which is the creation
      # of 10 models 200 times we are looking at approx 15 Hours!

      # Parallel random forest training
      # print(
      #     system.time({
      #       rf <- foreach(ntree = rep(ntree_per_core, ncores_to_use), .combine = combine, .packages = 'randomForest') %dopar% {
      #         randomForest(X1 ~ ., data = B4[ind != i,], mtry = floor(tt / 3), importance = TRUE, ntree = ntree, na.action = na.omit)
      #         }
      #     }
      #   )
      # )

      # Start - Parallel ranger training
      # Set the number of trees and other parameters
      ntree <- 500
      mtry_value <- floor(tt / 3)

      # Train the random forest using ranger with parallel processing
      print(system.time({
        rf <- ranger(
          X1 ~ .,
          data = B4[ind != i, ],
          mtry = mtry_value,
          importance = "impurity",
          # Use 'impurity' or 'permutation' for ranger importance
          num.trees = ntree,
          num.threads = ncores_to_use # Use one less core than available
        )
      }))


      # End of - Parallel ranger training

      ### predict using RadomForest model
      # NOTE! SRM
      # This was used with RandomForest object
      # pred <- predict(rf, B4[ind == i,])
      # the prediction function of ranger object returns more objects
      # however we only need the predictions hence the code change below
      pred <- predict(rf, B4[ind == i, ])$predictions

      ### judging the category of test samples with threshold = 0.5
      for (j in 1:length(pred))
      {
        if (pred[j] >= 0.5) {
          pred[j] <- 1
        } else {
          pred[j] <- 0
        }
      }

      ### construct confusion matrix
      t <- table(observed = B4[ind == i, "X1"], predicted = pred)

      ### compute classification accuracy
      laccuracy[i] <- (t[1, 1] + t[2, 2]) / (t[1, 1] + t[1, 2] + t[2, 1] +
        t[2, 2])
      lmeanaccuracy <- lmeanaccuracy + laccuracy[i] / 10
    }

    gaccuracy[tt / 50, 1] <- tt
    gaccuracy[tt / 50, 2] <- lmeanaccuracy

    tt <- tt + 50
    print(tt)
  }
  gaccuracy <- data.frame(gaccuracy)
  SaveData(
    df = gaccuracy,
    file_name = "../output_data/TrainingSample-gaccuracy",
    file_format = file_format
  )
}
# system.time(ComputeClassficationAccuracy(file_format = "parquet"))
# user    system   elapsed
# 51328.563    32.864  3050.017
##############################################################################################################



########################## 5-fold crossing validation on training sample set with 300 features ###############
### 5-fold crossing validation
FiveFoldCrossingValidation <- function(file_format) {
  ### read training sample set consisting of 5394 lncRNA-disease pairs (5394*(3+1952))
  B <- LoadData(
    file_name = "../output_data/TrainingSample", file_format =
      file_format
  )
  ### read variable importance score of each feature
  fs <- LoadData(
    file_name = "../output_data/FeatureScore", file_format =
      file_format
  )

  ### extract subset consisting of top 300 features
  tt <- 300
  ttt <- fs[1:tt, 1]
  B1 <- subset(B[, ], select = ttt)
  B2 <- subset(B[, ], select = X1)
  ### TB is training sample set without column X2（lncRNA name）and X3（disease name）
  TB <- cbind(B2, B1)

  ### read unlabeld sample set consisting of 96183 lncRNA-disease pairs ((98880-2697=96183)*(3+1952=1955))
  BB <- LoadData(
    file_name = "../output_data/UnlabeledSample", file_format =
      file_format
  )

  ### extract subset consisting of top 300 features
  B1 <- subset(BB[, ], select = ttt)
  B2 <- subset(BB[, ], select = X1)
  ### NB is unlabeled sample set without column X2（lncRNA name）and X3（disease name）
  NB <- cbind(B2, B1)

  sumauc <- 0
  sumap <- 0
  PTB <- TB[1:2697, ]
  # NOTE: SRM
  # Changed to use nrow to calculate row length to avoid hard coding of values
  # NTB <- TB[2698:5394,]
  NTB <- TB[2698:nrow(TB), ]

  len_PTB <- nrow(PTB)

  for (i in 1:5)
  {
    sample_start <- (540 * (i - 1)) + 1
    sample_end <- (540 * i)

    # avoid getting NULL rows which result from sampling with a
    # range larger than dataframe length
    if (sample_end > len_PTB) {
      sample_end <- len_PTB
    }

    ### training sample set
    PTB1 <- PTB[-(sample_start:sample_end), ]
    NTB1 <- NTB[-(sample_start:sample_end), ]
    TrainB <- rbind(PTB1, NTB1)

    ### test sample set
    PTB2 <- PTB[(sample_start:sample_end), ]
    TestB <- rbind(PTB2, NB)

    ### train RandomForest Model with parameter，try=the number of features（300）/3
    # rf=randomForest(X1~.,data = TrainB, mtry=100, importance = TRUE, ntree=500, na.action=na.omit)
    # switching to ranger as it supports parallel processing
    ntree <- 500
    mtry_value <- (300 / 3)
    print(system.time({
      rf <- ranger(
        X1 ~ .,
        data = TrainB,
        mtry = mtry_value,
        importance = "impurity",
        # Use 'impurity' or 'permutation' for ranger importance
        num.trees = ntree,
        num.threads = detectCores() - 1 # Use one less core than available
      )
    }))

    ### predict using RandomForest Model
    # NOTE! SRM
    # This was used with RandomForest object
    # pred <- predict(rf, TestB)
    # the prediction function of ranger object returns more objects
    # however we only need the predictions hence the code change below
    pred <- predict(rf, TestB)$predictions

    pred1 <- prediction(pred, TestB$X1)

    ### computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
    roc <- performance(pred1, "tpr", "fpr")
    plot(roc, main = "ROC chart")

    ### compute AUC value
    auc <- performance(pred1, "auc")@y.values
    print(auc)
    sumauc <- sumauc + as.numeric(auc[[1]])

    ### draw ROC precision/recall curve (x-axis: recall, y-axis: precision)
    perf1 <- performance(pred1, "prec", "rec")
    plot(perf1)

    ### compute AUPR value
    prec <- performance(pred1, "prec")@y.values
    rec <- performance(pred1, "rec")@y.values
    ap <- 0
    cur_rec <- rec[[1]][2]
    cur_prec <- prec[[1]][2]
    for (j in 3:length(rec[[1]])) {
      if (prec[[1]][j] >= cur_prec) {
        cur_prec <- prec[[1]][j]
      }
      if (abs(cur_rec - rec[[1]][j]) > 0) {
        ap <- ap + cur_prec * abs(cur_rec - rec[[1]][j])
      }
      cur_rec <- rec[[1]][j]
    }
    print(ap)
    sumap <- sumap + ap
  }

  ### compute AUC value
  sumauc <- sumauc / 5
  print(sumauc)

  ### compute AUPR value
  sumap <- sumap / 5
  print(sumap)
}
# system.time(FiveFoldCrossingValidation(file_format = "parquet"))
# user  system elapsed
# 193.168   4.086  23.835
##############################################################################################################



################################### predict all lncRNA-disease samples with 300 features#######################
Predict <- function(file_format) {
  ### read training sample set consisting of 5394 lncRNA-disease pairs (5394*(3+1952))
  B <- LoadData(
    file_name = "../output_data/TrainingSample", file_format =
      file_format
  )
  ### read variable importance score of each feature
  fs <- LoadData(
    file_name = "../output_data/FeatureScore", file_format =
      file_format
  )

  ### extract subset consisting of top 300 featues
  tt <- 300
  ttt <- fs[1:tt, 1]
  B1 <- subset(B[, ], select = ttt)
  B2 <- subset(B[, ], select = X1)
  ### TB is training sample set without column X2（lncRNA name）and X3（disease name）
  TB <- cbind(B2, B1)

  ### read unlabeld sample set consisting of 96183 lncRNA-disease pairs ((98880-2697=96183)*(3+1952=1955))
  BB <- LoadData(
    file_name = "../output_data/UnlabeledSample", file_format =
      file_format
  )

  ### extract subset consisting of top 300 featues
  tt <- 300
  ttt <- fs[1:tt, 1]
  B1 <- subset(BB[, ], select = ttt)
  B2 <- subset(BB[, ], select = X1)
  ### NB is unlabeled sample set without column X2（lncRNA name）and X3（disease name）
  NB <- cbind(B2, B1)

  ### train RandomForest Model with parameter，try=the number of features（300）/3
  # rf=randomForest(X1~.,data = TB, mtry=100, importance = TRUE, ntree=500, na.action=na.omit)
  # switching to ranger as it supports parallel processing
  ntree <- 500
  mtry_value <- (300 / 3)
  print(system.time({
    rf <- ranger(
      X1 ~ .,
      data = TB,
      mtry = mtry_value,
      importance = "impurity",
      # Use 'impurity' or 'permutation' for ranger importance
      num.trees = ntree,
      num.threads = detectCores() - 1 # Use one less core than available
    )
  }))

  ### predict all unknwon samples using RandomForest Model
  pred <- predict(rf, NB)$predictions
  NB1 <- BB[, 1:3]
  UnlabeledSampleScore <- cbind(NB1, data.frame(pred))
  SaveData(
    df = UnlabeledSampleScore,
    file_name = "../output_data/UnlabeledSampleScore-300-features",
    file_format = file_format
  )
}
# system.time(Predict(file_format = "parquet"))
# user  system elapsed
# 46.474   1.037   5.311
################################################################################################################
