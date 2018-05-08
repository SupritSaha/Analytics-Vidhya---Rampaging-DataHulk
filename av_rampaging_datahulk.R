
# Load Required packages
# =========================================================================
package_names <- c("data.table","h2o","plyr")
loadPackage <- function(pkg)
{
  if(missing(pkg) || !is.character(pkg))
  {
    stop("Package not correctly entered !!!")
  }
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if(length(new.pkg)) 
  {
    install.packages(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
  cat("Packages Loaded !!!")
}
suppressPackageStartupMessages(suppressWarnings(loadPackage(package_names)))

# Reading train and test data
# ==========================================================================
dt_train <- fread("train.csv",header = TRUE)
dt_test <- fread("test.csv",header = TRUE)

# Sorting the observations by Stock ID and timestamp
# ==========================================================================
dt_train <- dt_train[order(Stock_ID,timestamp),]
dt_test <- dt_test[order(Stock_ID,timestamp),]

# Checking if train data Stocks are in test data
# =========================================================================
length(unique(dt_train$Stock_ID))
length(unique(dt_test$Stock_ID))
length(intersect(dt_train$Stock_ID,dt_test$Stock_ID))

# ==============================================================================
# Computing transition matrix
# =========================================================================
transMat <- function(x,models)
{
  if(missing(x))
  {
    stop("No input character sequence!")
  }
  if(is.atomic(x) && !is.character(x))
  {
    #message("Changing input sequence to character vector!")
    x <- as.character(x)
  }
  x <- factor(x,levels = models)
  p <- matrix(0,nrow = length(models),ncol = length(models))
  for (t in 1:(length(x) - 1)) 
  {
    p[x[t], x[t + 1]] <- p[x[t], x[t + 1]] + 1
  }
  for (i in 1:length(models)) 
  {
    p[i, ] <- p[i, ] / sum(p[i, ],na.rm = TRUE)
  }
  rownames(p) <-  colnames(p) <- models
  return(p)
}

# Summary by Stock ID
# ================================================================================
trans_summary <- dt_train[,as.list(c(mean = mean(Outcome,na.rm = TRUE),transMat(Outcome,models = c(0,1)))),by = c("Stock_ID")]
names(trans_summary) <- c("Stock_ID","mean_outcome","zero_zero","one_zero","zero_one","one_one")


# Merging group summaries with train and test data
# ================================================================================
dt_train_new <- join(dt_train,trans_summary,by = "Stock_ID",type = "left")
dt_test_new <- join(dt_test,trans_summary,by = "Stock_ID",type = "left")


# Changing data type
# ==============================================================================
dt_train_new$Outcome <- as.factor(dt_train$Outcome)

# Using h2o gbm
# =========================================================================
h2o.init(nthreads = -1)
trainHex <- as.h2o(dt_train_new)
testHex <- as.h2o(dt_test_new)

features <- c("Three_Day_Moving_Average","Five_Day_Moving_Average","Ten_Day_Moving_Average",
              "Ten_Day_Moving_Average","Twenty_Day_Moving_Average","True_Range","Average_True_Range",
              "Positive_Directional_Movement","Negative_Directional_Movement","mean_outcome",
              "zero_zero","one_zero","zero_one","one_one")
target <- "Outcome"

gbm_fit <- h2o.gbm(x = features,
                   y = target,training_frame = trainHex,
                   ntrees = 200,
                   sample_rate = 0.65,
                   col_sample_rate = 0.3,
                   learn_rate = 0.01,
                   stopping_metric = "logloss",
                   seed = 1234,
                   nfolds = 5,
                   fold_assignment = "Stratified"
                    )

# Predicting probabilities on test data
# =====================================================================
gbm_pred <- as.data.table(h2o.predict(gbm_fit,testHex))

# Submission
# =================================================================
dt_submission <- data.table(ID = dt_test_new$ID, Outcome = gbm_pred$p1)
dt_submission <- join(dt_test,dt_submission,by = c("ID"),type = "left")
dt_submission <- dt_submission[,c("ID","Outcome"),with = FALSE]
fwrite(dt_submission,file = "submission_h2o_gbm.csv",row.names = FALSE)
