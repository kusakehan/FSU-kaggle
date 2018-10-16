rm(list=ls())
#install.packages("plyr")
library(plyr)
train = read.csv("final_train.csv")
test = read.csv("final_test.csv")

# -------------------------------------------------------------------
# Rename the "JobLevel" and "JobFunction" to avoid special characters
# -------------------------------------------------------------------

rename_JobLevel <- function(x) {
  JobLevel.new <- gsub("Manager Level", replacement = "Manager", x)
  JobLevel.new <- gsub("Board Level", replacement = "Board", JobLevel.new) 
  JobLevel.new <- gsub("C Level", replacement = "C",JobLevel.new) 
  JobLevel.new <- gsub("Executive Management", replacement = "Executive_Management", JobLevel.new)
  JobLevel.new <- gsub("Manger Level", replacement = "Manager", JobLevel.new)
  JobLevel.new <- gsub("Staff Level", replacement = "Staff", JobLevel.new)
  JobLevel.new <- gsub("Student/Job Seeker", replacement = "Student", JobLevel.new)
  JobLevel.new <- gsub("VP Level", replacement = "VP", JobLevel.new)
  JobLevel.new <- gsub("Director Level", replacement = "Director", JobLevel.new)
  JobLevel.new
}

rename_JobFunction <- function(x) {
  JobFunction.new <- gsub("General Management", replacement = "General_Management", x)
  JobFunction.new <- gsub("Product Management", replacement = "Product_Management",  JobFunction.new )
  JobFunction.new <- gsub("Software Developer", replacement = "Software_Developer",  JobFunction.new )
  JobFunction.new <- gsub("Software development", replacement = "Software_Developer",  JobFunction.new )
  JobFunction.new <- gsub("Finance & Accounting", replacement = "Finance_Accounting",  JobFunction.new )
  JobFunction.new <- gsub("Purchasing & Procurement", replacement = "Purchasing_Procurement",  JobFunction.new )
  JobFunction.new
}



train$JobLevel <- rename_JobLevel(train$JobLevel)
test$JobLevel <- rename_JobLevel(test$JobLevel)

train$JobFunction <- rename_JobFunction(train$JobFunction)
test$JobFunction <- rename_JobFunction(test$JobFunction)


# ---------------------------------------------------------------
# Add two new features using "Title": word count and letter count
# ---------------------------------------------------------------

train$Title_wc <- sapply(gregexpr("[[:alpha:]]+", train$Title), function(x) sum(x > 0))
test$Title_wc <- sapply(gregexpr("[[:alpha:]]+", test$Title), function(x) sum(x > 0))  

train$Title_lc <- sapply(as.character(train$Title), nchar, type = "chars")
test$Title_lc <- sapply(as.character(test$Title), nchar, type = "chars")

train$Title_wc[which(is.na(train$Title_wc))] <- 0 
test$Title_wc[which(is.na(test$Title_wc))] <- 0 

train$Title_lc[which(is.na(train$Title_lc))] <- 0 
test$Title_lc[which(is.na(test$Title_lc))] <- 0 

# --------------------------------------------------
# Explicitly express all 2-way and 3-way interations
# --------------------------------------------------

get_2way_iteractions <- function(data, features) {
  for(i in 1:(length(features)-1)) {
    for(j in (i+1):length(features)){
      it.name <- paste(c(features[i], features[j]), collapse = "**")
      string <- cbind(data[, features[i]], data[, features[j]])
      data[, it.name] <- apply(string, 1, paste, collapse = "++")
    }
  }
  return(data)
}

get_3way_iteractions <- function(data, features) {
  for(i in 1:(length(features)-2)) {
    for(j in (i+1):(length(features)-1)) {
      for(k in (j+1):length(features)) {
       it.name <- paste(c(features[i], features[j], features[k]), collapse = "***")
       string <- cbind(data[, features[i]], data[, features[j]], data[, features[k]])
       data[, it.name] <- apply(string, 1, paste, collapse = "+++")
      }
    }
  }
  return(data)
}


cate_col <- c("Title_wc", "Title_lc", "JobFunction", "JobLevel", 
              "InitialSourceCategory", "FinalSourceCategory",
              "EmployeeCount", "PromotionCodeCategory", "EditionCategory")

train <- get_2way_iteractions(train[, !names(train)=="Title"], cate_col)
test  <- get_2way_iteractions(test[,!names(test)=="Title"], cate_col)

train <- get_3way_iteractions(train, cate_col)
test <- get_3way_iteractions(test, cate_col)

# --------------------------------------------
# Add "COUNT" feature for each interation term
# --------------------------------------------

features <- names(train)[!names(train) %in% c("Id", "Purchase")]

features_inter <- features[!features %in% cate_col ]

cat2cnt <- function(data, col) {
  # Covert a categorical variable to the 'count' of samples in each level
  data_col <- data.frame(ID = c(1:nrow(data)),  var = as.factor(data[, col]))
  grpCount <- count(data_col, "var")
  x <- merge(data_col, grpCount, by = "var", all.x = TRUE)
  return(x[order(x$ID), "freq"])
}

data <- rbind(train[, !names(train)=="Purchase"], test)
for(i in 1:length(features_inter) ) {
  idx.name <- paste(c(features_inter[i], "CNT"), collapse = "_")
  data[, idx.name] <- cat2cnt(data, features_inter[i])
  #print(i)
}

tr <- read.csv("final_train.csv")
tr_idx <- 1:nrow(train)
train <- cbind(data[tr_idx,], Purchase = tr[, "Purchase"])
test <- data[-tr_idx,]

rm(list = c("data"))

# --------------------------------------------------------------------------
# Convert all the current features (excluding "COUNT") to "Average Response"
# --------------------------------------------------------------------------

cat2num <- function(data1, data2, columnName, cnt_thres = 0, LOO = FALSE, rand = FALSE,
                    r_k = 0.1) {
  
  #  Convert a categorical variable to the 'average' of all the responses in each level.
  
  #LOO (leave-one-out): tick out the current sample when computing the average.
  #rand: add some random noises to the resulting numerical values.
  #r_k: control the scale of random noise.
  da1 <- data.frame(var = as.factor(data1[, columnName]), Purchase = data1[, "Purchase"])
  grpAve <- aggregate(.~ var, data = da1, mean)
  grpCount <- count(da1, "var")
  grpAve[, "cnt"] <- grpCount$freq
  
  if(LOO) {
    grpAve[which(grpAve$cnt <= cnt_thres), "cnt"] <- NA
  } else {
    grpAve <- grpAve[grpAve$cnt > cnt_thres, ]
  }
  da2_col <- data.frame(ID = c(1:nrow(data2)), var = as.factor(data2[, columnName]))
  da2 <- merge(da2_col, grpAve, by = "var", all.x = TRUE)
  x = da2[order(da2$ID), "Purchase"]
  
  if(LOO) {
    outcomes <- data2[,"Purchase"]
    x <- ((x*da2[, "cnt"] - outcomes))/(da2[, "cnt"]-1)
  }
  if(rand) {
    x <- x*(1+runif(nrow(da2), -r_k, r_k))
  }
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    return(x)
}

for (clm  in features) {
   print(clm)
   test[, clm] <- cat2num(train, test, clm, cnt_thres = 1)
   train[, clm] <- cat2num(train, train, clm, cnt_thres = 1, LOO = TRUE, rand = TRUE)
}


# -----------------------------
# Train multiple xgboost models
# -----------------------------
library(xgboost)
xgb_train <- function(train, test, features, rep) {
  response1 <- nrow(train[train$Purchase == 1, ])
  response0 <- nrow(train) - response1
  scale_pos_weight <- response1/response0
  print(response1)
  print(response0)
  print(scale_pos_weight)
  
  dtrain <- xgb.DMatrix(as.matrix(train[, features]), label = train[, 'Purchase'])
  dtest  <- xgb.DMatrix(as.matrix(test[, features]))
  
  params = c()
  params$objective = "binary:logistic"
  params$eta = 0.01
  params$max_depth = 10
  params$colsample_bytree = 0.8
  params$min_child_weight = 10
  params$base_score = 0.005
  params$silent = TRUE
  params$scale_pos_weight = scale_pos_weight
  
  #print(Fitting)
  #trainpredictions = rep(NA, nrow(train))
  #testpredictions = rep(NA, nrow(test))
  
  for (i in 3:rep) {
    cat("Time", i)
    params$seed = i*10
    
    watchlist = list(train = dtrain, val = dtrain)
    params$eval_metric = "auc"
    
    clf = xgb.train(params, data = dtrain, nrounds = 800 , watchlist = watchlist,
                    early_stopping_rounds = 50, maximize = TRUE)
    limit = clf$best_iteration + 1
    
    predictions = predict(clf, newdata = dtrain, ntree_limit = limit)
    
    if(i == 1) {
       trainpredictions <- predictions
    } else {
       trainpredictions <- trainpredictions + predictions
    }
    
    predictions = predict(clf, newdata = dtest, ntree_limit = limit)
    if(i == 1) {
      testpredictions <- predictions
    } else {
      testpredictions <- testpredictions + predictions
    }
    
    imp = xgb.importance(feature_names = features,  model = clf)
  }
  
  avetrainpred <- trainpredictions/rep
  avetestpred <- testpredictions/rep
  
  return(list(avetrainpred, avetestpred))
}

tr_te_pred <- list(trainpredictions, testpredictions)
tr_te_pred <- xgb_train(train, test, features, rep = 20)


train_prob <- data.frame(Id = train[, 'Id'], Prediction = tr_te_pred[[1]],
                         Purchase = train[, "Purchase"])

test_prob <- data.frame(Id = test[, 'Id'], Purchase = tr_te_pred[[2]])

write.csv(train_prob, "train_prob2.csv")
write.csv(test_prob, "submission2.csv")