library(lightgbm)
library(Matrix)

# Defining paths and initial variables
BASE_DIR <- "M5Comp"
CLUSTER_BASE_DIR <- file.path(BASE_DIR, "dataset", "clusters")
COMP_DATA_DIR <- file.path(BASE_DIR, "dataset", "comp_data", "comp_data")
PREPROCESSED_DATA_DIR <- file.path(BASE_DIR, "dataset", "preprocessed_data")   
FORECAST_DIR <- file.path(BASE_DIR,"results", "final_submission_results")
TECHNIQUE <- "lightgbm"
dataset_name <- "m5_final"
horizon <- 28
lag <- 400
optimal_num_of_clusters <- 70


# Loading datasets
dataset <- read.csv(file.path(PREPROCESSED_DATA_DIR, "original_m5_evaluation_dataset.txt"), header = FALSE)
state_info <- read.csv(file.path(COMP_DATA_DIR, "sales_train_evaluation.csv"), header = TRUE)
date_info <- read.csv(file.path(COMP_DATA_DIR, "calendar.csv"), header = TRUE)

num_of_series <- nrow(dataset)

output_file_name <- file.path(FORECAST_DIR, paste0(dataset_name, "_",TECHNIQUE,"_", lag, "_forecasts.txt"))
unlink(output_file_name)

date_info$date <- sapply(date_info$date, as.character)
date_info$day1 <- strsplit(date_info$date, "-")

for(val in 1:nrow(date_info)){
  date_info[val, "day"] <-  as.numeric(date_info[val, "day1"][[1]][3])
}

event_name_one_vals <- as.vector(unique(date_info$event_name_1))
event_name_two_vals <- as.vector(unique(date_info$event_name_2))
event_type_one_vals <- as.vector(unique(date_info$event_type_1))
event_type_two_vals <- as.vector(unique(date_info$event_type_2))


# Function to extract the external variables
# Parameters
# series_index - series number
# date_index - date number
get_external_vars <- function(series_index, date_index){
  state <- as.character(state_info[series_index, "state_id"])
  current_date_info <- date_info[date_index,]
  month <- as.numeric(current_date_info$month)
  snap <- as.numeric(current_date_info[[paste0("snap_", state)]])
  wday <- as.numeric(current_date_info$wday)
  day <- as.numeric(current_date_info$day)
  event_name_1 <- which(event_name_one_vals==as.character(current_date_info$event_name_1))
  event_name_2 <- which(event_name_two_vals==as.character(current_date_info$event_name_2))
  event_type_1 <- which(event_type_one_vals==as.character(current_date_info$event_type_1))
  event_type_2 <- which(event_type_two_vals==as.character(current_date_info$event_type_2))

  if(as.character(current_date_info$weekday) %in% c("Saturday", "Sunday")){
    weekday <- 0
  }else{
    weekday <- 1
  }

  list("month"=month,
       "snap"=snap,
       "weekday"=weekday,
       "wday"=wday,
       "day"=day,
       "event_name_1"=event_name_1,
       "event_name_2"=event_name_2,
       "event_type_1"=event_type_1,
       "event_type_2"=event_type_2
  )
}


# Function to calculate the forecasts for a given cluster of series by training a lightgbm model
# Parameters
# cluster_data - a cluster of series that should be used for model training
# cluster_series_numbers - series indexes (numbers) corresponding with the clustered series
# lag - number of past values should be used from a series for model training
calculate_forecasts <- function(cluster_data, cluster_series_numbers, lag) {
  embedded_series <- NULL
  final_lags <- NULL
  series_means <- NULL

  for (i in 1:nrow(cluster_data)) {
    time_series <- cluster_data[i,]
    time_series <- as.numeric(time_series)

    # Dividing the series by its mean
    mean <- mean(time_series)
    series_means <- c(series_means, mean)
    time_series <- time_series / mean

    # Embedding the series
    embedded <- embed(time_series, lag + 1)

    # Adding external variables
    weekday_vec <- c()
    month_vec <- c()
    snap_vec <- c()
    wday_vec <- c()
    day_vec <- c()
    event_name_1_vec <- c()
    event_name_2_vec <- c()
    event_type_1_vec <- c()
    event_type_2_vec <- c()

    for(j in (lag+1):length(time_series)){
      external_vars <- get_external_vars(cluster_series_numbers[i], j)
      month_vec <- c(month_vec, external_vars$month)
      snap_vec <- c(snap_vec, external_vars$snap)
      wday_vec <- c(wday_vec, external_vars$wday)
      day_vec <- c(day_vec, external_vars$day)
      event_name_1_vec <- c(event_name_1_vec, external_vars$event_name_1)
      event_name_2_vec <- c(event_name_2_vec, external_vars$event_name_2)
      event_type_1_vec <- c(event_type_1_vec, external_vars$event_type_1)
      event_type_2_vec <- c(event_type_2_vec, external_vars$event_type_2)
      weekday_vec <- c(weekday_vec, external_vars$weekday)
    }

    embedded <- cbind(embedded, weekday_vec, month_vec, snap_vec, wday_vec, day_vec, event_name_1_vec, event_name_2_vec, event_type_1_vec, event_type_2_vec)
    
    if (!is.null(embedded_series)) {
      embedded_series <- as.matrix(embedded_series)
    }
    embedded_series <- rbind(embedded_series, embedded)
    
    if (!is.null(final_lags)) {
      final_lags <- as.matrix(final_lags)
    }

    # Creating the test set
    current_series_final_lags <- t(as.matrix(rev(tail(time_series, lag))))
    final_external_vars <- get_external_vars(cluster_series_numbers[i], (length(time_series)+1))

    current_series_final_lags <- cbind(current_series_final_lags,
                                       final_external_vars$weekday,
                                       final_external_vars$month,
                                       final_external_vars$snap,
                                       final_external_vars$wday,
                                       final_external_vars$day,
                                       final_external_vars$event_name_1,
                                       final_external_vars$event_name_2,
                                       final_external_vars$event_type_1,
                                       final_external_vars$event_type_2
    )

    final_lags <- rbind(final_lags, as.data.frame(current_series_final_lags))
  }


  # Changing the column type of embeded matrix - lags are numeric and external variabes are factors
  embedded_series <- as.data.frame(embedded_series)
  colnames(embedded_series)[1] <- "y"
  colnames(embedded_series)[2:(lag+1)] <- paste("Lag", 1:lag, sep = "")
  indx <- 1:(lag+1)
  embedded_series[indx] <- lapply(embedded_series[indx], function(x) as.numeric(x))
  indx <- (lag+2):ncol(embedded_series)
  embedded_series[indx] <- lapply(embedded_series[indx], function(x) as.factor(x))

  # Changing the column type of test set - lags are numeric and external variabes are factors
  final_lags <- as.data.frame(final_lags)
  colnames(final_lags)[1:lag] <- paste("Lag", 1:lag, sep = "")
  colnames(final_lags)[(lag+1):ncol(final_lags)] <- colnames(embedded_series)[(lag+2): ncol(embedded_series)]
  indx <- 1:lag
  final_lags[indx] <- lapply(final_lags[indx], function(x) as.numeric(x))
  indx <- (lag+1):ncol(final_lags)
  final_lags[indx] <- lapply(final_lags[indx], function(x) as.factor(x))

  cluster_forecasts <- fit_lightgbm_model(fitting_data = embedded_series, final_lags, cluster_series_numbers, horizon, series_means)  
  cluster_forecasts
}

# Fit and forecast from a lightgbm model for clustered data
# Parameters
# fitting_data - embeded matrix (training data)
# final_lags - test set
# cluster_series_numbers - series indexes (numbers) corresponding with the clustered series
# horizon - forecast horizon (28)
# series_means - the calculated mean values of the series in the cluster
fit_lightgbm_model <- function(fitting_data, final_lags, cluster_series_numbers, horizon, series_means) {

  # Defining hyperparameters
  lgb.grid <- list(objective = "poisson",
                  metric = "rmse",
                  bagging_freq = 1,
                  lambda_l2 = 0.1,
                  learning_rate = 0.075,
                  num_leaves = 128,
                  min_data_in_leaf = 100,
                  boosting_type = 'gbdt',
                  force_row_wise = TRUE,
                  sub_row = 0.75,
                  verbosity = 1,
                  num_iterations = 1200
  )

  train <- Matrix(as.matrix(fitting_data[-1]), sparse = TRUE)
  y_train <- as.numeric((fitting_data[,1]))
  dtrain <- lgb.Dataset(data = train, label = y_train, free_raw_data = FALSE)

  categoricals.vec <- colnames(final_lags)[-(1:lag)]

  # Fit the model
  model <- lgb.train(params = lgb.grid, data = dtrain, categorical_feature = categoricals.vec )

  # Do forecasting
  predictions <- forec_recursive_clustering(model, final_lags, cluster_series_numbers, horizon, series_means)
  predictions
}


# Function to recursively forecast all the series until the forecast horizon
# Parameters
# model - the trained lightgbm model
# final_lags - test set
# cluster_series_numbers - series indexes (numbers) corresponding with the clustered series
# horizon - forecast horizon (28)
# series_means - the calculated mean values of the series in the cluster
forec_recursive_clustering <- function(model, final_lags, cluster_series_numbers, horizon, series_means) {
  
  predictions <- NULL

  for (ho in 1:horizon){
    new_predictions <- predict(model, Matrix(as.matrix(final_lags), sparse=TRUE))
    predictions <- cbind(predictions, new_predictions)

    # Updating the test set for the next horizon
    if(ho < horizon){
        final_lags <- final_lags[-lag]
        if(dim(final_lags)[2] == 1){
          final_lags <- t(final_lags)
        }
        
        # Updating lags for the next horizon
        final_lags <- cbind(new_predictions, final_lags)
        colnames(final_lags)[1:lag] <- paste("Lag", 1:lag, sep="")
        
        for(col in (lag+1):ncol(final_lags)){
          final_lags[col] <- lapply(final_lags[col], as.character)[[1]]
        }
        
        # Updating the external variables for the next horizon
        for(series in 1:nrow(final_lags)){
          next_external_var <- get_external_vars(cluster_series_numbers[series], (ncol(dataset)+ho+1))
          final_lags[series, "weekday_vec"] <- next_external_var$weekday
          final_lags[series, "month_vec"] <- next_external_var$month
          final_lags[series, "snap_vec"] <- next_external_var$snap
          final_lags[series, "wday_vec"] <- next_external_var$wday
          final_lags[series, "day_vec"] <- next_external_var$day
          final_lags[series, "event_name_1_vec"] <- next_external_var$event_name_1
          final_lags[series, "event_name_2_vec"] <- next_external_var$event_name_2
          final_lags[series, "event_type_1_vec"] <- next_external_var$event_type_1
          final_lags[series, "event_type_2_vec"] <- next_external_var$event_type_2
        }
        
        for(col in (lag+1):ncol(final_lags)){
          final_lags[col] <- lapply(final_lags[col], as.factor)[[1]]
        }
        
        final_lags <- as.data.frame(final_lags)
    }
  
  }

  # Renormalize the predictions
  true_predictions <- predictions * as.vector(series_means)
  true_predictions
}


current_clusters <- readLines(file.path(CLUSTER_BASE_DIR, "m5_clusters_70.txt"))


# Obtaining lightgbm forecasts for all clusters
for(clus in 1:optimal_num_of_clusters){
  print(paste0("Starting cluster ", clus))
  processing_cluster <- as.numeric(unlist(strsplit(current_clusters[[clus]], " ")))
  processing_cluster <- processing_cluster + 1
  processing_dataset <- dataset[processing_cluster,]
  forecasts <- calculate_forecasts(processing_dataset, processing_cluster, lag)
  
  forecasts[forecasts < 0] <- 0
  
  # Clusters contain series in the same order as in original data file. Hence, can directly write forecasts into the output file.
  write.table(forecasts, output_file_name, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",", append = TRUE)
  
  print(paste0("Finished cluster ", clus))
}

print("finished")






