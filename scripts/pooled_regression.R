library(stringr)
library(dplyr)
source("./pooled_regression_base.R")

# TODO: feature scaling
horizon = 28

# sales data
all_sales_data = read.csv("../data/sales_train_valid_truncated.csv")

# calendar data
all_calendar_data = read.csv("../data/calendar.csv")

# add clumn for the day_of_month
all_calendar_data["day_of_month"] = as.integer(str_split_fixed(all_calendar_data$date,"-", 3)[,3])
calendar_data_train = all_calendar_data[1:(nrow(all_calendar_data)-horizon*2),]
calendar_data_test = all_calendar_data[(nrow(all_calendar_data)-horizon*2 + 1):(nrow(all_calendar_data)-horizon),]

# price data
all_price_data = read.csv("../data/sell_prices.csv")

lag=56
output_file_name = paste("./pooled_regression_lag", lag, "_forecasts.txt", sep="")

unlink(output_file_name)

model_results = NULL

sales_data = all_sales_data[, 7:ncol(all_sales_data)]
metda_data = all_sales_data[, 1:6]

# convert sales data to matrix
sales_data <- as.matrix(sales_data)

# normalize sales data by the mean
series_means = rowMeans(sales_data, na.rm = TRUE)
sales_data_mean_normalized = sales_data/series_means

# convert sales data to list of vectors
sales_data_mean_normalized.list <- split(sales_data_mean_normalized, seq(nrow(sales_data_mean_normalized)))

# perform embedding on the list elements
embedded_sales_data.list <- lapply(sales_data_mean_normalized.list, embed, dimension = lag + 1)

no_of_rows_in_each_series = ncol(all_sales_data) - 6 - lag
meta_data_embedded = metda_data[rep(seq_len(nrow(metda_data)), each = no_of_rows_in_each_series), ]

# create one matrix and combine the metadata
embedded_sales_data = do.call(rbind, embedded_sales_data.list)
embedded_data = cbind(meta_data_embedded, embedded_sales_data)

# combine the calendar data
calendar_data_train_embedded = calendar_data_train[(lag + 1):nrow(calendar_data_train),]
embedded_data = cbind(embedded_data, calendar_data_train_embedded)

# remove the na data
embedded_data_clean = na.omit(embedded_data)

# combine the price data
final_embedded_train_data = inner_join(embedded_data_clean, all_price_data)

# create the snap variable
final_embedded_train_data %>% mutate(snap=case_when(state_id=="CA" ~ snap_CA,
                                                    state_id=="TX" ~ snap_TX,
                                                    state_id=="WI" ~ snap_WI))

# create the series means vector
modified_no_of_rows_in_each_series = final_embedded_train_data %>% group_by(item_id, store_id) %>% count() %>% ungroup() %>% pull(n)
series_means_vector = as.numeric(rep(series_means, modified_no_of_rows_in_each_series))

# drop unwanted columns
to_drop = c("id", "wm_yr_wk", "date", "weekday", "year", "d", "snap_CA", "snap_TX", "snap_WI")
final_embedded_train_data = final_embedded_train_data[, !(names(final_embedded_train_data) %in% to_drop)]

# convert to correct data types
final_embedded_train_data$wday = factor(final_embedded_train_data$wday)
final_embedded_train_data$month = factor(final_embedded_train_data$month)
final_embedded_train_data$snap = factor(final_embedded_train_data$snap)
final_embedded_train_data$day_of_month = factor(final_embedded_train_data$day_of_month)
final_embedded_train_data$item_id = factor(final_embedded_train_data$item_id)
final_embedded_train_data$store_id = factor(final_embedded_train_data$store_id)
final_embedded_train_data$state_id = factor(final_embedded_train_data$state_id)
final_embedded_train_data$dept_id = factor(final_embedded_train_data$dept_id)
final_embedded_train_data$cat_id = factor(final_embedded_train_data$cat_id)

# fit a normal model
colnames(final_embedded_train_data)[which(names(final_embedded_train_data) == "1")] <- "y"
for (i in 2:(lag+1)){
  colnames(final_embedded_train_data)[which(names(final_embedded_train_data) == toString(i))] <- paste("Lag", (i - 1), sep="")
}

print("Embedding Completed")
model = fit_normal_model(fitting_data = final_embedded_train_data) 


print("Training Completed")


# create the final test data
test_data = final_embedded_train_data %>% group_by(item_id, store_id) %>% slice(n()) %>% ungroup()
test_data = test_data[, !(names(test_data) %in% c(paste0("Lag", lag)))]

# recursively predict for all the series until the forecast horizon
predictions = NULL
previous_week = NULL
for (i in 1:horizon){
  print(paste0("horizon: ",i))
  for (j in (lag-1):1){
    name = paste("Lag", j, sep="")
    colnames(test_data)[which(names(test_data) == name)] <- paste("Lag", (j+1), sep="")
  }
  if (i == 1){
    colnames(test_data)[which(names(test_data) == "y")] <- paste("Lag", 1, sep="") 
  }else{
    colnames(test_data)[which(names(test_data) == "new_predictions")] <- paste("Lag", 1, sep="")
  }
  
  # create the new external calendar data
  external_variables = calendar_data_test[i, ]
  current_week = external_variables$wm_yr_wk
  
  # drop the previous date specific data
  test_data = test_data[, !(names(test_data) %in% append(names(external_variables), c("snap")))]
  
  # add new external calendar data into the embedded data
  test_data = cbind(test_data, external_variables)
  test_data %>% mutate(snap=case_when(state_id=="CA" ~ snap_CA,
                                      state_id=="TX" ~ snap_TX,
                                      state_id=="WI" ~ snap_WI))
  
  
  # add price data into the test data
  if (is.null(previous_week) || current_week != previous_week){
    test_data = test_data[, !(names(test_data) %in% c("sell_price"))]
    test_data = right_join(all_price_data, test_data) 
  }
  
  # convert to correct data types
  test_data$wday = factor(test_data$wday)
  test_data$month = factor(test_data$month)
  test_data$snap = factor(test_data$snap)
  test_data$day_of_month = factor(test_data$day_of_month)
  test_data$item_id = factor(test_data$item_id)
  test_data$store_id = factor(test_data$store_id)
  test_data$state_id = factor(test_data$state_id)
  test_data$dept_id = factor(test_data$dept_id)
  test_data$cat_id = factor(test_data$cat_id)
  
  # drop unwanted columns
  to_drop = c("wm_yr_wk", "date", "weekday", "year", "d", "snap_CA", "snap_TX", "snap_WI")
  test_data = test_data[, !(names(test_data) %in% to_drop)]
  
  new_predictions = predict.glm(object=model, newdata = test_data)  
  predictions = cbind(predictions, new_predictions)
  
  # update the final lags
  last_lag = paste("Lag", lag, sep="")
  test_data = test_data[, !(names(test_data) %in% last_lag)]
  
  # save the current week
  previous_week = external_variables$wm_yr_wk
  
  test_data = cbind(new_predictions, test_data)
  
}
# renormalize the predictions
true_predictions = predictions * as.vector(series_means)
true_predictions = as.data.frame(true_predictions)
# make the submission
true_predictions = cbind(all_sales_data$id, true_predictions)

# add new names to the columns
colnames(true_predictions)[1] <- "id"
for (forecast in 1:horizon) {
  colnames(true_predictions)[forecast+1] <- paste("F", forecast, sep="")
}

# write true predictions to file
write.table(true_predictions, file=output_file_name, row.names = F, sep=",", quote=F)
