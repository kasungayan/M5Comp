library(glmnet)
library(pscl)

set.seed(1)

BASE_DIR <- "C:/Projects/M5Comp/"
FORECAST_DIR <- paste0(BASE_DIR, "results/zero_inflated_forecasts/")
TECHNIQUE <- "zero_inflated"   #poisson or zero_inflated
dataset_name <- "m5"
horizon <- 28
lag <- 10

output_file_name <- paste0(FORECAST_DIR, dataset_name, "_", TECHNIQUE, "_", lag, "_forecasts.txt")
unlink(output_file_name)

dataset <- read.csv(paste0(BASE_DIR, "dataset/preprocessed_data/original_m5_dataset.txt"), header = FALSE)
state_info <- read.csv(paste0(BASE_DIR, "dataset/comp_data/comp_data/sales_train_validation.csv"), header = TRUE)
date_info <- read.csv(paste0(BASE_DIR, "dataset/comp_data/comp_data/calendar.csv"), header = TRUE)
price_info <- read.csv(paste0(BASE_DIR, "dataset/preprocessed_data/Item-prices.txt"), header = FALSE)


embedded_series <- NULL
final_lags <- NULL
#series_means <- NULL


fill_vec <- function(data, attribute, null_string){
  out <- null_string
  if(as.character(data[[attribute]]) != ""){
    out <- as.character(data[[attribute]])
  }
  out
}


get_external_vars <- function(series_index, date_index){
  state <- as.character(state_info[series_index, "state_id"])
  category <- as.character(state_info[series_index, "cat_id"])
  current_date_info <- date_info[date_index,]
  month <- as.numeric(current_date_info$month)
  price <- as.numeric(price_info[series_index, date_index])
  snap <- as.numeric(current_date_info[[paste0("snap_", state)]])
  event_name_1 <- fill_vec(current_date_info, "event_name_1", "no_event_name_1")
  event_name_2 <- fill_vec(current_date_info, "event_name_2", "no_event_name_2")
  event_type_1 <- fill_vec(current_date_info, "event_type_1", "no_event_type_1")
  event_type_2 <- fill_vec(current_date_info, "event_type_2", "no_event_type_2")
  
  if(as.character(current_date_info$weekday) %in% c("Saturday", "Sunday")){
    weekday <- 0
  }else{
    weekday <- 1
  }
  
  list("state"=state, 
       "category"=category, 
       "month"=month, 
       "price"=price, 
       "snap"=snap, 
       "weekday"=weekday, 
       "event_name_1"=event_name_1, 
       "event_name_2"=event_name_2, 
       "event_type_1"=event_type_1, 
       "event_type_2"=event_type_2
       )
}

#Preprocessing
for (i in 1:nrow(dataset)) {  
  time_series <- as.numeric(dataset[i,])
  
  #mean <- mean(time_series)
  #series_means <- c(series_means, mean)
  #time_series <- time_series / mean
 
  embedded <- as.data.frame(embed(time_series, lag + 1))
  
  #add other variables
  states_vec <- c()
  categories_vec <- c()
  price_vec <- c()
  weekday_vec <- c()
  month_vec <- c()
  snap_vec <- c()
  event_name_1_vec <- c()
  event_name_2_vec <- c()
  event_type_1_vec <- c()
  event_type_2_vec <- c()
    
  #add other variables
  for(j in (lag+1):length(time_series)){
    external_vars <- get_external_vars(i, j)
    states_vec <- c(states_vec, external_vars$state)
    categories_vec <- c(categories_vec, external_vars$category)
    month_vec <- c(month_vec, external_vars$month)
    price_vec <- c(price_vec, external_vars$price)
    snap_vec <- c(snap_vec, external_vars$snap)
    event_name_1_vec <- c(event_name_1_vec, external_vars$event_name_1)
    event_name_2_vec <- c(event_name_2_vec, external_vars$event_name_2)
    event_type_1_vec <- c(event_type_1_vec, external_vars$event_type_1)
    event_type_2_vec <- c(event_type_2_vec, external_vars$event_type_2)
    weekday_vec <- c(weekday_vec, external_vars$weekday)
  }
  
  embedded <- cbind(embedded,
                    price_vec, 
                    weekday_vec, 
                    month_vec, 
                    snap_vec,
                    states_vec,
                    categories_vec,
                    event_name_1_vec, 
                    event_name_2_vec, 
                    event_type_1_vec, 
                    event_type_2_vec
                    )
  
  if (!is.null(embedded_series)) {
    embedded_series <- as.data.frame(embedded_series)
  }
  embedded_series <- rbind(embedded_series, embedded)
  
  if (!is.null(final_lags)) {
    final_lags <- as.data.frame(final_lags)
  }

  current_series_final_lags <- t(as.matrix(rev(tail(time_series, lag))))
  final_external_vars <- get_external_vars(i, (length(time_series)+1))
 
  current_series_final_lags <- cbind(current_series_final_lags,
                                     final_external_vars$price, 
                                     final_external_vars$weekday, 
                                     final_external_vars$month, 
                                     final_external_vars$snap,
                                     final_external_vars$state,
                                     final_external_vars$category,
                                     final_external_vars$event_name_1, 
                                     final_external_vars$event_name_2, 
                                     final_external_vars$event_type_1, 
                                     final_external_vars$event_type_2
                                    )
    
  final_lags <- rbind(final_lags, as.data.frame(current_series_final_lags))
}


fitting_data <- as.data.frame(embedded_series)
colnames(fitting_data)[1] <- "y"
colnames(fitting_data)[2:(lag+1)] <- paste("Lag", 1:lag, sep = "")
indx <- c((lag+3):(lag+5))
fitting_data[indx] <- lapply(fitting_data[indx], function(x) as.factor(x))

final_lags <- as.data.frame(final_lags)
colnames(final_lags)[1:lag] <- paste("Lag", 1:lag, sep = "")
colnames(final_lags)[(lag+1):ncol(final_lags)] <- colnames(fitting_data)[(lag+2): ncol(fitting_data)]
indx <- c(1:(lag+1))
final_lags[indx] <- lapply(final_lags[indx], function(x) as.numeric(as.character(x)))


#define count and zero model formulas
count_forumula <- "y ~ "
zero_formula <- ""

#considered all exterbal variables and lags with count model
for(cn in 2:ncol(fitting_data)){
  if(cn != ncol(fitting_data)){
    count_forumula <- paste0(count_forumula, colnames(fitting_data)[cn], " + ")
  }else{
    count_forumula <- paste0(count_forumula, colnames(fitting_data)[cn])
  }
}

for(cn in 1:lag){
  zero_formula <- paste0(zero_formula, "Lag", cn, " + ")
}

#considered only 5 external variables and lags with zero model
zero_formula <- paste0(zero_formula, "price_vec + weekday_vec + snap_vec + event_name_1_vec + event_name_2_vec") 


if(TECHNIQUE=="poisson"){
  #fit a poisson model
  model <- glm(as.formula(count_forumula), data=fitting_data,  family=poisson)
}else{
  # fit a zero-inflated model
  model <- zeroinfl(as.formula(paste0(count_forumula, "|", zero_formula)), data=fitting_data)
}

#forecast one horizon at a time recursively
predictions <- NULL

for (h in 1:horizon){
  new_predictions <- predict(object=model, newdata = final_lags)  
  predictions <- cbind(predictions, new_predictions)
  
  # update the final lags
  final_lags <- final_lags[-lag]
  if(dim(final_lags)[2] == 1){
    final_lags <- t(final_lags)
  }
  
  final_lags <- cbind(new_predictions, final_lags)
  colnames(final_lags)[1:lag] <- paste("Lag", 1:lag, sep="")
  
  for(col in (lag+2):ncol(final_lags)){
    final_lags[col] <- lapply(final_lags[col], as.character)[[1]]
  }
  
  #update external variables for the next time point
  for(series in 1:nrow(final_lags)){
    next_external_var <- get_external_vars(series, (ncol(dataset)+h+1))
    final_lags[series, "price_vec"] <- next_external_var$price
    final_lags[series, "weekday_vec"] <- next_external_var$weekday
    final_lags[series, "month_vec"] <- next_external_var$month
    final_lags[series, "snap_vec"] <- next_external_var$snap
    final_lags[series, "event_name_1_vec"] <- next_external_var$event_name_1
    final_lags[series, "event_name_2_vec"] <- next_external_var$event_name_2
    final_lags[series, "event_type_1_vec"] <- next_external_var$event_type_1
    final_lags[series, "event_type_2_vec"] <- next_external_var$event_type_2
  }
  
  for(col in (lag+2):ncol(final_lags)){
    final_lags[col] <- lapply(final_lags[col], as.factor)[[1]]
  }
  
  final_lags <- as.data.frame(final_lags)
}

# renormalize the predictions
#true_predictions <- predictions * as.vector(series_means)
true_predictions <- predictions

# write forecasts to file
write.table(true_predictions, file=output_file_name, row.names = F, col.names=F, sep=",", quote=F)
