library(tidyverse)

#### Set wd to the source file location

load("data-frames/weights_all_evaluation.rda")
load("data-frames/Test_evaluation.rda")
load("data-frames/Train_all_evaluation.rda")

###-- Uncomment when using full training period --###
# load("data-frames/weights_all_evaluation_full.rda")
# load("data-frames/Test_evaluation.rda")
# load("data-frames/Train_all_evaluation_full.rda")

###-- Uncomment when using validation period --###
# load("data-frames/weights_all_validation.rda")
# load("data-frames/Test_validation.rda")
# load("data-frames/Train_all_validation.rda")




# Forecasts <- read_csv("data-frames/Prophet_forecast_external-complex.csv")
# 
# forecasts <- Forecasts %>%
#   slice(1:30490)



############################################
        ####--Score function--####
############################################

Diff_func <- function(x){
  x <- na.omit(x)
  d <- diff(x, lag = 1)
  return((t(d)%*%d)/(length(x)-1))
}


WRMSSE_func <- function(forecasts){
  
  h <- 28
  forecasts <- as_tibble(forecasts) %>% 
    mutate(id = gsub(paste(c("_validation", "_evaluation"), collapse = '|'), "", id))
  actual <- as_tibble(actual) %>% 
    mutate(id = gsub(paste(c("_validation", "_evaluation"), collapse = '|'), "", id))
  training_set <- as_tibble(training_set) %>% 
    mutate(name = gsub(paste(c("_validation", "_evaluation"), collapse = '|'), "", name))
  weights <- as_tibble(weights) %>% 
    mutate(name = gsub(paste(c("_validation", "_evaluation"), collapse = '|'), "", name))
  
  ###-- Preparing forecasts --###
  fc_days <- paste("F", c(1:28), sep = "")
  colnames(forecasts) <- c("id", fc_days)
  
  group <- actual %>% 
    select(id, item_id, dept_id, cat_id, store_id, state_id)
  forecasts <- group %>% 
    left_join(forecasts, by = "id")
  
  forecasts <- forecasts %>% 
    gather(-id, -item_id, -dept_id, -cat_id, -store_id, -state_id, 
           key = h, value = sales_fc)
  forecasts <- forecasts %>% 
    mutate(id = factor(id),
           item_id = factor(item_id),
           dept_id = factor(dept_id),
           cat_id = factor(cat_id),
           store_id = factor(store_id),
           state_id = factor(state_id),
           h = factor(h, levels = fc_days))
  
  Fc_all <- matrix(0, 1, 29) %>% as.data.frame()
  colnames(Fc_all) <- c("name", fc_days)
  
  Fc_all <- Fc_all %>% 
    mutate(name = as.character(name)) %>% 
    filter(name != 0) %>% as_tibble() 
  
  Fc_all <- Fc_all %>% 
    bind_rows(forecasts %>% group_by(h) %>% summarise(fc = sum(sales_fc)) %>% 
                mutate(name = "Total") %>%  spread(key = h, value = fc),
              forecasts %>% group_by(state_id, h) %>% summarise(fc = sum(sales_fc)) %>%  
                spread(key = h, value = fc) %>% rename(name = state_id),
              forecasts %>% group_by(store_id, h) %>% summarise(fc = sum(sales_fc)) %>%  
                spread(key = h, value = fc) %>% rename(name = store_id),
              forecasts %>% group_by(cat_id, h) %>% summarise(fc = sum(sales_fc)) %>%  
                spread(key = h, value = fc) %>% rename(name = cat_id),
              forecasts %>% group_by(dept_id, h) %>% summarise(fc = sum(sales_fc)) %>%  
                spread(key = h, value = fc) %>% rename(name = dept_id),
              forecasts %>% group_by(state_id, cat_id, h) %>% summarise(fc = sum(sales_fc)) %>%  
                spread(key = h, value = fc) %>% unite("name", state_id, cat_id, sep = "_", 
                                                      remove = T),
              forecasts %>% group_by(state_id, dept_id, h) %>% summarise(fc = sum(sales_fc)) %>%  
                spread(key = h, value = fc) %>% unite("name", state_id, dept_id, sep = "_", 
                                                      remove = T),
              forecasts %>% group_by(store_id, cat_id, h) %>% summarise(fc = sum(sales_fc)) %>%  
                spread(key = h, value = fc) %>% unite("name", store_id, cat_id, sep = "_", 
                                                      remove = T),
              forecasts %>% group_by(store_id, dept_id, h) %>% summarise(fc = sum(sales_fc)) %>%  
                spread(key = h, value = fc) %>% unite("name", store_id, dept_id, sep = "_", 
                                                      remove = T),
              forecasts %>% group_by(item_id, h) %>% summarise(fc = sum(sales_fc)) %>%  
                spread(key = h, value = fc) %>% unite("name", item_id, sep = "_", 
                                                      remove = T), 
              forecasts %>% group_by(state_id, item_id, h) %>% summarise(fc = sum(sales_fc)) %>%  
                spread(key = h, value = fc) %>% unite("name", state_id, item_id, sep = "_", 
                                                      remove = T),
              forecasts %>% group_by(id, h) %>% summarise(fc = sum(sales_fc)) %>%  
                spread(key = h, value = fc) %>% rename(name = "id"))
  
  
  ###-- Preparing actual sales --###
  
  Actual_all <- matrix(0, 1, 29) %>% as.data.frame()
  test_days <- tail(names(actual), -6)
  colnames(Actual_all) <- c("name", test_days)
  
  Actual_all <- Actual_all %>% 
    mutate(name = as.character(name)) %>% 
    filter(name != 0) %>% as_tibble() 
  
  actual <- actual %>% 
    select(id, item_id, dept_id, cat_id, store_id, state_id, all_of(test_days)) %>% 
    gather(-id, -item_id, -dept_id, -cat_id, -store_id, -state_id, 
           key = d, value = sales)
  actual <- actual %>% 
    mutate(id = factor(id),
           item_id = factor(item_id),
           dept_id = factor(dept_id),
           cat_id = factor(cat_id),
           store_id = factor(store_id),
           state_id = factor(state_id),
           d = factor(d, levels = test_days))
  
  
  Actual_all <- Actual_all %>% 
    bind_rows(actual %>% group_by(d) %>% summarise(sales = sum(sales, na.rm = T)) %>% 
                mutate(name = "Total") %>%  spread(key = d, value = sales),
              actual %>% group_by(state_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
                spread(key = d, value = sales) %>% rename(name = state_id),
              actual %>% group_by(store_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
                spread(key = d, value = sales) %>% rename(name = store_id),
              actual %>% group_by(cat_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
                spread(key = d, value = sales) %>% rename(name = cat_id),
              actual %>% group_by(dept_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
                spread(key = d, value = sales) %>% rename(name = dept_id),
              actual %>% group_by(state_id, cat_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
                spread(key = d, value = sales) %>% unite("name", state_id, cat_id, sep = "_", 
                                                         remove = T),
              actual %>% group_by(state_id, dept_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
                spread(key = d, value = sales) %>% unite("name", state_id, dept_id, sep = "_", 
                                                         remove = T),
              actual %>% group_by(store_id, cat_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
                spread(key = d, value = sales) %>% unite("name", store_id, cat_id, sep = "_", 
                                                         remove = T),
              actual %>% group_by(store_id, dept_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
                spread(key = d, value = sales) %>% unite("name", store_id, dept_id, sep = "_", 
                                                         remove = T),
              actual %>% group_by(item_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
                spread(key = d, value = sales) %>% unite("name", item_id, sep = "_", 
                                                         remove = T), 
              actual %>% group_by(state_id, item_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
                spread(key = d, value = sales) %>% unite("name", state_id, item_id, sep = "_", 
                                                         remove = T),
              actual %>% group_by(id, d) %>% summarise(sales = sum(sales)) %>%  
                spread(key = d, value = sales) %>% rename(name = "id"))
  
  
  ###-- Calculating scales from training set --###
  
  Scale <- apply(training_set %>% select(-name), 1, Diff_func)
  
  RMSSE_denom <- tibble(name = training_set %>% pull(name),
                        scale = Scale) 
  
  
  # Binding True sales and forecasts for the test period
  
  colnames(Fc_all) <- c("name", 1:h)
  Fc_all <- Fc_all %>% 
    gather(-name, key = h, value = sales_fc) 
  
  
  colnames(Actual_all) <- c("name", 1:h)
  Actual_all <- Actual_all %>% 
    gather(-name, key = h, value = sales) 
  
  Fc_all <- Fc_all %>% 
    left_join(Actual_all, by = c("name", "h"))
  
  Summary_mse <- Fc_all %>% 
    mutate(SE = (sales - sales_fc)^2) %>% 
    group_by(name) %>% 
    summarise(MSE = mean(SE))
  
  Summary_mse <- RMSSE_denom %>% 
    left_join(Summary_mse, by = "name")
  
  Summary_mse <- Summary_mse %>% 
    mutate(RMSSE = sqrt(MSE/scale))
  
  Summary_mse <- Summary_mse %>% 
    left_join(weights, by = "name") 
  
  WRMSSE <- Summary_mse %>% 
    mutate(WRMSSE = RMSSE*weights) %>% 
    pull(WRMSSE) %>% 
    sum()
  
  return(WRMSSE)
  
}




start <- Sys.time()
suppressWarnings(WRMSSE_func(forecasts = forecasts))
end <- Sys.time()










