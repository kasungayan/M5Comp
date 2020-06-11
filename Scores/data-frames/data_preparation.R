library(tidyverse)

sales_train_validation <- read_csv("~/Desktop/M5Comp/Decomp_mod/data/sales_train_validation.csv")
sales_train_valid_new <- read_csv("~/Desktop/M5Comp/dataset/comp_data/sales_train_valid_new.csv")
sales_train_evaluation <- read_csv("~/Desktop/M5Comp/dataset/comp_data/sales_train_evaluation.csv")
Eval <- read_csv("~/Desktop/M5Comp/Reconciliation/Forecasts/m5_clustering_70_normalized_with_external_vars_pooled_regression_400_forecasts.csv")

Eval <- Eval %>% 
  slice(1:30490)

group <- sales_train_validation %>% 
  select(id, item_id, dept_id, cat_id, store_id, state_id)

Eval <- group %>% 
  left_join(Eval, by = "id")

Eval <- Eval %>% 
  gather(-id, -item_id, -dept_id, -cat_id, -store_id, -state_id, 
         key = h, value = sales_fc)
Eval <- Eval %>% 
  mutate(id = factor(id),
         item_id = factor(item_id),
         dept_id = factor(dept_id),
         cat_id = factor(cat_id),
         store_id = factor(store_id),
         state_id = factor(state_id),
         h = factor(h, levels = c("F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", 
                                  "F12", "F13", "F14","F15", "F16", "F17", "F18", "F19", "F20", "F21", 
                                  "F22", "F23", "F24","F25", "F26", "F27", "F28")))

Eval_all <- tibble(name = character(),
                   F1 = numeric(), F2 = numeric(), F3 = numeric(), F4 = numeric(),
                   F5 = numeric(), F6 = numeric(), F7 = numeric(), F8 = numeric(),
                   F9 = numeric(), F10 = numeric(), F11 = numeric(), F12 = numeric(),
                   F13 = numeric(), F14 = numeric(), F15 = numeric(), F16 = numeric(),
                   F17 = numeric(), F18 = numeric(), F19 = numeric(), F20 = numeric(),
                   F21 = numeric(), F22 = numeric(), F23 = numeric(), F24 = numeric(),
                   F25 = numeric(), F26 = numeric(), F27 = numeric(), F28 = numeric())

Eval_all <- Eval_all %>% 
  bind_rows(Eval %>% group_by(h) %>% summarise(fc = sum(sales_fc)) %>% 
              mutate(name = "Total") %>%  spread(key = h, value = fc),
            Eval %>% group_by(state_id, h) %>% summarise(fc = sum(sales_fc)) %>%  
              spread(key = h, value = fc) %>% rename(name = state_id),
            Eval %>% group_by(store_id, h) %>% summarise(fc = sum(sales_fc)) %>%  
              spread(key = h, value = fc) %>% rename(name = store_id),
            Eval %>% group_by(cat_id, h) %>% summarise(fc = sum(sales_fc)) %>%  
              spread(key = h, value = fc) %>% rename(name = cat_id),
            Eval %>% group_by(dept_id, h) %>% summarise(fc = sum(sales_fc)) %>%  
              spread(key = h, value = fc) %>% rename(name = dept_id),
            Eval %>% group_by(state_id, cat_id, h) %>% summarise(fc = sum(sales_fc)) %>%  
              spread(key = h, value = fc) %>% unite("name", state_id, cat_id, sep = "_", 
                                                    remove = T),
            Eval %>% group_by(state_id, dept_id, h) %>% summarise(fc = sum(sales_fc)) %>%  
              spread(key = h, value = fc) %>% unite("name", state_id, dept_id, sep = "_", 
                                                    remove = T),
            Eval %>% group_by(store_id, cat_id, h) %>% summarise(fc = sum(sales_fc)) %>%  
              spread(key = h, value = fc) %>% unite("name", store_id, cat_id, sep = "_", 
                                                    remove = T),
            Eval %>% group_by(store_id, dept_id, h) %>% summarise(fc = sum(sales_fc)) %>%  
              spread(key = h, value = fc) %>% unite("name", store_id, dept_id, sep = "_", 
                                                    remove = T),
            Eval %>% group_by(item_id, h) %>% summarise(fc = sum(sales_fc)) %>%  
              spread(key = h, value = fc) %>% unite("name", item_id, sep = "_", 
                                                    remove = T), 
            Eval %>% group_by(state_id, item_id, h) %>% summarise(fc = sum(sales_fc)) %>%  
              spread(key = h, value = fc) %>% unite("name", state_id, item_id, sep = "_", 
                                                    remove = T),
            Eval %>% group_by(id, h) %>% summarise(fc = sum(sales_fc)) %>%  
              spread(key = h, value = fc) %>% rename(name = "id"))



## Train all validation

Train_all <- matrix(0, 1, 1914) %>% as.data.frame()
colnames(Train_all) <- c("name", colnames(sales_train_valid_new)[-(1:6)])

Train_all <- Train_all %>% 
  mutate(name = as.character(name)) %>% 
  filter(name != 0) %>% as_tibble() 

Train <- sales_train_valid_new %>% 
  gather(-id, -item_id, -dept_id, -cat_id, -store_id, -state_id, 
         key = d, value = sales)
Train <- Train %>% 
  mutate(id = factor(id),
         item_id = factor(item_id),
         dept_id = factor(dept_id),
         cat_id = factor(cat_id),
         store_id = factor(store_id),
         state_id = factor(state_id),
         d = factor(d, levels = colnames(sales_train_valid_new)[-(1:6)]))


Train_all <- Train_all %>% 
  bind_rows(Train %>% group_by(d) %>% summarise(sales = sum(sales, na.rm = T)) %>% 
              mutate(name = "Total") %>%  spread(key = d, value = sales),
            Train %>% group_by(state_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% rename(name = state_id),
            Train %>% group_by(store_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% rename(name = store_id),
            Train %>% group_by(cat_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% rename(name = cat_id),
            Train %>% group_by(dept_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% rename(name = dept_id),
            Train %>% group_by(state_id, cat_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% unite("name", state_id, cat_id, sep = "_", 
                                                       remove = T),
            Train %>% group_by(state_id, dept_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% unite("name", state_id, dept_id, sep = "_", 
                                                       remove = T),
            Train %>% group_by(store_id, cat_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% unite("name", store_id, cat_id, sep = "_", 
                                                       remove = T),
            Train %>% group_by(store_id, dept_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% unite("name", store_id, dept_id, sep = "_", 
                                                       remove = T),
            Train %>% group_by(item_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% unite("name", item_id, sep = "_", 
                                                       remove = T), 
            Train %>% group_by(state_id, item_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% unite("name", state_id, item_id, sep = "_", 
                                                       remove = T),
            sales_train_valid_new %>% select(-item_id, -dept_id, -cat_id, -store_id, -state_id) %>% 
              rename(name = "id"))


write_csv(Train_all, path = "Train_all_validation.csv")


# Train_evaluation

sell_prices <- read_csv("~/Desktop/M5Comp/dataset/comp_data/sell_prices_eval.csv")
calendar <- read_csv("~/Desktop/M5Comp/dataset/comp_data/comp_data/calendar.csv")

date_week <- calendar %>% 
  select(date, wm_yr_wk, d)

sell_prices_new <- sell_prices 

sell_prices_new <- date_week %>% 
  left_join(sell_prices_new, by = "wm_yr_wk") %>% 
  rename("day" = "d")

data_full_train <- sales_train_evaluation
data_full_train <- data_full_train %>% 
  gather(-id, -item_id, -dept_id, -cat_id, -store_id, -state_id, key = day, value = sales) 

data_full_train <- data_full_train %>% 
  left_join(sell_prices_new, by = c("day", "store_id", "item_id")) %>% 
  select(-date, -wm_yr_wk)


data_full_train <- data_full_train %>% 
  mutate(sales = if_else(is.na(sell_price), sell_price, sales))

sales_Train_eval <- data_full_train %>% 
  select(-sell_price) %>% 
  spread(key = day, value = sales)

Train_eval <- data_full_train %>% 
  select(-sell_price) %>% 
  rename("d" = day)

train_eval_days <- unique(data_full_train$day)

Train_eval <- Train_eval %>% 
  mutate(id = factor(id),
         item_id = factor(item_id),
         dept_id = factor(dept_id),
         cat_id = factor(cat_id),
         store_id = factor(store_id),
         state_id = factor(state_id),
         d = factor(d, levels = train_eval_days))


Train_all_eval <- matrix(0, 1, 1942) %>% as.data.frame()
colnames(Train_all_eval) <- c("name", train_eval_days)

Train_all_eval <- Train_all_eval %>% 
  mutate(name = as.character(name)) %>% 
  filter(name != 0) %>% as_tibble() 

Train_all_eval <- Train_all_eval %>% 
  bind_rows(Train_eval %>% group_by(d) %>% summarise(sales = sum(sales, na.rm = T)) %>% 
              mutate(name = "Total") %>%  spread(key = d, value = sales),
            Train_eval %>% group_by(state_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% rename(name = state_id),
            Train_eval %>% group_by(store_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% rename(name = store_id),
            Train_eval %>% group_by(cat_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% rename(name = cat_id),
            Train_eval %>% group_by(dept_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% rename(name = dept_id),
            Train_eval %>% group_by(state_id, cat_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% unite("name", state_id, cat_id, sep = "_", 
                                                       remove = T),
            Train_eval %>% group_by(state_id, dept_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% unite("name", state_id, dept_id, sep = "_", 
                                                       remove = T),
            Train_eval %>% group_by(store_id, cat_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% unite("name", store_id, cat_id, sep = "_", 
                                                       remove = T),
            Train_eval %>% group_by(store_id, dept_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% unite("name", store_id, dept_id, sep = "_", 
                                                       remove = T),
            Train_eval %>% group_by(item_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% unite("name", item_id, sep = "_", 
                                                       remove = T), 
            Train_eval %>% group_by(state_id, item_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% unite("name", state_id, item_id, sep = "_", 
                                                       remove = T),
            sales_Train_eval %>% select(-item_id, -dept_id, -cat_id, -store_id, -state_id) %>% 
              rename(name = "id"))


write_csv(Train_all_eval, path = "Train_all_evaluation.csv")

level <- c(1, rep(2, 3), rep(3, 10), rep(4, 3), rep(5, 7), rep(6, 9), rep(7, 21),
           rep(8, 30), rep(9, 70), rep(10, 3049), rep(11, 9147), rep(12, 30490))
day_new <- alendar %>% filter(date < ymd("2015-05-23")) %>% pull(d)

Train_all_evaluation <- Train_all_evaluation %>% 
  mutate(level_id = c(1, rep(2, 3), rep(3, 10), rep(4, 3), rep(5, 7), rep(6, 9), rep(7, 21),
                    rep(8, 30), rep(9, 70), rep(10, 3049), rep(11, 9147), rep(12, 30490))) %>% 
  select(level_id, name, all_of(day_new))

write_csv(Train_all_evaluation, path = "Train_all_evaluation.csv")


# Test set
Test_all <- matrix(0, 1, 29) %>% as.data.frame()
test_days <- paste("d", c(1914:1941), sep = "_")
colnames(Test_all) <- c("name", test_days)

Test_all <- Test_all %>% 
  mutate(name = as.character(name)) %>% 
  filter(name != 0) %>% as_tibble() 

Test <- sales_train_evaluation %>% 
  select(id, item_id, dept_id, cat_id, store_id, state_id, all_of(test_days)) %>% 
  gather(-id, -item_id, -dept_id, -cat_id, -store_id, -state_id, 
         key = d, value = sales)
Test <- Test %>% 
  mutate(id = factor(id),
         item_id = factor(item_id),
         dept_id = factor(dept_id),
         cat_id = factor(cat_id),
         store_id = factor(store_id),
         state_id = factor(state_id),
         d = factor(d, levels = test_days))


Test_all <- Test_all %>% 
  bind_rows(Test %>% group_by(d) %>% summarise(sales = sum(sales, na.rm = T)) %>% 
              mutate(name = "Total") %>%  spread(key = d, value = sales),
            Test %>% group_by(state_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% rename(name = state_id),
            Test %>% group_by(store_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% rename(name = store_id),
            Test %>% group_by(cat_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% rename(name = cat_id),
            Test %>% group_by(dept_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% rename(name = dept_id),
            Test %>% group_by(state_id, cat_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% unite("name", state_id, cat_id, sep = "_", 
                                                       remove = T),
            Test %>% group_by(state_id, dept_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% unite("name", state_id, dept_id, sep = "_", 
                                                       remove = T),
            Test %>% group_by(store_id, cat_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% unite("name", store_id, cat_id, sep = "_", 
                                                       remove = T),
            Test %>% group_by(store_id, dept_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% unite("name", store_id, dept_id, sep = "_", 
                                                       remove = T),
            Test %>% group_by(item_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% unite("name", item_id, sep = "_", 
                                                       remove = T), 
            Test %>% group_by(state_id, item_id, d) %>% summarise(sales = sum(sales, na.rm = T)) %>%  
              spread(key = d, value = sales) %>% unite("name", state_id, item_id, sep = "_", 
                                                       remove = T),
            Test %>% group_by(id, d) %>% summarise(sales = sum(sales)) %>%  
              spread(key = d, value = sales) %>% rename(name = "id"))


Diff_func <- function(x){
  x <- na.omit(x)
  d <- diff(x, lag = 1)
  return((t(d)%*%d)/(length(x)-1))
}


X <- apply(Train_all %>% select(-name), 1, Diff_func)

RMSSE_denom <- tibble(name = Train_all %>% pull(name),
                      scale = X) %>% 
  mutate(name = gsub("_validation", "", name))

# Binding True sales and fprecasts for the test period

colnames(Eval_all) <- c("name", 1:28)
Eval_all <- Eval_all %>% 
  gather(-name, key = h, value = sales_fc) %>% 
  mutate(name = gsub("_validation", "", name))


colnames(Test_all) <- c("name", 1:28)
Test_all <- Test_all %>% 
  gather(-name, key = h, value = sales) %>% 
  mutate(name = gsub("_evaluation", "", name))

Eval_all <- Eval_all %>% 
  left_join(Test_all)

Eval_all <- Eval_all %>% 
  mutate(SE = (sales - sales_fc)^2)
  
Summary_mse <- Eval_all %>% 
  group_by(name) %>% 
  summarise(MSE = mean(SE))
  
Summary_mse <- RMSSE_denom %>% 
  left_join(Summary_mse)
  
Summary_mse <- Summary_mse %>% 
  mutate(RMSSE = sqrt(MSE/scale))

Summary_mse <- Summary_mse %>% 
  left_join(df_weights_all, by = "name") 

Summary_mse %>% 
  mutate(WRMSSE = RMSSE*weights) %>% 
  pull(WRMSSE) %>% 
  sum()



# Calculating weights - validation

sell_prices <- read_csv("~/Desktop/M5Comp/dataset/comp_data/comp_data/sell_prices.csv")
calendar <- read_csv("~/Desktop/M5Comp/dataset/comp_data/comp_data/calendar.csv")
  
date_week <- calendar %>% 
  select(date,wm_yr_wk, d)

sell_prices_new <- sell_prices 

sell_prices_new <- date_week %>% 
  left_join(sell_prices_new, by = "wm_yr_wk") %>% 
  rename("day" = "d")

data_full_train <- sales_train_validation
data_full_train <- data_full_train %>% 
  gather(-id, -item_id, -dept_id, -cat_id, -store_id, -state_id, key = day, value = sales) 

data_full_train <- data_full_train %>% 
  left_join(sell_prices_new, by = c("day", "store_id", "item_id")) %>% 
  select(-date, -wm_yr_wk)


train_days <- paste("d", c(1:1913), sep = "_")

data_full_train <- data_full_train %>% 
  filter(day %in% tail(train_days, 28)) %>% 
  mutate(dd_sales = sales*sell_price)

df_weights <- data_full_train %>% 
  select(-sales, -sell_price) %>% 
  group_by(id, item_id, dept_id, cat_id, store_id, state_id) %>% 
  summarise(dd_sales = sum(dd_sales)) %>% 
  ungroup()

dd_sales_sum <- sum(df_weights %>% pull(dd_sales))

df_weights_all <- tibble(name = character(),
                         weights = numeric())

df_weights_all <- df_weights_all %>% 
  bind_rows(data_frame(name = "Total", weights = 1),
            df_weights %>% group_by(state_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              rename(name = state_id),
            df_weights %>% group_by(store_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              rename(name = store_id),
            df_weights %>% group_by(cat_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              rename(name = cat_id),
            df_weights %>% group_by(dept_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              rename(name = dept_id),
            df_weights %>% group_by(state_id, cat_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              unite("name", state_id, cat_id, sep = "_", remove = T),
            df_weights %>% group_by(state_id, dept_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              unite("name", state_id, dept_id, sep = "_", remove = T),
            df_weights %>% group_by(store_id, cat_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              unite("name", store_id, cat_id, sep = "_", remove = T),
            df_weights %>% group_by(store_id, dept_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              unite("name", store_id, dept_id, sep = "_", remove = T),
            df_weights %>% group_by(item_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              rename(name = item_id),
            df_weights %>% group_by(state_id, item_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              unite("name", state_id, item_id, sep = "_", remove = T),
            df_weights %>% group_by(id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              rename(name = id))

df_weights_all <- df_weights_all %>% 
  mutate(weights = weights/12) %>% 
  mutate(name = gsub("_validation", "", name))

write_csv(df_weights_all, "weights_all_validation.csv")



# Calculating weights - evaluation (for whole period)

sell_prices <- read_csv("~/Desktop/M5Comp/dataset/comp_data/sell_prices_eval.csv")
calendar <- read_csv("~/Desktop/M5Comp/dataset/comp_data/comp_data/calendar.csv")

date_week <- calendar %>% 
  select(date, wm_yr_wk, d)

sell_prices_new <- sell_prices 

sell_prices_new <- date_week %>% 
  left_join(sell_prices_new, by = "wm_yr_wk") %>% 
  rename("day" = "d")

data_full_train <- sales_train_evaluation
data_full_train <- data_full_train %>% 
  gather(-id, -item_id, -dept_id, -cat_id, -store_id, -state_id, key = day, value = sales) 

data_full_train <- data_full_train %>% 
  left_join(sell_prices_new, by = c("day", "store_id", "item_id")) %>% 
  select(-date, -wm_yr_wk)


train_days <- paste("d", c(1:1941), sep = "_")

data_full_train <- data_full_train %>% 
  filter(day %in% tail(train_days, 28)) %>% 
  mutate(dd_sales = sales*sell_price)

df_weights <- data_full_train %>% 
  select(-sales, -sell_price) %>% 
  group_by(id, item_id, dept_id, cat_id, store_id, state_id) %>% 
  summarise(dd_sales = sum(dd_sales)) %>% 
  ungroup()

dd_sales_sum <- sum(df_weights %>% pull(dd_sales))

df_weights_all <- tibble(name = character(),
                         weights = numeric())

df_weights_all <- df_weights_all %>% 
  bind_rows(data_frame(name = "Total", weights = 1),
            df_weights %>% group_by(state_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              rename(name = state_id),
            df_weights %>% group_by(store_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              rename(name = store_id),
            df_weights %>% group_by(cat_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              rename(name = cat_id),
            df_weights %>% group_by(dept_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              rename(name = dept_id),
            df_weights %>% group_by(state_id, cat_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              unite("name", state_id, cat_id, sep = "_", remove = T),
            df_weights %>% group_by(state_id, dept_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              unite("name", state_id, dept_id, sep = "_", remove = T),
            df_weights %>% group_by(store_id, cat_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              unite("name", store_id, cat_id, sep = "_", remove = T),
            df_weights %>% group_by(store_id, dept_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              unite("name", store_id, dept_id, sep = "_", remove = T),
            df_weights %>% group_by(item_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              rename(name = item_id),
            df_weights %>% group_by(state_id, item_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              unite("name", state_id, item_id, sep = "_", remove = T),
            df_weights %>% group_by(id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              rename(name = id))

df_weights_all <- df_weights_all %>% 
  mutate(weights = weights/12) %>% 
  mutate(name = gsub("_validation", "", name))

write_csv(df_weights_all, "weights_all_evaluation.csv")





# Calculating weights - evaluation (until 2015-05-22)

sell_prices <- read_csv("~/Desktop/M5Comp/dataset/comp_data/sell_prices_eval.csv")
calendar <- read_csv("~/Desktop/M5Comp/dataset/comp_data/comp_data/calendar.csv")

date_week <- calendar %>% 
  select(date, wm_yr_wk, d)

sell_prices_new <- sell_prices 

sell_prices_new <- date_week %>% 
  left_join(sell_prices_new, by = "wm_yr_wk") %>% 
  rename("day" = "d")

data_full_train <- sales_train_evaluation
data_full_train <- data_full_train %>% 
  gather(-id, -item_id, -dept_id, -cat_id, -store_id, -state_id, key = day, value = sales) 

data_full_train <- data_full_train %>% 
  left_join(sell_prices_new, by = c("day", "store_id", "item_id")) %>% 
  select(-date, -wm_yr_wk)


train_days <- paste("d", c(1:1575), sep = "_")

data_full_train <- data_full_train %>% 
  filter(day %in% tail(train_days, 28)) %>% 
  mutate(dd_sales = sales*sell_price)

df_weights <- data_full_train %>% 
  select(-sales, -sell_price) %>% 
  group_by(id, item_id, dept_id, cat_id, store_id, state_id) %>% 
  summarise(dd_sales = sum(dd_sales, na.rm = T)) %>% 
  ungroup()

dd_sales_sum <- sum(df_weights %>% pull(dd_sales))

df_weights_all <- tibble(name = character(),
                         weights = numeric())

df_weights_all <- df_weights_all %>% 
  bind_rows(data_frame(name = "Total", weights = 1),
            df_weights %>% group_by(state_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              rename(name = state_id),
            df_weights %>% group_by(store_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              rename(name = store_id),
            df_weights %>% group_by(cat_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              rename(name = cat_id),
            df_weights %>% group_by(dept_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              rename(name = dept_id),
            df_weights %>% group_by(state_id, cat_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              unite("name", state_id, cat_id, sep = "_", remove = T),
            df_weights %>% group_by(state_id, dept_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              unite("name", state_id, dept_id, sep = "_", remove = T),
            df_weights %>% group_by(store_id, cat_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              unite("name", store_id, cat_id, sep = "_", remove = T),
            df_weights %>% group_by(store_id, dept_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              unite("name", store_id, dept_id, sep = "_", remove = T),
            df_weights %>% group_by(item_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              rename(name = item_id),
            df_weights %>% group_by(state_id, item_id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              unite("name", state_id, item_id, sep = "_", remove = T),
            df_weights %>% group_by(id) %>% summarise(weights = sum(dd_sales)/dd_sales_sum) %>% 
              rename(name = id))

df_weights_all <- df_weights_all %>% 
  mutate(weights = weights/12) %>% 
  mutate(name = gsub("_validation", "", name))

write_csv(df_weights_all, "weights_all_evaluation.csv")







# Test_validation

Test_valid <- sales_train_evaluation %>% 
  gather(-id, -item_id, -dept_id, -cat_id, -store_id, -state_id, key = day, value = sales) %>% 
  filter(day %in% tail(train_days, 28))

Test_valid <- Test_valid %>% 
  spread(key = day, value = sales)

write_csv(Test_valid, path = "Test_valid.csv")



# Test_evaluation - from 2015-05-23 to 2015-06-19

library(lubridate)

eval_days <- calendar %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date >= ymd("2015-05-23") & date <= ymd("2015-06-19")) %>% 
  pull(d)


Test_eval <- sales_train_evaluation %>% 
  gather(-id, -item_id, -dept_id, -cat_id, -store_id, -state_id, key = day, value = sales) %>% 
  filter(day %in% eval_days)

Test_eval <- Test_eval %>% 
  spread(key = day, value = sales)

test_eval_days <- paste("d", c(1942:1969), sep = "_")

colnames(Test_eval)[-(1:6)] <- test_eval_days
write_csv(Test_eval, path = "Test_eval.csv")


Forecasts <- read_csv("~/Desktop/M5Comp/Reconciliation/Forecasts/m5_clustering_70_normalized_with_external_vars_pooled_regression_400_forecasts.csv")

Forecasts <- Forecasts %>% 
  slice(1:30490)

write_csv(Forecasts, path = "Forecasts.csv")







weights <- read_csv("weights_all_validation.csv")

forecasts <- Forecasts










############################################
        ####--Score function--####
############################################

Diff_func <- function(x){
  x <- na.omit(x)
  d <- diff(x, lag = 1)
  return((t(d)%*%d)/(length(x)-1))
}

X <- apply(Train_all %>% select(-name), 1, Diff_func)


WRMSSE_func <- function(forecasts, actual, training_set, weights){
  
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
    left_join(Actual_all)
  
  Summary_mse <- Fc_all %>% 
    mutate(SE = (sales - sales_fc)^2) %>% 
    group_by(name) %>% 
    summarise(MSE = mean(SE))
  
  Summary_mse <- RMSSE_denom %>% 
    left_join(Summary_mse)
  
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
