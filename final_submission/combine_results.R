# Combining forecasts and creating the final submission

BASE_DIR <- "M5Comp"
FORECAST_DIR <- file.path(BASE_DIR, "results", "final_submission_results")

pr_forecasts <- read.csv(file.path(FORECAST_DIR, "m5_final_pooled_regression_400_forecasts.txt"), header = F)
lgb_forecasts <- read.csv(file.path(FORECAST_DIR, "m5_final_lightgbm_400_forecasts.txt"), header = F)

# Take the average of sub-model forecasts
evaluation_results <- (pr_forecasts + lgb_forecasts)/2

sales_dataset <- read.csv(file.path(BASE_DIR, "dataset", "comp_data", "comp_data", "sales_train_evaluation.csv"))
validation_results <- sales_dataset[,(ncol(sales_dataset)-27):ncol(sales_dataset)]

validation_results$id = paste0(sales_dataset$item_id,"_",sales_dataset$store_id,"_validation")
evaluation_results$id =  paste0(sales_dataset$item_id,"_",sales_dataset$store_id,"_evaluation")

final_validation_results <- validation_results[,c(29,1:28)]
final_evaluation_results <- evaluation_results[,c(29,1:28)]

names <- c("id", sprintf("F%s",seq(1:28)))
colnames(final_validation_results) <- names
colnames(final_evaluation_results) <- names

final <- rbind(final_validation_results, final_evaluation_results)

write.csv(final, file.path(FORECAST_DIR, "final_ensemble_lightgbm_pooled_regression_400.csv"), row.names = FALSE)