BASE_DIR <- "C:/Projects/M5Comp/"

forecasts_path <- "results/combined_forecasts/intermittent_sbj_non_intermittent_rnn_forecasts.txt"
output_path <-  "results/submissions/intermittent_sbj_non_intermittent_rnn_forecasts.csv" 


results <- read.csv(paste0(BASE_DIR, forecasts_path), header=FALSE)
sales_dataset <- read.csv(paste0(BASE_DIR,"dataset/comp_data/comp_data/sales_train_validation.csv"))

validation_results <- results[,c(1:28)]
evaluation_results <- results[,c(29:56)]

validation_results$id = paste0(sales_dataset$item_id,"_",sales_dataset$store_id,"_validation")
evaluation_results$id =  paste0(sales_dataset$item_id,"_",sales_dataset$store_id,"_evaluation")

final_validation_results <- validation_results[,c(29,1:28)]
final_evaluation_results <- evaluation_results[,c(29,1:28)]

names <- c("id", sprintf("F%s",seq(1:28)))
colnames(final_validation_results) <- names
colnames(final_evaluation_results) <- names

final <- rbind(final_validation_results, final_evaluation_results)
write.csv(final, paste0(BASE_DIR, output_path), row.names = FALSE)
