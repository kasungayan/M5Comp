library(forecast)
library(tsintermittent)

BASE_DIR <- "/home/rakshitha/time-series-forecasting/"

# read the data
file <- paste0(BASE_DIR, "datasets/text_data/M5/original_m5_dataset.txt")
dataset <- read.csv(file, header = FALSE)
forecast_horizon <- 56 # validation + evaluation

output_file_name_1 = paste0(BASE_DIR, "results/M5/m5_intermittent_croston.txt")
output_file_name_2 = paste0(BASE_DIR, "results/M5/m5_intermittent_sba.txt")
output_file_name_3 = paste0(BASE_DIR, "results/M5/m5_intermittent_sbj.txt")
unlink(output_file_name_1)
unlink(output_file_name_2)
unlink(output_file_name_3)

intermittent_series <-idclass(t(dataset), type="SBC")$idx.sba

for (i in 1:nrow(dataset)) { 
  if(i%%10==0){
    print(i)
  }

  if(i %in% intermittent_series){
    time_series = as.numeric(dataset[i,])
    
    forecasts_1 = crost(time_series, type="croston", h=forecast_horizon)$frc.out
    forecasts_2 = crost(time_series, type="sba", h=forecast_horizon)$frc.out
    forecasts_3 = crost(time_series, type="sbj", h=forecast_horizon)$frc.out
    
    forecasts_1[forecasts_1<0] <- 0
    forecasts_2[forecasts_2<0] <- 0
    forecasts_3[forecasts_3<0] <- 0
    #forecasts <- round(forecasts)
    
    write.table(t(forecasts_1), file = output_file_name_1, row.names = F, col.names = F, sep = ",", quote = F, append = TRUE)
    write.table(t(forecasts_2), file = output_file_name_2, row.names = F, col.names = F, sep = ",", quote = F, append = TRUE)
    write.table(t(forecasts_3), file = output_file_name_3, row.names = F, col.names = F, sep = ",", quote = F, append = TRUE)
  } 
}
