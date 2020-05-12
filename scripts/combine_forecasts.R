library(forecast)
library(tsintermittent)

BASE_DIR <- "C:/Projects/M5Comp/"
intermittent_forecasts_path <- "results/intermittent_forecasts/croston_forecasts/m5_intermittent_sbj.txt"
non_intermittent_forecasts_path <- "results/non_intermittent_forecasts/nstl_m5_non_intermittent_series_chopped_global.txt"
output_path <- "results/combined_forecasts/intermittent_sbj_non_intermittent_rnn_forecasts.txt"


# read the data
file <- paste0(BASE_DIR, "dataset/preprocessed_data/original_m5_dataset.txt")
dataset <- read.csv(file, header = FALSE)

non_intermittent_series <-idclass(t(dataset), type="SBC")$idx.croston

intermittent_forecasts <- read.csv(paste0(BASE_DIR, intermittent_forecasts_path), header=FALSE)
rnn_forecasts <- read.csv(paste0(BASE_DIR, non_intermittent_forecasts_path), header=FALSE)

forecasts <- intermittent_forecasts

insertRow <- function(existingDF, newrow, index) {
  existingDF[seq(index+1,nrow(existingDF)+1),] <- existingDF[seq(index,nrow(existingDF)),]
  existingDF[index,] <- newrow
  existingDF
}

for(i in 1:nrow(rnn_forecasts)){
  forecasts <- insertRow(forecasts, as.numeric(rnn_forecasts[i,]), non_intermittent_series[i])
}

write.table(forecasts, paste0(BASE_DIR, output_path), row.names = F, col.names = F, sep=",")
