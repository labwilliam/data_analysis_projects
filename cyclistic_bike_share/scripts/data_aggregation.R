# Aggregating data

# List all the CSV files
file_names <- list.files(path = "../datasets/cleaned_datasets/",
                         pattern = "*.csv", full.names = FALSE,
                         recursive = FALSE)

# Save the first file and header
aggregated_data <- read_csv(paste("../datasets/cleaned_datasets/",
                                  file_names[1], sep = ""))
write_csv(aggregated_data, "../datasets/analyzed_datasets/aggregated_data.csv")

# Aggregate all CSV files
for(name in file_names[2:length(file_names)]){
  # Read CSV file
  data <- read_csv(paste("../datasets/cleaned_datasets/", name, sep = ""))
  
  # Aggregating datasets
  #aggregated_data <- union_all(aggregated_data, data)
  
  # Drop duplicates
  #aggregated_data <- distinct(aggregated_data)
  
  # Save aggregated data as a CSV file
  write_csv(data, "../datasets/analyzed_datasets/aggregated_data.csv",
            append = TRUE)
}