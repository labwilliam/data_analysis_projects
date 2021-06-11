# Cleaning data

# List all the CSV files
file_names <- list.files(path = "../datasets/original_datasets/",
                         pattern = "*.csv", full.names = FALSE,
                         recursive = FALSE)

# Clean all CSV files
for(name in file_names){
  # Read CSV file
  data <- read_csv(paste("../datasets/original_datasets/", name,
                         sep = ""))[, c(2:8,13)]

  # Drop rows with missing values
  data <- drop_na(data)
  
  # Drop duplicates
  data <- distinct(data)
  
  # Calculate the ride length
  ride_length <- difftime(data$ended_at, data$started_at, units = "secs")
  
  # Calculate the day of week (1 = Sunday, 7 = Saturday)
  day_of_week <- wday(data$started_at, week_start = 7)
  
  # Add ride_length and day_of_week columns to the data
  data <- add_column(data, ride_length, day_of_week, .after = "ended_at")
  
  # Filter the ride_length to be at least 5 min and at most 24 hours
  data <- filter(data, 300 <= abs(data$ride_length) &
                   abs(data$ride_length) <= 86400)
  
  # Find the index of ride_length, started_at, and ended_at columns
  index_ride <- grep("ride_length", colnames(data))
  index_started <- grep("started_at", colnames(data))
  index_ended <- grep("ended_at", colnames(data))
  
  # If the ride_length is negative, change the started_at and ended_at
  # columns and make ride_length positive
  for(row_number in 1:nrow(data)){
    if(data[row_number, index_ride] < 0){
      tmp <- data[row_number, index_started]
      data[row_number, index_started] <- data[row_number, index_ended]
      data[row_number, index_ended] <- tmp
      data[row_number, index_ride] = -1 * data[row_number, index_ride]
    }
  }
  
  # Rename member_casual column to membership
  data <- rename(data, membership = member_casual)
  
  # Saves data as a CSV file
  write_csv(data, paste("../datasets/cleaned_datasets/", name, sep = ""))
}