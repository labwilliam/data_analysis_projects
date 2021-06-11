# Create mode function
mode_func <- function(data){
  unique_data <- unique(data)
  tabulate_data <- tabulate(match(data, unique_data))
  unique_data[tabulate_data == max(tabulate_data)]
}

# Create save function
save_file <- function(data, file_name){
  write_csv(data, paste("../datasets/analyzed_datasets/", file_name, ".csv",
                        sep = ""))
}

# Summarizing data

# Read the aggregated data
data <- read_csv("../datasets/analyzed_datasets/aggregated_data.csv",
                 col_types = cols(start_station_id = col_character(),
                                  end_station_id = col_character()))

# Identify the top 10 stations that members start a ride
member <- filter(data, membership == "member")
member <- count(member, start_station_id, name = "frequency", sort = TRUE)
member$membership = "member"
top_member <- head(member, 10)

# Identify the top 10 stations that casuals start a ride
casual <- filter(data, membership == "casual")
casual <- count(casual, start_station_id, name = "frequency", sort = TRUE)
casual$membership = "casual"
top_casual <- head(casual, 10)

# Complement the top stations of members with the casual
top_member_complete <- inner_join(select(top_member, "start_station_id"),
                                  unique(casual), by = "start_station_id")
top_member_complete <- union(top_member, top_member_complete)

# Complement the top stations of casual with the members
top_casual_complete <- inner_join(select(top_casual, "start_station_id"),
                                  unique(member), by = "start_station_id")
top_casual_complete <- union(top_casual, top_casual_complete)

# List the stations names
stations_names <- unique(select(data, "start_station_id", "start_station_name"))

# Add the stations names to top stations of members
top_member_complete <- inner_join(top_member_complete, stations_names,
                                  by = "start_station_id")
top_member_complete <- top_member_complete[, c(1, 4, 2, 3)]

# Add the stations names to top stations of casuals
top_casual_complete <- inner_join(top_casual_complete, stations_names,
                                  by = "start_station_id")
top_casual_complete <- top_casual_complete[, c(1, 4, 2, 3)]

# Save the frequency of top stations of members data as a CSV file
save_file(top_member_complete, "top_member")

# Save the frequency of top stations of casuals data as a CSV file
save_file(top_casual_complete, "top_casual")

# Group data by member relationship
data <- group_by(data, membership)

# Summarize the data
summarized_data <- summarize(data, min_ride_length = min(ride_length),
                             mean_ride_length = mean(ride_length),
                             max_ride_length = max(ride_length),
                             mode_day_week = mode_func(day_of_week))
frequency_rides <- count(data, membership, name = "frequency_rides")
summarized_data <- inner_join(summarized_data, frequency_rides,
                              by = "membership")
summarized_data <- summarized_data[, c(2, 3, 4, 5, 6, 1)]
save_file(summarized_data, "summarized_data")

# Frequency of rides by day
frequency_week <- count(data, day_of_week, name = "frequency")
frequency_week$day_of_week <- recode(frequency_week$day_of_week, "1" = "Sunday",
                                     "2" = "Monday", "3" = "Tuesday",
                                     "4" = "Wednesday", "5" = "Thursday",
                                     "6" = "Friday", "7" = "Saturday")
frequency_week <- rename(frequency_week, weekday = day_of_week)
frequency_week <- frequency_week[, c(2, 3, 1)]

# Save the frequency of week data as a CSV file
save_file(frequency_week, "frequency_week")

# Frequency of rideables
frequency_rideables <- count(data, rideable_type, name = "frequency")
frequency_rideables$rideable_type <- str_replace(frequency_rideables$rideable_type, "_", " ")
frequency_rideables <- frequency_rideables[, c(2, 3, 1)]

# Save the frequency of rideables data as a CSV file
save_file(frequency_rideables, "frequency_rideables")

# Splitting datetime to date and time
tmp <- as_date(data$started_at)
#data$year <- format(tmp, "%Y")
#data$month <- format(tmp, "%m")
data$month_year <- format(tmp, "%m/%Y")
#data$day <- format(tmp, "%d")
#data$time <- format(data$started_at, format = "%H:%M:%S")
data$hour <- as.numeric(format(data$started_at, format = "%H"))

# Frequency of rides by month
frequency_rides_month <- count(data, month_year, name = "frequency")
frequency_rides_month <- frequency_rides_month[, c(2, 3, 1)]

# Save the frequency of rides by month data as a CSV file
save_file(frequency_rides_month, "frequency_rides_months")

# Frequency of rides by period of the day
period_time <- hour(hm("00:00", "06:00", "12:00", "18:00", "23:59"))
period_name <- c("Night", "Morning", "Afternoon", "Evening")
data$period <- cut(x = data$hour, breaks = period_time,
                       labels = period_name, include.lowest = TRUE)
frequency_period <- count(data, period, name = "frequency")
frequency_period <- frequency_period[, c(2, 3, 1)]

# Save the frequency of period of day data as a CSV file
save_file(frequency_period, "frequency_rides_period")