# Sorting into 16 equiangular sections, where X=[0,22,45,67,90,112,135,157,180,202,225,247,270,292,315,337] (e.g., REI_V0).

# Calculating REI_VX: the average of the highest 10% wind speeds that were allocated to the 16 equiangular sectors centered on the angles listed above.
# Calculating REI_PCTX: 16 percent values (which sum to 1 when added together) correspond to the proportion of the highest 10% wind speeds which are centered on the main sector direction X listed above.

library(dplyr)

data <- read.csv("balticsea_extremewinds3.csv", header=TRUE, stringsAsFactors=FALSE)
head(data)

# Create a new data frame data2 with one row for each unique lat and lon combination
unique_coords <- unique(data[, c("lat", "lon")])
data2 <- data.frame(lat = numeric(nrow(unique_coords)),
                    lon = numeric(nrow(unique_coords)))

# Define the bin boundaries
bins <- seq(0, 360, by = 22.5)

# Iterate over the unique lat and lon combinations
for (i in 1:nrow(unique_coords)) {
  lat <- unique_coords[i, "lat"]
  lon <- unique_coords[i, "lon"]
  
  # Subset the data for the current lat and lon
  subset_data <- data[data$lat == lat & data$lon == lon, ]
  
  # Initialize a list to store the average wind speeds for each bin
  avg_speeds <- list()
  
  # Iterate over the bins
  for (j in 1:length(bins)) {
    bin_lower <- bins[j]
    bin_upper <- ifelse(j == length(bins), bins[1], bins[j+1])  # Handle last bin wrapping around
    bin_name <- paste("REI_V", bin_lower, sep = "")
    
    # Subset the data within the bin boundaries
    bin_data <- subset_data[subset_data$wind_to_dir >= bin_lower & subset_data$wind_to_dir < bin_upper, ]
    
    # Calculate the average wind_speed for the bin
    avg_speed <- mean(bin_data$wind_speed, na.rm = TRUE)
    
    # Store the average speed in the list
    avg_speeds[[bin_name]] <- avg_speed
  }
  
  # Calculate the sum of average speeds
  sum_avg_speeds <- sum(unlist(avg_speeds), na.rm = TRUE)
  
  # Iterate over the bins again to calculate the percent values
  for (j in 1:length(bins)) {
    bin_lower <- bins[j]
    bin_upper <- ifelse(j == length(bins), bins[1], bins[j+1])  # Handle last bin wrapping around
    bin_name <- paste("REI_V", bin_lower, sep = "")
    pct_name <- paste("REI_PCT", bin_lower, sep = "")
    
    # Get the average speed for the bin
    avg_speed <- avg_speeds[[bin_name]]
    
    # Calculate the percent value
    pct <- avg_speed / sum_avg_speeds
    
    # Add the percent value to data2
    data2[i, pct_name] <- pct
  }
  
  # Add the lat and lon to data2
  data2[i, "lat"] <- lat
  data2[i, "lon"] <- lon
}

head(data2)

# data2[is.nan(data2)] <- 0


data3 <- data2 %>% select(-REI_PCT360)
head(data3)
names(data3) <- c("lat", "lon", "REI_PCT0", "REI_PCT22", "REI_PCT45", "REI_PCT67", "REI_PCT90", "REI_PCT112",
                  "REI_PCT135", "REI_PCT157", "REI_PCT180", "REI_PCT202", "REI_PCT225", "REI_PCT247",
                  "REI_PCT270", "REI_PCT292", "REI_PCT315", "REI_PCT337")

data3 <- lapply(data3, function(x) replace(x, is.nan(x), 0))

my_dataframe2 <- do.call(data.frame, data3)
head(my_dataframe2)

wind_speed_pcent <- left_join(my_dataframe, my_dataframe2, by = c("lat", "lon"))
head(wind_speed_pcent)


# Calculating V10PCT_X: the average of the highest 10% wind speeds that are centered on the main sector direction X.

data4 <- data.frame(lat = numeric(nrow(unique_coords)),
                    lon = numeric(nrow(unique_coords)))
head(data4)
print(unique_coords)

# Define the bin boundaries
bins <- seq(0, 360, by = 22.5)

# Iterate over the unique lat and lon combinations
for (i in 1:nrow(unique_coords)) {
  lat <- unique_coords[i, "lat"]
  lon <- unique_coords[i, "lon"]
  
  # Subset the data for the current lat and lon
  subset_data <- data[data$lat == lat & data$lon == lon, ]
  
  # Initialize a list to store the average wind speeds for each bin
  avg_speeds <- list()
  
  # Iterate over the bins
  for (j in 1:length(bins)) {
    bin_lower <- bins[j]
    bin_upper <- ifelse(j == length(bins), bins[1], bins[j+1])  # Handle last bin wrapping around
    bin_name <- paste("V10PCT_", bin_lower, sep = "")
    
    # Calculate the bin center and the narrower slice boundaries
    bin_center <- (bin_lower + bin_upper) / 2
    slice_lower <- bin_center - 5
    slice_upper <- bin_center + 5
    
    # Subset the data within the narrower slice boundaries
    slice_data <- subset_data[subset_data$wind_to_dir >= slice_lower & subset_data$wind_to_dir < slice_upper, ]
    
    # Calculate the average wind speed for the narrower slice
    avg_speed <- mean(slice_data$wind_speed, na.rm = TRUE)
    
    # Store the average speed in the list
    avg_speeds[[bin_name]] <- avg_speed
  }
  
  # Add the lat, lon, and average speeds to data5
  data4[i, "lat"] <- lat
  data4[i, "lon"] <- lon
  data4[i, names(avg_speeds)] <- unlist(avg_speeds)
}

data5 <- data4 %>% select(-V10PCT_360)
names(data5) <- c("lat", "lon", "V10PCT_0", "V10PCT_22", "V10PCT_45", "V10PCT_67", "V10PCT_90", "V10PCT_112",
                  "V10PCT_135", "V10PCT_157", "V10PCT_180", "V10PCT_202", "V10PCT_225", "V10PCT_247",
                  "V10PCT_270", "V10PCT_292", "V10PCT_315", "V10PCT_337")
data5 <- lapply(data5, function(x) replace(x, is.nan(x), 0))
str(data5)

v10_dataframe <- do.call(data.frame, data5)
head(v10_dataframe)

wind_data <- left_join(v10_dataframe, wind_speed_pcent, by = c("lat", "lon"))
head(wind_data)

write.csv(wind_data, "wind_data.csv", row.names = FALSE)
